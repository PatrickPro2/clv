# module ui
initRetentionInsightUI <- function(id) {

  retention.period.selection <- setNames(
    as.list(names(retention.data)[grep("day", colnames(retention.data))]), 
    paste0(as.integer(gsub("day_", "", colnames(retention.data)[grep("day", names(retention.data))])), " days")
  )
  dimension <- setdiff(colnames(retention.data), c(colnames(retention.data)[grep("day", colnames(retention.data))], "month", "total"))
  
  # set namespace via id
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=4, div(selectInput(inputId=ns("period"), label="Retention Period", choices=retention.period.selection), style="color: #ffffff"))
    ),
    fluidRow(
      column(width=6, div(selectInput(inputId=ns("retentionIsCohort"), label="Show All First Transaction Months / Not Show All First Transaction Months", choices=c("Yes", "No"), width="100%"), style="color: #ffffff")),
      column(width=6, uiOutput(outputId=ns("reactiveRetentionCohort")))
    ),
    fluidRow(
      column(width=6, div(selectInput(inputId=ns("retetnionIsDimension"), label="Dimension", choices=append("all", dimension), selected="all", width="100%"), style="color: #ffffff")),
      column(width=6, uiOutput(outputId=ns("reactiveRetentionDimension")))
    ),
    fluidRow(column(width=12, actionButton(inputId=ns("run"), label = "RUN", icon = icon("paper-plane"), style="color: #ffffff; background-color: #1976d2"))),
    fluidRow(column(width=12, highchartOutput(outputId=ns("retentionLine"), height="500px"))),
    fluidRow(column(width=12, div(dataTableOutput(outputId=ns("retentionTable"), height="500px"), style="color: #ffffff")))
  )
}


# module server
initRetentionInsight <- function(input, output, session, retention.data) {
  
  retention <- reactiveValues(line=NULL, table=NULL)
  item.name.table <- unique(order.detail.table[order(item.name.id), .(item.name, item.name.id)])
  
  output$reactiveRetentionCohort <- renderUI({
    if (input$retentionIsCohort == "Yes") {}
    else {
      ns <- session$ns
      div(selectInput(inputId=ns("retentionCohort"), label="First Transaction Month", choices=sort(unique(retention.data$month)), multiple=TRUE, width="100%"), style="color: #ffffff")
    }
  })
  
  output$reactiveRetentionDimension <- renderUI({
    if (input$retetnionIsDimension == "all") {}
    else {
      ns <- session$ns
      item.name.list <- setNames(item.name.table$item.name.id, item.name.table$item.name)
      div(selectInput(inputId=ns("retentionDimension"), label="Value", choices=item.name.list, selected="all", width="100%", multiple=TRUE), style="color: #ffffff")
    }
  })

  observeEvent(input$run, {
    if (input$retentionIsCohort == "Yes") {
      selected.cohort <- unique(retention.data$month)
    } else {
      selected.cohort <- input$retentionCohort
    }
    if (input$retetnionIsDimension == "all") {
      selected.retention <- retention.data[month %in% selected.cohort, .(month, dimension="all", total, seq=get(input$period))]
    } else {
      selected.dimension <- input$retentionDimension
      selected.retention <- retention.data[month %in% selected.cohort, .(month, dimension=first_item_name_id, total, seq=get(input$period))]
      item <- lapply(selected.retention$dimension, function(x) {
        item = str_trim(str_sub(x, 2, str_length(x)-1))
        item = str_split(item, "\\s+")
        item = unlist(lapply(item, function(x) setNames(x, 1:length(x))))
        item
      })
      max.item.quantity <- max(unlist(lapply(item, length)))
      item.format <- data.table(sapply(data.table(do.call("rbind", lapply(item, function(x) x[match(1:max.item.quantity, names(x))]))), as.integer))
      colnames(item.format) <- as.character(1:max.item.quantity)
      selected.retention <- cbind(selected.retention, item.format)[, !"dimension"]
      selected.retention <- melt(selected.retention, id.vars = c("month", "total", "seq"), measure.vars = as.character(1:max.item.quantity), value.name="dimension", na.rm=TRUE)[, !"variable"]
      selected.retention <- selected.retention[dimension %in% selected.dimension]
      dimension.table <- item.name.table
      colnames(dimension.table) <- c("item.name", "dimension")
      selected.retention <- dimension.table[selected.retention, on="dimension"][, !"dimension"]
      setnames(selected.retention, old=c("item.name"), new=c("dimension"))
    }

    if (dim(selected.retention)[1]==0) {
      retention$line <- NULL
      retention$table <- NULL
      return()
    }

    selected.retention$day_format <- gsub("[{} ']", "", selected.retention$seq)
    selected.retention$day_format <- list(strsplit(selected.retention$day_format, ","))
    period <- lapply(selected.retention$day_format, function(x) {
      period = str_split(x, ":")
      period = unlist(lapply(period, function(x) setNames(x[2], x[1])))
      period
    })

    max.period <- as.integer(unlist(strsplit(unlist(selected.retention[1, day_format])[length(unlist(selected.retention[1, day_format]))][1],":"))[1])
    period.format <- data.table(sapply(data.table(do.call("rbind", lapply(period, function(x) x[match(0:max.period, names(x))]))), as.integer))
    output.retention <- data.table(cbind(month=selected.retention$month, selected.dimension=selected.retention$dimension, total=selected.retention$total, period.format))
    period.list <- colnames(output.retention)[which(!colnames(output.retention) %in% c("month", "selected.dimension"))]
    unmelt.retention <- output.retention[,
      lapply(.SD, sum),
      by=.(month, selected.dimension),
      .SDcols=period.list
    ]

    unmelt.retention$group <- paste0(unmelt.retention$month, "-", unmelt.retention$selected.dimension)
    melt.retention <- melt(unmelt.retention, id.vars=c("group", "total"), measure.vars = period.list[-grep("total", period.list)])
    melt.retention$rate <- round(melt.retention$value / melt.retention$total, 3)
    retention$line <- melt.retention
    retention$table <- dcast(melt.retention, group+total~variable, value.var="rate")
  })

  # 同期留存曲线
  output$retentionLine <- renderHighchart({
    if (is.null(retention$line)) return()

    hchart(retention$line, "line", hcaes(x=variable, y=rate, group=group)) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text="")) %>%
      hc_yAxis(title=list(text=(""))) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="Retention rate: {point.y}") %>%
      hc_title(text=list("Retention Curve in Cohorts (First Trnsaction Month"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Cohort Retention Rate", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 同期留存矩阵
  output$retentionTable <- renderDataTable({
    if (is.null(retention$table)) return()

    retention$table$month <- lapply(retention$table$group, function(x) unlist(strsplit(x, "-"))[1])
    retention$table$cohort <- lapply(retention$table$group, function(x) unlist(strsplit(x, "-"))[2])
    converted.table <- retention$table[, -grep("group", colnames(retention$table)), with=FALSE]
    ordered.colname <- c("month", "cohort", colnames(converted.table)[1:(length(colnames(converted.table))-2)])
    converted.table <- converted.table[, ordered.colname, with=FALSE]
    datatable(
      converted.table,
      rownames=FALSE,
      extensions=c("Buttons"),
      options=list(
        searching=FALSE, paging=FALSE, dom="Bt", buttons=c("copy", "csv"), ordering=TRUE, deferRender=TRUE,
        info=FALSE, scrollX=TRUE, scroller=TRUE, scrollY=900, fill=0)
    ) %>%
      formatStyle(c(1:length(colnames(converted.table))), color = "#000000") %>%
      formatStyle(c(4:length(colnames(converted.table))), backgroundColor = styleInterval(c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30), brewer.pal(8, "Blues"))) %>%
      formatStyle("total", background = styleColorBar(retention.data$total,"#f76262"))
  })
}
