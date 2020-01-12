# module ui
initRfmInsightUI <- function(id) {
  # set namespace via id
  ns <- NS(id)
  tagList(
    fluidRow(div(
      column(width = 12, selectInput(inputId = ns("recencyCohort"), label = "首单月", choices = sort(unique(order.table.describe$month)), multiple = TRUE, selected = min(order.table.describe$month), width = "100%"))
    ), style = "color: #ffffff"),
    fluidRow(div(
      column(width = 3, radioButtons(inputId = ns("quantileMinSelector"), label = "最小分位数", choices = list("0%" = 0, "1%" = 0.01, "5%" = 0.05), inline = TRUE)),
      column(width = 3, radioButtons(inputId = ns("quantileMaxSelector"), label = "最大分位数", choices = list("100%" = 1, "99%" = 0.99, "95%" = 0.95), inline = TRUE))
    ), style = "color: #ffffff"),
    fluidRow(
      column(width=12, actionButton(inputId=ns("run"), label = "运行", icon = icon("paper-plane"), style="color: #ffffff; background-color: #1976d2"))
    ),
    fluidRow(
      column(width = 6, highchartOutput(outputId = ns("distFirstRecent"), height = "400px")),
      column(width = 6, highchartOutput(outputId = ns("distFirstNow"), height = "400px"))
    ),
    fluidRow(
      column(width = 6, highchartOutput(outputId = ns("distFirstSecond"), height = "400px")),
      column(width = 6, highchartOutput(outputId = ns("distAtv"), height = "400px"))
    ),
    fluidRow(
      column(width = 6, highchartOutput(outputId = ns("distFrequency"), height = "400px"))
    )
  )
}


# module server
initRfmInsight <- function(input, output, session, order.table.describe, first.second.table) {
  last.date <- as.Date(max(order.table.describe$txn.time.recent))
  
  rfm <- reactiveValues(
    firstRecent=NULL,
    firstNow=NULL,
    first.second=NULL,
    atv=NULL,
    txn.first.last.customer=NULL,
    frequency=NULL
  )
  
  observeEvent(input$run, {
    rfm$firstRecent <- rbindlist(lapply(input$recencyCohort, function(x){
      selected.month.value <- order.table.describe[month == x & recency > 0, recency]
      selected.month.value <- selected.month.value[
        which(selected.month.value >= quantile(selected.month.value, as.numeric(input$quantileMinSelector))
              & selected.month.value <= quantile(selected.month.value, as.numeric(input$quantileMaxSelector)))
        ]
      histogram <- hist(selected.month.value, breaks="scott")
      histogram.table <- data.table(x=histogram$mids, y=histogram$counts, group=x)
      histogram.table
    }))

    rfm$firstNow <- rbindlist(lapply(input$recencyCohort, function(x){
      selected.month.value <- order.table.describe[month == x & total.calibration > 0, total.calibration]
      selected.month.value <- selected.month.value[
        which(selected.month.value >= quantile(selected.month.value, as.numeric(input$quantileMinSelector))
              & selected.month.value <= quantile(selected.month.value, as.numeric(input$quantileMaxSelector)))
        ]
      histogram <- hist(selected.month.value, breaks="scott")
      histogram.table <- data.table(x=histogram$mids, y=histogram$counts, group=x)
      histogram.table
    }))

    rfm$first.second <- rbindlist(lapply(input$recencyCohort, function(x){
      selected.month.value <- first.second[month == x & diff.first.second > 0, diff.first.second]
      selected.month.value <- selected.month.value[
        which(selected.month.value >= quantile(selected.month.value, as.numeric(input$quantileMinSelector))
              & selected.month.value <= quantile(selected.month.value, as.numeric(input$quantileMaxSelector)))
        ]
      histogram <- hist(selected.month.value, breaks="scott")
      histogram.table <- data.table(x=histogram$mids, y=histogram$counts, group=x)
      histogram.table
    }))

    rfm$atv <- rbindlist(lapply(input$recencyCohort, function(x){
      selected.month.value <- order.table.describe[month == x & atv > 0, atv]
      selected.month.value <- selected.month.value[
        which(selected.month.value >= quantile(selected.month.value, as.numeric(input$quantileMinSelector))
              & selected.month.value <= quantile(selected.month.value, as.numeric(input$quantileMaxSelector)))
        ]
      histogram <- hist(selected.month.value, breaks="scott")
      histogram.table <- data.table(x=histogram$mids, y=histogram$counts, group=x)
      histogram.table
    }))

    rfm$frequency <- rbindlist(lapply(input$recencyCohort, function(x){
      selected.month.order.table.describe <- order.table.describe[month == x & txn.times > 0, .(month, txn.times)]
      quantileMin <- quantile(selected.month.order.table.describe$txn.times, as.numeric(input$quantileMinSelector))
      quantileMax <- quantile(selected.month.order.table.describe$txn.times, as.numeric(input$quantileMaxSelector))
      data <- selected.month.order.table.describe[txn.times >= quantileMin & txn.times <= quantileMax, .(month, txn.times)]
      data <- data[order(month, txn.times), .N, by=.(month, txn.times)]
      data
    }))
  })

  output$distFirstRecent <- renderHighchart({
    if (is.null(rfm$firstRecent)) return()

    hchart(rfm$firstRecent, type="area", hcaes(x=x, y=y, group=group)) %>%
      hc_plotOptions(series=list(marker=list(enabled=FALSE))) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text=""), tickLength=0) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="顾客数: {point.y}") %>%
      hc_title(text=list("首单日期与末单日期天数差"), style=list(color="#ffffff")) %>%
      hc_subtitle(text=list("天数差 > 0")) %>%
      hc_exporting(enabled=TRUE, filename="首单日期与末单日期天数差", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  output$distFirstNow <- renderHighchart({
    if (is.null(rfm$firstNow)) return()

    hchart(rfm$firstNow, type="area", hcaes(x=x, y=y, group=group)) %>%
      hc_plotOptions(series=list(marker=list(enabled=FALSE))) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text=""), tickLength=0) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="顾客数: {point.y}") %>%
      hc_title(text=list("首单日期与校准日期天数差"), style=list(color="#ffffff")) %>%
      hc_subtitle(text=list(paste0("天数差 > 0, 校准日期: ", last.date))) %>%
      hc_exporting(enabled=TRUE, filename="首单日期与校准日期天数差", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  output$distFirstSecond <- renderHighchart({
    if (is.null(rfm$first.second)) return()

    hchart(rfm$first.second, type="area", hcaes(x=x, y=y, group=group)) %>%
      hc_plotOptions(series=list(marker=list(enabled=FALSE))) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text=""), tickLength=0) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="顾客数: {point.y}") %>%
      hc_title(text=list("首单日期与第二单日期天数差"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="首单日期与第二单日期天数差", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  output$distAtv <- renderHighchart({
    if (is.null(rfm$atv)) return()

    hchart(rfm$atv, type="area", hcaes(x=x, y=y, group=group)) %>%
      hc_plotOptions(series=list(marker=list(enabled=FALSE))) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text=""), tickLength=0) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="顾客数: {point.y}") %>%
      hc_title(text=list("Average Transaction Value"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Average Transaction Value", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  output$distFrequency <- renderHighchart({
    if (is.null(rfm$frequency)) return()

    hchart(rfm$frequency, type="column", hcaes(x=txn.times, y=N, group=month)) %>%
      hc_plotOptions(series=list(marker=list(enabled=FALSE))) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text=""), tickLength=0) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="顾客数: {point.y}") %>%
      hc_title(text=list("购买次数"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="购买次数", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })
}
