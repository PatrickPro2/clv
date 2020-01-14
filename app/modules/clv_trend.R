# module ui
clvTrendUI <- function(id) {
  table.name <- c("Total CLV in First Transaction Months", "Average CLV in First Transaction Months")

  # set namespace via id
  ns <- NS(id)
  tagList(
    fluidRow(div(
      column(width = 3, radioButtons(inputId = ns("quantileMinSelector"), label = "Minimum Percentile", choices = list("0%" = 0, "1%" = 0.01, "5%" = 0.05), inline = TRUE)),
      column(width = 3, radioButtons(inputId = ns("quantileMaxSelector"), label = "Maximum Percentile", choices = list("100%" = 1, "99%" = 0.99, "95%" = 0.95), inline = TRUE)),
      column(width = 3, radioButtons(inputId = ns("hideLastThreeMonth"), label = "Hide the Last Three Months / Not Hide the Last Three Months", choices = list("Yes" = 1, "No" = 0), inline = TRUE)),
      column(width = 3, actionButton(inputId=ns("run"), label = "RUN", icon = icon("paper-plane"), style="color: #ffffff; background-color: #1976d2"))
    ), style = "color: #ffffff"),
    fluidRow(
      column(width = 4, uiOutput(outputId=ns("reactiveHelpText")))
    ),
    fluidRow(
      column(width = 6, highchartOutput(outputId = ns("clvDist"), height = "400px")),
      column(width = 6, highchartOutput(outputId = ns("predictPrecisionCLV"), height = "400px"))
    ),
    fluidRow(div(
      column(width = 2, selectInput(inputId = ns("tableName"), label = "Chart Name", choices = table.name, width = "100%")),
      column(width = 4, uiOutput(outputId=ns("reactiveCohorts"))),
      column(width = 2, radioButtons(inputId = ns("aggregation"), label = "Aggregation Method", choices = list("Sum" = "sum", "Average" = "average"), inline = TRUE)),
      column(width = 2, uiOutput(outputId = ns("reactiveAgg")))
    ), style="color: #ffffff"),
    fluidRow(
      column(width = 6, highchartOutput(outputId = ns("attributeSumCLV"), height = "400px")),
      column(width = 6, highchartOutput(outputId = ns("attributeMeanCLV"), height = "400px"))
    ),
    fluidRow(
      column(width = 6, highchartOutput(outputId = ns("cohortSalesCLV"), height = "400px")),
      column(width = 6, highchartOutput(outputId = ns("cohortSumCustomerAverageCLV"), height = "400px"))
    )
  )
}


# module server
clvTrend <- function(input, output, session, clv.model.fitting.output) {
  clv.output.model <- reactiveValues(value=NULL)
  select.clv.cohort <- reactiveValues(value=NULL)
  select.total.txn.amount.cohort <- reactiveValues(value=NULL)
  table.name.list <- list("Total CLV in First Transaction Months" = "sum.clv", "Average CLV in First Transaction Months" = "avg.clv")

  observeEvent(input$run, {
    if (is.null(clv.model.fitting.output$clv.output.model())) {
      output$reactiveHelpText <- renderUI({
        div(helpText("Please run the model in module Model Fitting, and then this module will be activated."), style="color: #ffffff")
      })
      return()
    }

    select.order.table.output <- clv.model.fitting.output$select.order.table()
    select.total.txn.amount.cohort$value <- select.order.table.output[
      select.order.table.output[, .(txn.year.month=min(txn.year.month)), by=.(customer.id)], on=.(customer.id, txn.year.month)
    ][order(txn.year.month), .(total.txn.amount=sum(txn.amount)), by=.(txn.year.month)]

    if (as.numeric(input$hideLastThreeMonth) == 1) {
      if (n_distinct(clv.model.fitting.output$clv.output.model()$clv.by.attribute) > 3) {
        selected.cohort <- unique(clv.model.fitting.output$clv.output.model()$clv.by.attribute)[1:(n_distinct(clv.model.fitting.output$clv.output.model()$clv.by.attribute)-3)]
        selected.txn.year.month <- unique(clv.model.fitting.output$select.order.table()$txn.year.month)[1:(n_distinct(clv.model.fitting.output$select.order.table()$txn.year.month)-3)]
      } else {
        selected.cohort <- unique(clv.model.fitting.output$clv.output.model()$clv.by.attribute)
        selected.cohort <- unique(clv.model.fitting.output$clv.output.model()$clv.by.attribute)
      }
    } else {
      selected.cohort <- unique(clv.model.fitting.output$clv.output.model()$clv.by.attribute)
      selected.txn.year.month <- unique(clv.model.fitting.output$select.order.table()$txn.year.month)
    }

    select.total.txn.amount.cohort$value <- select.total.txn.amount.cohort$value[txn.year.month %in% selected.txn.year.month]
    clv.output.model$value <- clv.model.fitting.output$clv.output.model()[clv.by.attribute %in% selected.cohort]
    select.clv.cohort$value <- clv.output.model$value[order(clv.by.attribute), .(sum.clv=sum(clv), avg.clv=mean(clv), customer=length(customer.id)), by=.(clv.by.attribute)]
  })

  output$reactiveCohorts <- renderUI({
    if (is.null(select.clv.cohort$value)) return()

    ns <- session$ns
    txn.year.month <- sort(unique(select.clv.cohort$value$clv.by.attribute))
    div(selectInput(inputId = ns("cohorts"), label = "Period", choices = txn.year.month, multiple = TRUE, selected = txn.year.month, width = "100%"), style="color: #ffffff")
  })
  
  output$reactiveAgg <- renderUI({
    if (is.null(select.clv.cohort$value)) return()

    colname <- table.name.list[[input$tableName]]
    if (input$aggregation == "sum") {
      result <- sum(select.clv.cohort$value[clv.by.attribute %in% input$cohorts][[colname]])
    } else {
      result <- mean(select.clv.cohort$value[clv.by.attribute %in% input$cohorts][[colname]])
    }
    paste0("Aggregated Value: ", format(round(sum(result)), big.mark=","))
  })

  # 所有训练数据样本CLV的分布
  output$clvDist <- renderHighchart({
    if (is.null(clv.output.model$value)) return()

    histogram <- clv.output.model$value$clv[
      which(clv.output.model$value$clv >= quantile(clv.output.model$value$clv, as.numeric(input$quantileMinSelector))
            & clv.output.model$value$clv <= quantile(clv.output.model$value$clv, as.numeric(input$quantileMaxSelector)))
    ]
    hchart(histogram) %>%
      hc_plotOptions(series=list(marker=list(enabled=FALSE), lineWidth=0)) %>%
      hc_legend(enabled=FALSE) %>%
      hc_xAxis(tickLength=0, title=list(text="")) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(pointFormat="Number of Customers: {point.y}") %>%
      hc_title(text=list("Customer Lifetime Value Distribution"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Customer Lifetime Value Distribution", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 所有训练数据样本CLV在未来不同预测周期区间内的累计CLV
  output$predictPrecisionCLV <- renderHighchart({
    if (is.null(clv.output.model$value)) return()

    predict.period.length <- as.integer(substring(max(colnames(clv.output.model$value)[grepl(".*\\d", colnames(clv.output.model$value))]), 4))
    predict.precision.clv <- data.table(clv=lapply(clv.output.model$value[, c(paste0("clv", 1:predict.period.length)), with=FALSE], sum))
    highchart() %>%
      hc_chart(type="column") %>%
      hc_series(list(name="CLV", type="column", zIndex=2, tooltip=list(headerFormat="CLV<br>", pointFormat="Sum: ¥ {point.y:.2f}"), data=predict.precision.clv$clv)) %>%
      hc_legend(enabled=FALSE) %>%
      hc_xAxis(title=list(text=""), categories=c(paste0("Period ", 1:predict.period.length))) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="", pointFormat="CLV: ¥ {point.x:.2f}") %>%
      hc_title(text=list("Total Customer Lifetime Value in the Future Periods"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Total Customer Lifetime Value in the Future Periods", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 分组计算标准-CLV累计图
  output$attributeSumCLV <- renderHighchart({
    if (is.null(select.clv.cohort$value)) return()

    highchart() %>%
      hc_chart(type="column") %>%
      hc_series(list(name="CLV", type="column", zIndex=2, tooltip=list(headerFormat="CLV<br>", pointFormat="Sum: ¥ {point.y:.2f}"), data=select.clv.cohort$value$sum.clv)) %>%
      hc_legend(enabled=FALSE) %>%
      hc_xAxis(title=list(text=""), categories=select.clv.cohort$value$clv.by.attribute) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_title(text=list("Total Customer Lifetime Value in the First Transaction Months"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Total Customer Lifetime Value in the First Transaction Months", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 所有训练数据样本首单attribute的人均CLV
  output$attributeMeanCLV <- renderHighchart({
    if (is.null(select.clv.cohort$value)) return()

    highchart() %>%
      hc_chart(type="column") %>%
      hc_series(list(name="CLV", type="column", zIndex=2, tooltip=list(headerFormat="CLV<br>", pointFormat="Average: ¥ {point.y:.2f}"), data=select.clv.cohort$value$avg.clv)) %>%
      hc_legend(enabled=FALSE) %>%
      hc_xAxis(title=list(text=""), categories=select.clv.cohort$value$clv.by.attribute) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_title(text=list("Average Customer Lifetime Value in the First Transaction Months"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Average Customer Lifetime Value in the First Transaction Months", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 每个首单月所有新客在当月的总销售额和总CLV
  output$cohortSalesCLV <- renderHighchart({
    if (is.null(select.total.txn.amount.cohort$value) & is.null(select.clv.cohort$value)) return()

    highchart() %>%
      hc_chart(type="line") %>%
      hc_series(
        list(name="Sales", type="line", yAxis=0, tooltip=(list(headerFormat="Sales<br>", pointFormat="Sum: ¥ {point.y:.2f}")), data=select.total.txn.amount.cohort$value$total.txn.amount),
        list(name="CLV", type="line", yAxis=1, tooltip=(list(headerFormat="CLV<br>", pointFormat="Sum: ¥ {point.y:.2f}")), data=select.clv.cohort$value$sum.clv)
      ) %>%
      hc_xAxis(title=list(text=""), categories=select.clv.cohort$value$clv.by.attribute) %>%
      hc_yAxis_multiples(list(title=list(text="")), list(title=list(text=""), opposite=TRUE)) %>%
      hc_title(text=list("Total Sales and Customer Lifetime Value in the First Transaction Months"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Total Sales and Customer Lifetime Value in the First Transaction Months", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 每个首单月所有新客的总CLV和人均CLV
  output$cohortSumCustomerAverageCLV <- renderHighchart({
    if (is.null(select.clv.cohort$value)) return()

    highchart() %>%
      hc_chart(type="line") %>%
      hc_series(
        list(name="Customers", type="line", yAxis=0, tooltip=(list(headerFormat="Number of Customers<br>", pointFormat="Sum: {point.y}")), data=select.clv.cohort$value$customer),
        list(name="CLV", type="line", yAxis=1, tooltip=(list(headerFormat="CLV<br>", pointFormat="Average: ¥ {point.y:.2f}")), data=select.clv.cohort$value$avg.clv)
      ) %>%
      hc_xAxis(title=list(text=""), categories=select.clv.cohort$value$clv.by.attribute) %>%
      hc_yAxis_multiples(list(title=list(text="")), list(title=list(text=""), opposite=TRUE)) %>%
      hc_title(text=list("Total Customers and Customer Lifetime Value in the First Transaction Months"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Total Customers and Customer Lifetime Value in the First Transaction Months", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })
}
