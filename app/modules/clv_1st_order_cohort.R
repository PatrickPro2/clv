# module ui
clv1stOrderCohortUI <- function(id) {
  item.name <- sort(unique(order.detail.table$item.name))
  
  # set namespace via id
  ns <- NS(id)
  tagList(
    fluidRow(div(
      column(width = 9, selectInput(inputId = ns("itemName"), label = "Item Name", choices = item.name, multiple = TRUE, selected = item.name, width = "100%")),
      column(width = 3, radioButtons(inputId = ns("hideLastThreeMonth"), label = "Hide the Last Three Months / Not Hide the Last Three Months", choices = list("Yes" = 1, "No" = 0), inline = TRUE))
    ), style = "color: #ffffff"),
    fluidRow(div(
      column(width = 3, actionButton(inputId=ns("run"), label = "RUN", icon = icon("paper-plane"), style="color: #ffffff; background-color: #1976d2"))
    ), style = "color: #ffffff"),
    fluidRow(
      column(width = 4, uiOutput(outputId=ns("reactiveHelpText")))
    ),
    fluidRow(column(width = 12, highchartOutput(outputId=ns("firstOrderDetailSumCLV"), height="500px"))),
    fluidRow(column(width = 12, highchartOutput(outputId=ns("firstOrderDetailMeanCLV"), height="500px")))
  )
}


# module server
clv1stOrderCohort <- function(input, output, session, clv.model.fitting.output, order.detail.table) {

  first.order.detail.clv <- reactiveValues(sum=NULL, mean=NULL)
  
  observeEvent(input$run, {
    if (is.null(clv.model.fitting.output$clv.output.model())) {
      output$reactiveHelpText <- renderUI({
        div(helpText("Please run the model in module Model Fitting, and then this module will be activated."), style="color: #ffffff")
      })
      return()
    }
    detail <- order.detail.table
    clv.selected <- clv.model.fitting.output$clv.output.model()
    detail <- order.detail.table[txn.id %in% clv.model.fitting.output$select.order.table()$txn.id]
    clv.selected$first.txn.date <- as.Date(clv.selected$first.txn.date)
    detail$txn.date <- as.Date(detail$txn.date)
    detail <- detail[, .N, by=.(customer.id, txn.date, item.name)]
    select.first.order.detail <- detail[detail[, .(txn.date=min(txn.date)), by=.(customer.id)], on=c("customer.id", "txn.date")]
    setnames(select.first.order.detail, old=c("txn.date"), new=c("first.txn.date"))
    select.first.order.detail.clv <- select.first.order.detail[clv.selected, on=c("customer.id", "first.txn.date")]
    first.order.detail.clv$sum <- select.first.order.detail.clv[order(clv.by.attribute), .(clv=round(sum(clv),0)), by=.(clv.by.attribute, item.name)][item.name %in% input$itemName]
    first.order.detail.clv$mean <- select.first.order.detail.clv[order(clv.by.attribute), .(clv=round(mean(clv),0)), by=.(clv.by.attribute, item.name)][item.name %in% input$itemName]
    if (as.numeric(input$hideLastThreeMonth) == 1) {
      if (n_distinct(select.first.order.detail.clv$clv.by.attribute) > 3) {
        selected.cohort <- unique(first.order.detail.clv$sum$clv.by.attribute)[1:(n_distinct(first.order.detail.clv$sum$clv.by.attribute)-3)]
        first.order.detail.clv$sum <- first.order.detail.clv$sum[clv.by.attribute %in% selected.cohort]
        first.order.detail.clv$mean <- first.order.detail.clv$mean[clv.by.attribute %in% selected.cohort]
      }
    }
  })

  # 不同首单月不同商品总CLV
  output$firstOrderDetailSumCLV <- renderHighchart({
    if (is.null(first.order.detail.clv$sum)) return()
    
    hchart(first.order.detail.clv$sum, "column", hcaes(x=clv.by.attribute, y=clv, group=item.name)) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text=""), categories=sort(unique(first.order.detail.clv$sum$clv.by.attribute))) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="Sum: {point.y}") %>%
      hc_title(text=list("Total Customer Lifetime Value for Items in the first Transaction"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Total Customer Lifetime Value for Items in the first Transaction", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 不同首单月不同商品平均CLV
  output$firstOrderDetailMeanCLV <- renderHighchart({
    if (is.null(first.order.detail.clv$mean)) return()
    
    hchart(first.order.detail.clv$mean, "column", hcaes(x=clv.by.attribute, y=clv, group=item.name)) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text=""), categories=sort(unique(first.order.detail.clv$mean$clv.by.attribute))) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="Average: {point.y}") %>%
      hc_title(text=list("Average Customer Lifetime Value for Items in the first Transaction"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Average Customer Lifetime Value for Items in the first Transaction", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })
}