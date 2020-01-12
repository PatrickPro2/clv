# module ui
init1stOrderCohortUI <- function(id) {

  item.name <- sort(unique(order.detail.table$item.name))

  # set namespace via id
  ns <- NS(id)
  tagList(
    fluidRow(div(
      column(width = 12, selectInput(inputId = ns("itemName"), label = "商品名称", choices = item.name, multiple = TRUE, selected = item.name, width = "100%"))
    ), style = "color: #ffffff"),
    fluidRow(div(
      column(width = 3, actionButton(inputId=ns("run"), label = "运行", icon = icon("paper-plane"), style="color: #ffffff; background-color: #1976d2"))
    ), style = "color: #ffffff"),
    fluidRow(
      column(width = 12, highchartOutput(outputId = ns("firstByItemTime"), height = "400px"))
    ),
    fluidRow(div(
      column(width = 12, dataTableOutput(outputId = ns("productDrifting"), height = "800px"))
    ), style = "color: #ffffff")
  )
}


# module server
init1stOrderCohort <- function(input, output, session, first.order.time.item, purchase.drifting) {

  value <- reactiveValues(first.order.time.item=NULL, purchase.drifting=NULL)

  observeEvent(input$run, {
    value$first.order.time.item <- first.order.time.item
    value$purchase.drifting <- purchase.drifting
  })

  # 首单月购买不同商品顾客数
  output$firstByItemTime <- renderHighchart({
    if (is.null(value$first.order.time.item)) return()

    first.order.time.item.selected <- value$first.order.time.item[first.item.name %in% input$itemName]
    first.order.time.item.selected <- first.order.time.item.selected[order(txn.date.cohort, first.item.name)]
    hchart(first.order.time.item.selected, "column", hcaes(x=txn.date.cohort, y=total.new.customer, group=first.item.name)) %>%
      hc_legend(enabled=TRUE) %>%
      hc_xAxis(title=list(text=""), categories=unique(first.order.time.item.selected$txn.date.cohort)) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_tooltip(headerFormat="{series.name}<br>", pointFormat="新客数: {point.y}") %>%
      hc_title(text=list("首单购买商品新客数"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="首单购买商品新客数", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 首单商品复购率
  output$productDrifting <- renderDataTable({
    if (is.null(value$purchase.drifting)) return()

    purchase.drifting.dcast <- dcast(value$purchase.drifting, first.item.name ~ after.item.name, value.var = "repurchase.rate")
    purchase.drifting.dcast.selected <- purchase.drifting.dcast[first.item.name %in% input$itemName]
    setnames(purchase.drifting.dcast.selected, old=c("first.item.name"), new=c("首单商品连带复购率"))
    datatable(
      purchase.drifting.dcast.selected,
      rownames=FALSE,
      extensions=c("Buttons"),
      options=list(
        searching=FALSE, paging=FALSE, dom="Bt", buttons=c("copy", "csv"), ordering=TRUE, deferRender=TRUE,
        info=FALSE, scrollX=TRUE, scrollY=TRUE, fill=0
      )
    ) %>%
      formatStyle(c(1:length(colnames(purchase.drifting.dcast.selected))), color = "#000000") %>%
      formatStyle(c(2:length(colnames(purchase.drifting.dcast.selected))), backgroundColor = styleInterval(c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30), brewer.pal(8, "Blues")))
  })
}
