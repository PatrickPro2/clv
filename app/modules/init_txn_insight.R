# module ui
initTxnInsightUI <- function(id) {
  item.name <- sort(unique(order.detail.table$item.name))
  table.name <- c("Sales", "Orders", "Customers", "Average Transaction Value", "Average Customer Value")
  customer.name <- c("All customers", "New customers", "Old customers")
  txn.year.month <- sort(unique(order.detail.table$txn.year.month))

  # set namespace via id
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(outputId = ns("totalAmount")),
      valueBoxOutput(outputId = ns("totalCustomer")),
      valueBoxOutput(outputId = ns("totalOrder"))
    ),
    fluidRow(div(
      column(width = 2, selectInput(inputId = ns("tableName"), label = "Chart Name", choices = table.name, width = "100%")),
      column(width = 2, selectInput(inputId = ns("customerCategory"), label = "Category of Customer", choices = customer.name, width = "100%")),
      column(width = 4, selectInput(inputId = ns("cohorts"), label = "Period", choices = txn.year.month, multiple = TRUE, selected = txn.year.month, width = "100%")),
      column(width = 2, radioButtons(inputId = ns("aggregation"), label = "Aggregation Method", choices = list("Sum" = "sum", "Average" = "average"), inline = TRUE)),
      column(width = 2, uiOutput(outputId = ns("reactiveAgg")))
    ), style="color: #ffffff"),
    fluidRow(column(width = 12, highchartOutput(outputId = ns("sale"), height = "400px"))),
    fluidRow(column(width = 6, highchartOutput(outputId = ns("order"), height = "400px")),
             column(width = 6, highchartOutput(outputId = ns("atv"), height = "400px"))),
    fluidRow(column(width = 6, highchartOutput(outputId = ns("customer"), height = "400px")),
             column(width = 6, highchartOutput(outputId = ns("acv"), height = "400px"))),
    fluidRow(div(
      column(width = 12, selectInput(inputId = ns("itemName"), label = "Item Name", choices = item.name, multiple = TRUE, selected = item.name, width = "100%"))
    ), style="color: #ffffff"),
    fluidRow(column(width = 12, highchartOutput(outputId = ns("dspItemPrice"), height = "400px")))
  )
}


# module server
initTxnInsight <- function(input, output, session, order.table, order.detail.table) {
  # 销售额、新客数、订单数
  output$totalAmount <- renderValueBox({
    valueBox(paste0("¥ ", format(round(sum(order.table$txn.amount)), big.mark=",")), subtitle="Total Sales", icon=icon("coins"), width=3)
  })
  output$totalCustomer <- renderValueBox({
    valueBox(paste0(format(round(n_distinct(order.table$customer.id)), big.mark=",")), subtitle="Total Customers", icon=icon("user-friends"), width=3)
  })
  output$totalOrder <- renderValueBox({
    valueBox(paste0(format(round(n_distinct(order.table$txn.id)), big.mark=",")), subtitle="Total Orders", icon=icon("copy"), width=3)
  })

  # 所有顾客、新客、老客每个周期内的交易额、订单量、顾客数
  order.new.customer <- order.table[order.table[, .(txn.year.month=min(txn.year.month)), by=.(customer.id)], on=.(customer.id, txn.year.month), nomatch=0]
  order.new.customer.groupby.year.month <- order.new.customer[order(txn.year.month), .(
    total.txn.amount.new.customer=sum(txn.amount),
    total.order.new.customer=n_distinct(txn.id),
    total.customer.new.customer=n_distinct(customer.id)
  ), by=.(txn.year.month)]
  order.table.groupby.year.month <- order.table[order(txn.year.month), .(
    total.txn.amount=sum(txn.amount),
    total.order=n_distinct(txn.id),
    total.customer=n_distinct(customer.id)
  ), by=.(txn.year.month)]
  order.table.groupby.year.month <- order.table.groupby.year.month[order.new.customer.groupby.year.month, on=.(txn.year.month)]
  order.table.groupby.year.month$total.txn.amount.old.customer <- order.table.groupby.year.month$total.txn.amount - order.table.groupby.year.month$total.txn.amount.new.customer
  order.table.groupby.year.month$total.order.old.customer <- order.table.groupby.year.month$total.order - order.table.groupby.year.month$total.order.new.customer
  order.table.groupby.year.month$total.customer.old.customer <- order.table.groupby.year.month$total.customer - order.table.groupby.year.month$total.customer.new.customer
  order.table.groupby.year.month$average.transaction.value <- round(order.table.groupby.year.month$total.txn.amount / order.table.groupby.year.month$total.order, 2)
  order.table.groupby.year.month$average.transaction.value.new.customer <- round(order.table.groupby.year.month$total.txn.amount.new.customer / order.table.groupby.year.month$total.order.new.customer, 2)
  order.table.groupby.year.month$average.transaction.value.old.customer <- round(order.table.groupby.year.month$total.txn.amount.old.customer / order.table.groupby.year.month$total.order.old.customer, 2)
  order.table.groupby.year.month$average.customer.value <- round(order.table.groupby.year.month$total.txn.amount / order.table.groupby.year.month$total.customer, 2)
  order.table.groupby.year.month$average.customer.value.new.customer <- round(order.table.groupby.year.month$total.txn.amount.new.customer / order.table.groupby.year.month$total.customer.new.customer, 2)
  order.table.groupby.year.month$average.customer.value.old.customer <- round(order.table.groupby.year.month$total.txn.amount.old.customer / order.table.groupby.year.month$total.customer.old.customer, 2)
  order.table.groupby.year.month[is.na(average.transaction.value.old.customer), average.transaction.value.old.customer:=0]
  order.table.groupby.year.month[is.na(average.customer.value.old.customer), average.customer.value.old.customer:=0]

  table.name.list <- list(
    "Sales" = "total.txn.amount", "Orders" = "total.order", "Customers" = "total.customer",
    "Average Transaction Value" = "average.transaction.value",
    "Average Customer Value" = "average.customer.value")
  customer.name.list <- list(
    "All customers" = "",
    "New customers" = ".new.customer",
    "Old customers" = ".old.customer"
  )

  output$reactiveAgg <- renderUI({
    colname <- paste0(table.name.list[[input$tableName]], customer.name.list[[input$customerCategory]])

    if (input$aggregation == "sum") {
      result <- sum(order.table.groupby.year.month[txn.year.month %in% input$cohorts][[colname]])
    } else {
      result <- mean(order.table.groupby.year.month[txn.year.month %in% input$cohorts][[colname]])
    }
    paste0("Aggregated Value: ", format(round(sum(result)), big.mark=","))
  })

  # 销售额时序图
  output$sale <- renderHighchart({
    highchart() %>%
      hc_chart(type="line") %>%
      hc_legend(enabled=TRUE) %>%
      hc_add_series(name="All customers", data=order.table.groupby.year.month$total.txn.amount, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Sales: ¥{point.y}")) %>%
      hc_add_series(name="New customers", data=order.table.groupby.year.month$total.txn.amount.new.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Sales: ¥{point.y}")) %>%
      hc_add_series(name="Old customers", data=order.table.groupby.year.month$total.txn.amount.old.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Sales: ¥{point.y}")) %>%
      hc_xAxis(categories=order.table.groupby.year.month$txn.year.month) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_title(text=list("Sales"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Sales", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 订单量时序图
  output$order <- renderHighchart({
    highchart() %>%
      hc_chart(type="line") %>%
      hc_legend(enabled=TRUE) %>%
      hc_add_series(name="All customers", data=order.table.groupby.year.month$total.order, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Number of Orders: {point.y}")) %>%
      hc_add_series(name="New customers", data=order.table.groupby.year.month$total.order.new.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Number of Orders: {point.y}")) %>%
      hc_add_series(name="Old customers", data=order.table.groupby.year.month$total.order.old.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Number of Orders: {point.y}")) %>%
      hc_xAxis(categories=order.table.groupby.year.month$txn.year.month) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_title(text=list("Orders"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Orders", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 顾客数时序图
  output$customer <- renderHighchart({
    highchart() %>%
      hc_chart(type="line") %>%
      hc_legend(enabled=TRUE) %>%
      hc_add_series(name="All customers", data=order.table.groupby.year.month$total.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Number of Customers: {point.y}")) %>%
      hc_add_series(name="New customers", data=order.table.groupby.year.month$total.customer.new.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Number of Customers: {point.y}")) %>%
      hc_add_series(name="Old customers", data=order.table.groupby.year.month$total.customer.old.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="Number of Customers: {point.y}")) %>%
      hc_xAxis(categories=order.table.groupby.year.month$txn.year.month) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_title(text=list("Customers"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Customers", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 平均每单交易额
  output$atv <- renderHighchart({
    highchart() %>%
      hc_chart(type="line") %>%
      hc_legend(enabled=TRUE) %>%
      hc_add_series(name="All customers", data=order.table.groupby.year.month$average.transaction.value, tooltip=list(headerFormat="{series.name}<br>", pointFormat="ATV: ¥{point.y}")) %>%
      hc_add_series(name="New customers", data=order.table.groupby.year.month$average.transaction.value.new.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="ATV: ¥{point.y}")) %>%
      hc_add_series(name="Old customers", data=order.table.groupby.year.month$average.transaction.value.old.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="ATV: ¥{point.y}")) %>%
      hc_xAxis(categories=order.table.groupby.year.month$txn.year.month) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_title(text=list("Average Transaction Value"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Average Transaction Value", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 平均每人交易额
  output$acv <- renderHighchart({
    highchart() %>%
      hc_chart(type="line") %>%
      hc_legend(enabled=TRUE) %>%
      hc_add_series(name="All customers", data=order.table.groupby.year.month$average.customer.value, tooltip=list(headerFormat="{series.name}<br>", pointFormat="ACV: ¥{point.y}")) %>%
      hc_add_series(name="New customers", data=order.table.groupby.year.month$average.customer.value.new.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="ACV: ¥{point.y}")) %>%
      hc_add_series(name="Old customers", data=order.table.groupby.year.month$average.customer.value.old.customer, tooltip=list(headerFormat="{series.name}<br>", pointFormat="ACV: ¥{point.y}")) %>%
      hc_xAxis(categories=order.table.groupby.year.month$txn.year.month) %>%
      hc_yAxis(title=list(text="")) %>%
      hc_title(text=list("Average Customer Value"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Average Customer Value", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme)
  })

  # 商品价格描述统计
  output$dspItemPrice <- renderHighchart({
    item.price.description <- order.detail.table[item.name %in% input$itemName, .(
      "25th"=quantile(item.amortized.price, 0.25),
      "50th"=quantile(item.amortized.price, 0.5),
      "75th"=quantile(item.amortized.price, 0.75),
      "AVERAGE"=mean(item.amortized.price)
    ), by=.(item.name)]
    item.price.description <- item.price.description[order(item.name)]
    highchart() %>%
      hc_chart(type="column") %>%
      hc_legend(enabled=TRUE) %>%
      hc_add_series(name="25th", data=item.price.description$`25th`, tooltip=list(headerFormat="{point.x}<br>", pointFormat="25th: ¥{point.y:.2f}")) %>%
      hc_add_series(name="50th", data=item.price.description$`50th`, tooltip=list(headerFormat="{point.x}<br>", pointFormat="50th: ¥{point.y:.2f}")) %>%
      hc_add_series(name="Avg", data=item.price.description$AVERAGE, tooltip=list(headerFormat="{point.x}<br>", pointFormat="Avg: ¥{point.y:.2f}")) %>%
      hc_add_series(name="75th", data=item.price.description$`75th`, tooltip=list(headerFormat="{point.x}<br>", pointFormat="75th: ¥{point.y:.2f}")) %>%
      hc_xAxis(categories=item.price.description$item.name) %>%
      hc_title(text=list("Item Price Description Analysis"), style=list(color="#ffffff")) %>%
      hc_exporting(enabled=TRUE, filename="Item Price Description Analysis", buttons=list(contextButton=list(menuItems=c("downloadPNG", "downloadCSV")))) %>%
      hc_add_theme(customized.theme) %>%
      hc_colors(brewer.pal(dim(item.price.description)[1], "Blues"))
  })
}
