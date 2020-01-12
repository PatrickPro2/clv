
attributes <- list("产品SKU" = "item.name", "产品数量" = "item.number", "产品原价" = "item.original.price", "产品摊销价" = "item.amortized.price", "商品ID" = "item.name.id")
all.attributes <- append(list("首单周期" = "first.txn.time"), attributes)
order.detail.table <- fread('../data/demo_order_detail.csv')
colnames(order.detail.table) <- gsub("_", ".", colnames(order.detail.table))
order.detail.table <- order.detail.table[, c("customer.id", "txn.id", "txn.time", "txn.amount", unlist(attributes)), with=FALSE]
order.detail.table <- order.detail.table[order(customer.id, txn.time, txn.id)]
order.detail.table$txn.time <- as.POSIXct(order.detail.table$txn.time)
order.detail.table$txn.date <- as.Date(order.detail.table$txn.time)
order.detail.table$txn.year.month <- format(order.detail.table$txn.date, "%Y%m")

order.table <- order.detail.table[, .(txn.date=min(txn.date), customer.id=first(customer.id), txn.amount=mean(txn.amount)), by=.(txn.id)]
order.table$txn.year.month <- format(order.table$txn.date, "%Y%m")
order.table <- order.table[order(customer.id, txn.date, txn.id)]
order.table[, `:=`(sequence=seq_len(.N)), by=.(customer.id)]
retention.data <- fread("../data/demo_retention_db.csv")
retention.data <- retention.data[month != max(retention.data$month)]

getOrderTableTxn <- function(){
  last.date <- as.Date(max(order.table$txn.date))
  data <- order.table[, .(txn.id=length(txn.id), txn.amount=sum(txn.amount)), by=.(customer.id, txn.date)][
    , .(txn.id=sum(txn.id), txn.amount=sum(txn.amount), txn.time.first=min(txn.date), txn.time.recent=max(txn.date), txn.times=n_distinct(txn.date)), by=.(customer.id)
  ]
  data$atv <- data$txn.amount / data$txn.id
  data$month <- format(data$txn.time.first, "%Y%m")
  data$last.month <- format(data$txn.time.recent, "%Y%m")
  data$recency <- as.integer(difftime(data$txn.time.recent, data$txn.time.first, units="days"))
  data$total.calibration <- as.integer(difftime(last.date, data$txn.time.first, units="days"))
  return(data)
}

getFirstSecond <- function(){
  data <- order.table[, shift(.SD, 0:1, as.POSIXct("1970-01-01 00:00:00"), "lead"), by=.(customer.id), .SDcols=c("txn.date")]
  data$diff.first.second <- as.integer(difftime(data$V2, data$V1, units="days"))
  data[V2=="1970-01-01 00:00:00", diff.first.second:=-1]
  data$month <- format(data$V1, "%Y%m")
  data.first <- data[, .(month=first(month), diff.first.second=first(diff.first.second)), by=.(customer.id)]
  return(data.first)
}

getInit1stOrderCohort <- function(){
  order.info <- order.detail.table[order(txn.date), .N, by=.(customer.id, txn.date, item.name)]
  order.info$txn.date.customer.id <- paste0(order.info$txn.date, order.info$customer.id)
  order.info.first.product <- order.info[order.info[, .(txn.date=min(txn.date)), by=.(customer.id)], on=.(customer.id, txn.date)]
  order.info.first.product$txn.date.customer.id <- paste0(order.info.first.product$txn.date, order.info.first.product$customer.id)
  setnames(order.info.first.product, old=c("item.name"), new=c("first.item.name"))
  order.info.first.product$txn.date.cohort <- format(order.info.first.product$txn.date, "%Y%m")
  first.order.time.item <- order.info.first.product[, .(total.new.customer=n_distinct(customer.id)), by=.(txn.date.cohort, first.item.name)]
  first.order.group <- first.order.time.item[, .(total.new.customer=sum(total.new.customer)), by=.(first.item.name)]
  
  order.info.after.product <- order.info[!(txn.date.customer.id %in% order.info.first.product$txn.date.customer.id)][, .N, by=.(customer.id, item.name)]
  setnames(order.info.after.product, old=c("item.name"), new=c("after.item.name"))
  
  order.info.first.after.product <- order.info.after.product[, .(customer.id, after.item.name)][
    order.info.first.product[, .(customer.id, first.item.name)], on="customer.id", nomatch=0]
  
  purchase.drifting <- order.info.first.after.product[, .(total.customer=.N), by=.(first.item.name, after.item.name)]
  purchase.drifting <- first.order.group[purchase.drifting, on="first.item.name"]
  purchase.drifting$repurchase.rate <- round(purchase.drifting$total.customer / purchase.drifting$total.new.customer, 2)

  return(list(first.order.time.item = first.order.time.item, purchase.drifting = purchase.drifting))
}

order.table.describe <- getOrderTableTxn()
first.second <- getFirstSecond()
result <- getInit1stOrderCohort()
first.order.time.item <- result$first.order.time.item
purchase.drifting <- result$purchase.drifting
