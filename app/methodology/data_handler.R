# SQL_HOST = Sys.getenv("SQL_HOST")
# SQL_PORT = Sys.getenv("SQL_PORT")
# SQL_DBNAME = Sys.getenv("SQL_DBNAME")
# SQL_USERNAME = Sys.getenv("SQL_USERNAME")
# SQL_PASSWORD = Sys.getenv("SQL_PASSWORD")

# drv = DBI::dbDriver("PostgreSQL")
# conn = DBI::dbConnect(drv, host=SQL_HOST, port=SQL_PORT, dbname=SQL_DBNAME, user=SQL_USERNAME, password=SQL_PASSWORD)

# fetch data from db
getData <- function(src, customer.id=c(), date.range=list(), txn.amount=list(), other=c()) {
  # Fetch raw sales data from database
  #
  # Data attributes:
  #   customer_id: string, unique customer identifiers
  #   txn_time: timestamp, when the transaction happened
  #   txn_amount: float, transaction amount / ticket size in CNY
  #   (optional): optional attributes that are exclusive to clients
  #
  # Arguments:
  #   src: string, client name
  #   customer.id: tuple, to filter transactions by customers; all customers' transactions are included if left blank
  #   date.range: list, filter transaction time by specifying "lte" (<=) and/or "gte" (>=); all-time transactions are included if left blank
  #   txn.amount: list, filter transaction ticket size by specifying "lte" (<=) and/or "gte" (>=); transactions with any amount are included if left blank
  #
  # Returns:
  #   A data.table with database relation attributes as columns

  query <- paste0("SELECT customer_id, txn_id, txn_time, txn_amount, ", paste0(str_replace(other, "\\.", "_"), collapse = ", "), " FROM clv.", src, " WHERE TRUE ")
  conditions <- c()
  
  if (length(customer.id) > 0) {
    conditions <- append(conditions, paste0("AND customer_id IN ('", paste0(customer.id, collapse = "', '"), "')"))
  }
  
  if (length(date.range) > 0) {
    if (!is.null(date.range$gte))
      conditions <- append(conditions, paste0("AND txn_time >= '", date.range$gte, "'"))
    if (!is.null(date.range$lte))
      conditions <- append(conditions, paste0("AND txn_time <= '", date.range$lte, "'"))
  }

  if (length(txn.amount) > 0) {
    if (!is.null(txn.amount$gte))
      conditions <- append(conditions, paste0("AND txn_amount >= ", txn.amount$gte))
    if (!is.null(txn.amount$lte))
      conditions <- append(conditions, paste0("AND txn_amount <= ", txn.amount$lte))
  }
  
  query <- paste0(query, paste0(conditions, collapse="\n"), collapse="\n")
  data <- DBI::dbGetQuery(conn, query)
  data <- data.table(data)
  colnames(data) <- gsub("_", ".", colnames(data))
  data = data[, c("customer.id", "txn.id", "txn.time", "txn.amount", unlist(other)), with=FALSE]
  data$txn.time <- as.POSIXct(data$txn.time)

  return(data)
}


# transform data for model's input from db
formatInputCLV <- function(origin.data, date.range=list(), avg.ticket.range=list(), max.repurchase) {
  # Transform data for model's input from database
  #
  # Arguments:
  #   origin.data: data.table, an order table with different customers' transaction amount at different time
  #   date.range: list, filter transaction time by specifying "lte" (<=) and/or "gte" (>=)
  #   avg.ticket.range: list, filter average transaction value by specifying "lte" (<=) and/or "gte" (>=)
  #   clv.by.cohort: string, divide into each cohort by monthly or quarterly
  #   max.repurchase: integer, the max repurchase number
  #
  # Returns:
  #   A data.table

  if (as.Date(date.range$lte) <= as.Date(max(origin.data$txn.date))) {
    last.date <- as.Date(date.range$lte) + 1
  } else {
    last.date <- as.Date(max(origin.data$txn.date)) + 1
  }

  origin.data$txn.date <- as.Date(origin.data$txn.date)
  order.group <- origin.data[
    (txn.date >= date.range$gte) & (txn.date <= date.range$lte),
    .(
      avg.ticket=mean(txn.amount), frequency=n_distinct(txn.date) - 1,
      first.txn.date=min(txn.date), last.txn.date=max(txn.date)
    ), by=.(customer.id)]

  clv.input <- order.group[
    (avg.ticket >= avg.ticket.range$gte) & (avg.ticket <= avg.ticket.range$lte), 
    .(
      customer.id, clv.by.cohort = 0, first.txn.date, avg.ticket, frequency, 
      recency=as.numeric(difftime(last.txn.date, first.txn.date, units="days")),
      total.time.observed=as.numeric(difftime(last.date, first.txn.date, units="days"))
    )]

  clv.input$clv.by.cohort <- format(clv.input$first.txn.date, "%Y-%m")
  clv.input <- clv.input[frequency<=max.repurchase]

  return(clv.input)
}
