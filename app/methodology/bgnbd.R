getOutputCLV <- function(model.data, discount.rate, predict.precision, predict.period.length) {
  # Fit rfm order data
  #
  # Arguments:
  #   model.data: A data.table wtih 3 columns: customer.id, first.txn.date, avg.ticket, frequency, recency, total.time.observed
  #   discount.rate: float, to calculate current discounted value of future CLV
  #   predict.precision: string, by year, by quarter, by month
  #   predict.period.length: integer, the length under predict.precision
  #
  # Returns:
  #   A data.table with every customer's CLV
  
  colnames(model.data) <- c("customer.id", "clv.by.attribute", "first.txn.date", "avg.ticket", "x", "t.x", "T.cal")
  predict.precision.length <- switch(predict.precision, year=365, quarter=90, month=30)

  cohort.clv.by.attribute <- sort(unique(model.data$clv.by.attribute))
  params <- NULL
  model <- NULL
  
  # The 1st iteration is to calculate clv for each cohort, while the 2nd iteration is to calculate cumulative clv
  for (cohort in cohort.clv.by.attribute) {
    cohort.model <- model.data[clv.by.attribute == cohort]
    cohort.params <- bgnbd.EstimateParameters(cal.cbs=cohort.model[, .(x, t.x, T.cal), ])
    cohort.model$palive <- bgnbd.PAlive(params=cohort.params, x=cohort.model$x, t.x=cohort.model$t.x, T.cal=cohort.model$T.cal)
    
    for (length in 1:predict.period.length) {
      cumulative.expected.transactions <- bgnbd.ConditionalExpectedTransactions(
        params=cohort.params, T.star=length*predict.precision.length, x=cohort.model$x, t.x=cohort.model$t.x, T.cal=cohort.model$T.cal)

      if (length > 1) {
        period.in.expected.transactions <- (
          cumulative.expected.transactions - tmp.cumulative.expected.transactions) / (1 + discount.rate)**(length - 1)
        clv.name <- paste0("clv", length)
        cohort.model[, clv.name] <- period.in.expected.transactions * cohort.model$avg.ticket
        cohort.model$expected.number.transactions <- cohort.model$expected.number.transactions + (period.in.expected.transactions)
      } else {
        cohort.model$expected.number.transactions <- cumulative.expected.transactions
        clv.name <- paste0("clv", length)
        cohort.model[, clv.name] <- cumulative.expected.transactions * cohort.model$avg.ticket
      }

      tmp.cumulative.expected.transactions <- cumulative.expected.transactions
    }

    cohort.model$clv <- cohort.model$expected.number.transactions * cohort.model$avg.ticket
    model <- rbind(model, cohort.model)
    params <- rbind(params, c(cohort, cohort.params))
  }
  
  params <- data.table(params)
  colnames(params) <- c("clv.by.attribute", "r", "alpha", "a", "b")
  model.output <- list(model=model, params=params)
  return(model.output)
}
