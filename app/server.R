server <- function(input, output, session) {
  callModule(initTxnInsight, "page1", order.table, order.detail.table)
  callModule(init1stOrderCohort, "page2", first.order.time.item, purchase.drifting)
  callModule(initRetentionInsight, "page3", retention.data)
  callModule(initRfmInsight, "page4", order.table.describe, first.second.table)
  clv.model.fitting.output <- callModule(clvModelFitting, "page5", order.table)
  callModule(clvTrend, "page6", clv.model.fitting.output)
  callModule(clv1stOrderCohort, "page7", clv.model.fitting.output, order.detail.table)
}
