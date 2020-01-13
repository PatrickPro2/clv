Sys.setenv(TZ="UTC")

#Base
library("bit64")

# Shiny
library("shiny")
library("shinymaterial")
library("shinydashboard")
library("shinyjs")

# Database connect
library("DBI")
library("RPostgreSQL")

# Data viz
library("highcharter")
library("ggplot2")
library("DT")

# Data wrangling
library("xts")
library("data.table")
library("stringr")
library("dplyr")
library("tidyr")

# CLV
library("BTYD")

# Color
library("RColorBrewer")

# Methodology definition
source("methodology/data_handler.R")
source("methodology/utils.R")
source("methodology/preread.R")
source("methodology/bgnbd.R")


# Modules definition
source("modules/init_txn_insight.R")
source("modules/init_1st_order_cohort.R")
source("modules/init_retention_insight.R")
source("modules/init_rfm_insight.R") # 慢
source("modules/clv_model_fitting.R")
source("modules/clv_trend.R")
source("modules/clv_1st_order_cohort.R")
source("modules/instruction.R")

customized.theme <- hc_theme(
  colors = c("#00818A", "#F7BE16", "#F76262", "#6B778D"),
  chart = list(backgroundColor = NULL),
  title = list(
    style = list(
      color="#ffffff"
    )
  ),
  subtitle = list(
    style = list(
      color = "#ffffff"
    )
  ),
  legend = list(
    itemStyle = list(
      color = "#ffffff"
    ),
    itemHoverStyle = list(
      color = "#757575"
    )
  )
)
