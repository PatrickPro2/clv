# fill in void date
fill.void.date <- function(table, date.col) {
  # Fill in void date for discontinuous column
  #
  # Arguments:
  #   table: data.table, have a column of discontinuous date
  #   date.col: string, identify the column of date
  #
  # Returns:
  #   A data.table with continuous and sorted number

  names(table)[names(table) == date.col] <- "date.col"
  full.date <- data.table(date.col=seq.POSIXt(
    from=as.POSIXct(range(table$date.col)[1], tz="UTC"),
    to=as.POSIXct(range(table$date.col)[2], tz="UTC"),
    by="days"))

  # group by applied by data.table
  continuous.data <- table[full.date, on="date.col"]
  names(continuous.data)[names(continuous.data) == "date.col"] <- date.col
  return(continuous.data)
}


# fill in void number
fill.void.number <- function(table, number.col) {
  # Fill in void number for discontinuous column
  #
  # Arguments:
  #   table: data.table, have a column of discontinuous number
  #   date.col: string, identify the column of date
  #
  # Returns:
  #   A data.table with continuous and sorted number
  
  names(table)[names(table) == number.col] <- "number.col"
  full.number <- data.table(number.col=seq(
    from=range(table$number.col)[1],
    to=range(table$number.col)[2]))

  # group by applied by data.table
  continuous.data <- table[full.number, on="number.col"]
  names(continuous.data)[names(continuous.data) == "number.col"] <- number.col
  return(continuous.data)
}


# output descriptive statistics
descriptive.data <- function(table, number.col, precision.point = 2) {
  # Output a customized descriptive statistcs
  #
  # Arguments:
  #   table: data.table, have a column used for descriptive statistics
  #   number.col: array, identify the continuous columns to get descriptive statistics
  #   precision.point: integer, precision of decimal part
  #
  # Returns:
  #   A one-row data.table containing identified columns' descriptive statistics

  summary.table <- NULL

  for (col in number.col) {
    summary <- table[,
      .(
        min(get(col)),
        quantile(get(col), 0.01, names=FALSE), 
        quantile(get(col), 0.25, names=FALSE),
        median(get(col)), 
        mean(get(col)),
        quantile(get(col), 0.75, names=FALSE), 
        quantile(get(col), 0.99, names=FALSE),
        max(get(col)), 
        sum(get(col))
      ), ]
    summary <- round(summary, precision.point)
    summary.table <- rbind(summary.table, summary)
  }

  colnames(summary.table) <- c("min", "1st", "25th", "median", "mean", "75th", "99th", "max", "sum")
  return(summary.table)
}


# select data based on the range of percentile
data.by.percentile <- function(numeric.data, lower.percentile, upper.percentile) {
  # Select definitive data between lower percentile and upper percentile
  #
  # Arguments:
  #   numeric.data: array
  #   lower.percentile: float, a number between 0.0 and 1.0
  #   upper.percentile: float, a number between 0.0 and 1.0 and larger than lower.percentile
  #
  # Returns:
  #   An array of numeric data from the position of lower percentile to the position of upper percentile
  
  sort.numeric.data <- sort(numeric.data)
  rank.index <- frank(sort.numeric.data, ties.method = "first")
  lower.upper <- round(quantile(rank.index, c(lower.percentile, upper.percentile)))
  numeric.data.by.percentile <- sort.numeric.data[lower.upper[1]:lower.upper[2]]
  return(list(data=numeric.data.by.percentile, percentile.index=lower.upper))
}

