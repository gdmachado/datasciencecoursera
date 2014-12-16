corr <- function(directory, threshold = 0) {
  monitors <- complete(directory, 1:332)
  ids <- monitors[monitors$nobs > threshold, 1]
  return(sapply(
    ids,
    function(f) {
      df <- read.csv(paste(directory, sprintf("%03d.csv", f), sep="/"))
      cor(df$sulfate, df$nitrate, use="complete")
    }
  ))
}