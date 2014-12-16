complete <- function(directory,id = 1:332) {
  nobs <- sapply(
    id,
    function(f) {
      sum(complete.cases(read.csv(paste(directory, sprintf("%03d.csv", f), sep="/"))))
    }
  )
  data.frame(id, nobs)
}