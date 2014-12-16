rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(outcome == "heart attack") { col <- 11 } 
  else if(outcome == "heart failure") { col <- 17 }
  else if(outcome == "pneumonia") { col <- 23 } 
  else { stop("invalid outcome") }  
  
  states <- unique(data[,7])
  
  df <- do.call(
    rbind,
    lapply(
      states,
      function(f) {
        a <- data[data$State == f, ]
        a[, col] <- suppressWarnings(as.numeric(a[, col]))
        rank <- order(a[, col], a[, 2], na.last=NA)
        if(num == "best") { num <- 1 }
        if(num == "worst") { num <- length(rank) }
        a <- a[rank,]
        data.frame(hospital=a[num,2], state=f, row.names = f)
      }
    )
  )  
  return(df[order(row.names(df)),])
}