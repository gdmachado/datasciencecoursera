rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(outcome == "heart attack") { col <- 11 } 
  else if(outcome == "heart failure") { col <- 17 }
  else if(outcome == "pneumonia") { col <- 23 } 
  else { stop("invalid outcome") }  
  
  if(state %in% data$State) {
    data <- data[data$State == state, ]
    data[, col] <- suppressWarnings(as.numeric(data[, col]))
    rank <- order(data[,col],data[,2], na.last=NA)
    if(num == "best") { num <- 1 }
    if(num == "worst") { num <- length(rank) }    
    data[order(data[,col],data[,2], na.last=NA),][num,c(2,7,col)]
  } else {
    stop("invalid state")
  }
}