best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(outcome == "heart attack") { col <- 11 } 
  else if(outcome == "heart failure") { col <- 17 }
  else if(outcome == "pneumonia") { col <- 23 } 
  else { stop("invalid outcome") }
  
  if(state %in% data$State) {
    data <- data[data$State == state, ]
    data[, col] <- suppressWarnings(as.numeric(data[, col]))
    data[order(data[,col],data[,2]),][1,2]
  } else {
    stop("invalid state")
  }
}