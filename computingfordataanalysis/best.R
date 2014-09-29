best <- function (state,outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome.name<-c("heart attack","heart failure","pneumonia")
  check.outcome<-is.na(match(outcome,outcome.name))
  if (check.outcome==TRUE){
  stop("invalid outcome")
  }
  check.state<-is.na(match(state,data$State))
  if (check.state==TRUE){
  stop("invalid state")
  }
  specific.state <- subset(data, State==state)
  specific.state[specific.state=="Not Available"] <- NA
  if(outcome == "heart attack"){
    specific.state[,11] <- as.numeric(specific.state[,11])
    best.outcome <- min(na.omit(specific.state[,11]))
    best.hospital <- subset(specific.state, specific.state[,11] == best.outcome)
  }
  if(outcome == "heart failure"){
    specific.state[,17] <- as.numeric(specific.state[,17])
    best.outcome <- min(na.omit(specific.state[,17]))
    best.hospital <- subset(specific.state, specific.state[,17] == best.outcome)
  }
  if (outcome=="pneumonia"){
  specific.state[,23] <- as.numeric(specific.state[,23])
  best.outcome <- min(na.omit(specific.state[,23]))
  best.hospital <- subset(specific.state, specific.state[,23] == best.outcome)
  } 
 return(as.character(best.hospital$Hospital.Name))
} 
