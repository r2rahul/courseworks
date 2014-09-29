rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate

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
 if(num < 0 && num >= nrow(specific.state)) {
   return("NA")
   }
   
 if(outcome == "heart attack"){
 if(num == "best"){
    specific.state[,11] <- as.numeric(specific.state[,11])
    best.outcome <- min(na.omit(specific.state[,11]))
    best.hospital <- subset(specific.state, specific.state[,11] == best.outcome)
  } else {
      if (num == "worst"){
      specific.state[,11] <- as.numeric(specific.state[,11])
      best.outcome <- max(na.omit(specific.state[,11]))
      best.hospital <- subset(specific.state, specific.state[,11] == best.outcome)
      }
  else {
 rank.hospital <- specific.state[order(as.numeric(specific.state[,11]),specific.state[,2],na.last = NA),]
 best.hospital <- rank.hospital[num,]  
   }   
  
  } 
 
 }
 if(outcome == "heart failure"){
 if(num == "best"){
    specific.state[,17] <- as.numeric(specific.state[,17])
    best.outcome <- min(na.omit(specific.state[,17]))
    best.hospital <- subset(specific.state, specific.state[,17] == best.outcome)
  } else {
      if (num == "worst"){
      specific.state[,17] <- as.numeric(specific.state[,17])
      best.outcome <- max(na.omit(specific.state[,17]))
      best.hospital <- subset(specific.state, specific.state[,17] == best.outcome)
      }
  else {
 rank.hospital <- specific.state[order(as.numeric(specific.state[,17]),specific.state[,2],na.last = NA),]
 best.hospital <- rank.hospital[as.numeric(num),]   
    }   
  
  } 
 
 }
 if(outcome == "pneumonia"){
 if(num == "best"){
    specific.state[,23] <- as.numeric(specific.state[,23])
    best.outcome <- min(na.omit(specific.state[,23]))
    best.hospital <- subset(specific.state, specific.state[,23] == best.outcome)
  } else {
      if (num == "worst"){
      specific.state[,23] <- as.numeric(specific.state[,23])
      best.outcome <- max(na.omit(specific.state[,23]))
      best.hospital <- subset(specific.state, specific.state[,23] == best.outcome)
      }
  else {
  rank.hospital <- specific.state[order(as.numeric(specific.state[,23]),specific.state[,2],na.last = NA),]
 best.hospital <- rank.hospital[num,]
   }   
  
  } 
 
 }
 return(as.character(best.hospital$Hospital.Name))
 }
