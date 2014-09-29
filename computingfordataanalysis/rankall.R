rankall <- function(outcome, num = "best") {
data.ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
state.name <- names(table(data.ocm[,7]))
state.unique<-unique(state.name)

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
 rank.hospital <- specific.state[order(as.numeric(specific.state[,11]),specific.state[,2],na.last = NA),]
 if(num == "best"){
    best.hospital <- rank.hospital[1,]
  } else {
      if (num == "worst"){
      best.hospital <- tail(rank.hospital,n=1L)
      }
  else {
 
 best.hospital <- rank.hospital[num,]  
   }   
  
  } 
 
 }
 if(outcome == "heart failure"){
 rank.hospital <- specific.state[order(as.numeric(specific.state[,17]),specific.state[,2],na.last = NA),]
 if(num == "best"){
    best.hospital <- rank.hospital[1,]
  } else {
      if (num == "worst"){
       best.hospital <- tail(rank.hospital,n=1L)
      }
  else { 
 best.hospital <- rank.hospital[as.numeric(num),]   
    }   
  
  } 
 
 }
 if(outcome == "pneumonia"){
 rank.hospital <- specific.state[order(as.numeric(specific.state[,23]),specific.state[,2],na.last = NA),]
 if(num == "best"){
    best.hospital <-rank.hospital[1,]
  } else {
      if (num == "worst"){
      best.hospital <- tail(rank.hospital,n=1L)
      }
  else {  
 best.hospital <- rank.hospital[num,]
   }   
  
  } 
 
 }
 return(c(as.character(best.hospital$Hospital.Name),state))
 }
rank.list<-lapply(state.unique,outcome,num,FUN=rankhospital)
list.char<-unlist(rank.list)
length.list<-length(list.char)
rank.final<-data.frame(hospital=list.char[seq(1,length.list,2)],state=list.char[seq(2,length.list,2)],row.names=list.char[seq(2,length.list,2)])
return(rank.final)  
}
