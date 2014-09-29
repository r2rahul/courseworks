count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause
  homicides <- readLines("homicides.txt")   
  cause.check<-c("asphyxiation", "blunt force", "other", "shooting",
                 "stabbing", "unknown")
  k<-paste("\\b",cause,"\\b",sep="")
  check.cause<-grep(k,cause.check)
  if (check.cause==0){
    stop("Wrong Cause or no Cause provided")
  }
  else {
    length.cause<-nchar(cause)
    pat.match<-paste("Cause: [",toupper(substring(cause,1,1)),tolower(substring(cause,1,1)),"]",substring(cause,2),sep="")
    pat.cause<-grep(pat.match,homicides)
    count.cause<-length(pat.cause)
    
  }
  return(as.integer(count.cause))
}