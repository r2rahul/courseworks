agecount <- function(age = NULL) {
    ## Check that "age" is non-NULL; else throw error
    ## Read "homicides.txt" data file
    ## Extract ages of victims; ignore records where no age is
    ## given
    ## Return integer containing count of homicides for that age
    homicides <- readLines("homicides.txt")
    if (is.null(age)==TRUE){
      stop("age is empty")
    }
    #pat.sub<-grep("[0-9] [Yy]ears old",homicides,value=TRUE)
    pat.match<-paste(as.character(age)," [Yy]ears old",sep="")
    pat.age<-grep(pat.match,homicides)
    count.age<-length(pat.age)
    
    return(as.numeric(count.age))
  }