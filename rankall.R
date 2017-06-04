rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ocnames <- names(oc)
  mycols <- grepl(sprintf("^Hospital.30.Day.Death.*%s$",gsub(" ", ".", outcome)), ocnames, ignore.case = TRUE)
  oc[, mycols] <- as.numeric(oc[, mycols])
  
  splitted <- split(oc, oc[,7])
  
  if (num=="best"){
    r<-data.frame(w2[[1]][order(w2[[1]][[mycolname]]),c(2,7)])
  }else if (num=="worst"){
    return(tail(ocstate[!is.na(ocstate[[mycolname]]),],1)[2])
  } else{
    return(sapply(splitted, function(x){ x[order(x[[mycolname]]),2][as.numeric(num)] }))
  }
}