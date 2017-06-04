rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ocnames <- names(oc)
  mycols <- grepl(sprintf("^Hospital.30.Day.Death.*%s$",gsub(" ", ".", outcome)), ocnames, ignore.case = TRUE)
  oc[, mycols] <- as.numeric(oc[, mycols])
  ocstate <- oc[which(oc[,7]==state),]
  mycolname <- ocnames[mycols]
  ocstate <- ocstate[order(ocstate[[mycolname]], ocstate$Hospital.Name),]
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if (num=="best"){
    return(ocstate[1,2])
  }else if (num=="worst"){
    return(tail(ocstate[!is.na(ocstate[[mycolname]]),],1)[2])
  } else{
    return(ocstate[as.numeric(num),2])
  }
}
