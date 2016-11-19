best <- function(state, outcome){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Check that state and outcome are valid
  if(!state %in% data$State){
    stop("invalid state")
  }
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  data = data[data$State == state,]
  data1 = data[,c(2, outcomes[outcome])]
  names(data1) = c("hospitals", outcome)
  data1 = data1[order(data1$hospitals),]
  data1 = data1[order(data1[outcome]),]
  return(data1[1,1])
}

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Check that state and outcome are valid
  if(!state %in% data$State){
    stop("invalid state")
  }
  else if(!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }
  
  data = data[data$State == state,]
  data1 = data[,c(2, outcomes[outcome])]
  names(data1) = c("hospitals", outcome)
  data1 = data1[order(data1$hospitals),]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data.outcome = data1[outcome]
  if(num == "best"){
    num = 1
  }
  else if(num == "worst"){
    num = nrow(data.outcome)-sum(is.na(data.outcome))
  }
  data1 = data1[order(data.outcome),]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(data1[num,1])
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Check that outcome is valid
  if(!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }
  
  if(num == "best"){
    num = 1
  }
  
  data1 = data[,c(2, 7, outcomes[outcome])]
  names(data1) = c("hospitals","states", outcome)
  data1 = data1[order(data1$hospitals),]
  data1 = data1[order(data1[outcome]),]
  data1 = data1[order(data1$states),]
  split.data = split(data1, data1$states)
  results = sapply(split.data, function(x) x[num,1])
  results = sapply(split.data, function(x){
    if(num == "worst"){
      num = nrow(x[outcome])-sum(is.na(x[outcome]))
    }
    x[num,1]
  })
  data.frame(hospital=results, state=names(results), row.names=names(results))
}  
  