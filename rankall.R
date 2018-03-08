rankall <- function(outcome, num){
# this function finds the hospitals with a given rate of the given outcome in all the states
  
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # I read the whole CSV files
  x <- x[, c(2,7,11,17,23)] #I take only the collumns I need
  
  if(!(outcome =='heart attack' || outcome == 'heart failure' || outcome == 'pneumonia')){
    stop('invalid outcome')
  } #I just checked whether the outcome is valid
  
  states <- unique(x[,2])
  
  for(i in states){
  # searching for best rate in every state 
  }
}
