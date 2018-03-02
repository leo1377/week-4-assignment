best <- function(state, outcome) { #this function finds the best hospital in a given state, considering only the given outcome
  
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # I read the whole CSV files
  x <- x[, c(2,7,11,17,23)] #I take only the collumns I need
  y <- x[,2] #I take the "state" collumn
  z <- y == state #I take the pisitions with the given state
  if(length(y[,z]) == 0){
    stop(paste('Error in best("',state,'","',outcome,'") : invalid state)'))
  }  #I just checked whether the number of positions with the given state is greater than zero
  
  if(!(outcome =='heart attack' || outcome == 'heart failure' || outcome == 'pneumonia')){
    stop(paste('Error in best("',state,'","',outcome,'") : invalid outcome)'))
  }
  
  if(outcome=='heart attack'){ #if we concider heart attack
    heart_attack <- x[z,c(1,3)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
    #!!! im here, now i have to find the best hospital and print the name
  }
  
 
  
  
  
  
  ## Return hospital name in that state with lowest 30-day death
  
  ## rate
}