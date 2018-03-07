rankhospital <- function(state, outcome, num) { 
  # this function finds teh hospitalwith a given rate of the given outcome in the given state
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # I read the whole CSV files
  x <- x[, c(2,7,11,17,23)] #I take only the collumns I need
  y <- x[,2] #I take the "state" collumn
  z <- y == state #I create a vector with true on positions with a given state
  
  if(length(y[z]) == 0){
    stop('invalid state')
  }  #I just checked whether the number of positions with the given state is greater than zero
  
  if(!(outcome =='heart attack' || outcome == 'heart failure' || outcome == 'pneumonia')){
    stop('invalid outcome')
  } #I just checked whether the outcome is valid
  
  if(num=="best") num<- 1
  
  if(outcome=='heart attack'){ #if we concider heart attack
    heart_attack <- x[z,c(1,3)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
    heart_attack[,2] <- as.numeric(heart_attack[,2]) #I coerce numeric in outcome, so i have NAs
    heart_attack <- heart_attack[complete.cases(heart_attack),] #im taking complete cases
    heart_attack <- heart_attack[order(heart_attack[,2],heart_attack[,1]),] #I sort the data frame by ascending mortality rate, and hospital name
    heart_attack$rank <- c(1:dim(heart_attack)[1]) # I add a column with a rank
    if(num > dim(heart_attack)[1]) return(NA)
    if(num=="worst") num<- dim(heart_attack)[1]
    
    
    return(heart_attack[1,heart_attack[,3]==num]) #I return the name of hospitals with the best rank and first name (if there is a tie)
  } # I finished here
  
  if(outcome=='heart failure'){ #if we concider heart failure
    heart_failure <- x[z,c(1,4)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
    heart_failure[,2] <- as.numeric(heart_failure[,2])
    heart_failure <- heart_failure[complete.cases(heart_failure),] #im taking complete cases
    best_hospitals <- heart_failure[heart_failure[,2]==min(heart_failure[,2]),1]
    #I take the names of hospitals with the least level of outcome
    return(sort(best_hospitals)[1])
  }
  if(outcome=='pneumonia'){ #if we concider pneumonia
    pneumonia <- x[z,c(1,5)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
    pneumonia[,2] <- as.numeric(pneumonia[,2])
    pneumonia <- pneumonia[complete.cases(pneumonia),] #im taking complete cases
    best_hospitals <- pneumonia[pneumonia[,2]==min(pneumonia[,2]),1]
    #I take the names of hospitals with the least level of outcome
    return(sort(best_hospitals)[1])
  }
  
}