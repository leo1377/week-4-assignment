best <- function(state, outcome) { #this function finds the best hospital in a given state, considering only the given outcome
  
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # I read the whole CSV files
  x <- x[, c(2,7,11,17,23)] #I take only the collumns I need
  y <- x[,2] #I take the "state" collumn
  z <- y == state #I take the pisitions with the given state
  if(length(y[z]) == 0){
    stop('invalid state')
  }  #I just checked whether the number of positions with the given state is greater than zero
  
  if(!(outcome =='heart attack' || outcome == 'heart failure' || outcome == 'pneumonia')){
    stop('invalid outcome')
  }
  
  if(outcome=='heart attack'){ #if we concider heart attack
    heart_attack <- x[z,c(1,3)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
    heart_attack[,2] <- as.numeric(heart_attack[,2]) #I coerce numeric in outcome, so i have NAs
    heart_attack <- heart_attack[complete.cases(heart_attack),] #im taking complete cases
    best_hospitals <- heart_attack[heart_attack[,2]==min(heart_attack[,2]),1]
    #I take the names of hospitals with the least level of outcome
    return(sort(best_hospitals)[1])
  }
  if(outcome=='heart failure'){ #if we concider heart failure
    heart_failure <- x[z,c(1,4)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
    heart_failure[,2] <- as.numeric(heart_failure[,2])
    heart_failure <- heart_failure[complete.cases(heart_failure),] #im taking complete cases
    print(heart_failure)
    best_hospitals <- heart_failure[heart_failure[,2]==min(heart_failure[,2]),1]
    print(best_hospitals)
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