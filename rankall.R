rankall <- function(outcome, num = 'best'){
# this function finds the hospitals with a given rate of the given outcome in all the states
  
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # I read the whole CSV files
  x <- x[, c(2,7,11,17,23)] #I take only the collumns I need
  
  if(!(outcome =='heart attack' || outcome == 'heart failure' || outcome == 'pneumonia')){
    stop('invalid outcome')
  } #I just checked whether the outcome is valid
  
  states <- unique(x[,2]) # I create a vector with the names of the states
  states <- states[order(states)]
  result <- data.frame(hospital = character(), state = character()) #I create a data frame with two columns
  number <- num #I assign the rank number
  if(num=="best") number <- 1 #if you want the best I take rank 1.
  current_line <- 1

  
  
  
  if(outcome=='heart attack'){ #if we concider heart attack
    for(i in states){ # searching for chosen rate in every state 
      y <- x[,2] #I take the "state" collumn
      z <- y == i #I create a vector with true on positions with a given state
      heart_attack <- x[z,c(1,3)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
      heart_attack[,2] <- as.numeric(heart_attack[,2]) #I coerce numeric in outcome, so i have NAs
      heart_attack <- heart_attack[complete.cases(heart_attack),] #im taking complete cases
      heart_attack <- heart_attack[order(heart_attack[,2],heart_attack[,1]),] #I sort the data frame by ascending mortality rate, and hospital name
      heart_attack$rank <- c(1:dim(heart_attack)[1]) # I add a column with a rank
      if(num=="worst") number<- dim(heart_attack)[1] #if you want the worst I take the last rank
      if(number > dim(heart_attack)[1]){ # if the num is higher than hihgest rank, function returns NA and the name of the state
        result[current_line, ] <- c(NA, i)
      }else{ #if not, it returns the name of the right hospital
        result[current_line, ] <- c(heart_attack[heart_attack[,3]==number, 1], i) # I assign the hospital name with the chosen rate to the current state
      }
      current_line <- current_line + 1 # I go to the next row (next state)
    } 
    return(result) #I return the data frame with states and hospital names with the given rank
  } 
  
  if(outcome=='heart failure'){ #if we concider heart failure
    for(i in states){ # searching for chosen rate in every state 
      y <- x[,2] #I take the "state" collumn
      z <- y == i #I create a vector with true on positions with a given state
      heart_failure <- x[z,c(1,4)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
      heart_failure[,2] <- as.numeric(heart_failure[,2]) #I coerce numeric in outcome, so i have NAs
      heart_failure <- heart_failure[complete.cases(heart_failure),] #im taking complete cases
      heart_failure <- heart_failure[order(heart_failure[,2],heart_failure[,1]),] #I sort the data frame by ascending mortality rate, and hospital name
      heart_failure$rank <- c(1:dim(heart_failure)[1]) # I add a column with a rank
      if(num=="worst") number<- dim(heart_failure)[1] #if you want the worst I take the last rank
        
      if(number > dim(heart_failure)[1]){ # if the num is higher than hihgest rank, function returns NA, and the name of the state
        result[current_line, ] <- c(NA, i)
        }else{ #if not, it returns the name of the right hospital
          result[current_line, ] <- c(heart_failure[heart_failure[,3]==number, 1], i)
        }
      
      current_line <- current_line + 1 # I go to the next row (next state)
    }
    return(result)  #I return the data frame with states and hospital names with the given rank
  }
  
  if(outcome=='pneumonia'){ #if we concider pneumonia
    for(i in states){ # searching for chosen rate in every state 
      y <- x[,2] #I take the "state" collumn
      z <- y == i #I create a vector with true on positions with a given state
      pneumonia <- x[z,c(1,5)] #a new data frame with two collumns: hospital name, and value(only from the selected state)
      pneumonia[,2] <- as.numeric(pneumonia[,2]) #I coerce numeric in outcome, so i have NAs
      pneumonia <- pneumonia[complete.cases(pneumonia),] #im taking complete cases
      pneumonia <- pneumonia[order(pneumonia[,2],pneumonia[,1]),] #I sort the data frame by ascending mortality rate, and hospital name
      pneumonia$rank <- c(1:dim(pneumonia)[1]) # I add a column with a rank
      if(num=="worst") number<- dim(pneumonia)[1] #if you want the worst I take the last rank
      if(number > dim(pneumonia)[1]){ # if the num is higher than hihgest rank, function returns NA, and the name of the state
        result[current_line, ] <- c(NA, i)
      }else{ #if not, it returns the name of the right hospital
        result[current_line, ] <- c(pneumonia[pneumonia[,3]==number, 1], i) # I assign the hospital name with the chosen rate to the current state
      }
      current_line <- current_line + 1 # I go to the next row (next state)
    
    }
    return(result)  #I return the data frame with states and hospital names with the given rank
  }
}
