pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  setwd(file.path(getwd(), directory)) ## setting the directory
  total = 0                            ## the sum of all observed values of pollutant (either sulfate or nitrate)
  observations = 0                     ## the total number of observed values of pollutant (either sulfate or nitrate)
  
  
  for (i in id)
  {
    
    
           
    if (i <10) { 
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    
    else if (i>=10 & i<100) { 
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""), 
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    
    
    else       { 
      data <- read.csv(paste(as.character(i), ".csv", sep=""),     ## Normal
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
    data = na.omit(data)    
    ##  cumulative addition of the complete observations
    observations = observations + nrow(data)
    ## depending the poluttant ( sulfate or nitrate), we aggregate the observed values
    if (pollutant == "sulfate") {total = total + sum(data$sulfate)}
    else {total = total + sum(data$nitrate)}
    
  }
  
  ## reset directory path
  setwd("..")
  ## returning the mean of the pollutant values
  return (total/observations)
  
}