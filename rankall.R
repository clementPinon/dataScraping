rankall <- function(d = NULL, Outcome = NULL, Num = "best") {
  # Read outcome data
  # Data needs to be loaded already, this is the first variable of our function
  if(is.null(d)){
    stop('No data specified')
    }
  
  outcome <- d
  # Here we use to pre loaded data set we loaded thanks to dataScraping function in the same working directory
  #outcome <- mudRunData
  # We set a vector containing the names of the different sections thanks to which we want to sort or dataset
  # We only focus on SEx, Age, Country, Team, Name of the category and the localization
  Cases <- c('Sex', 'Age', 'Country', 'Team', 'Cat_name','Localization')
  
  # Initialisation of vectors we will then fill in
  Requested.outcome <- vector()
    
  # Check that outcome is valid, we'll match it with Cases and should describe the asset on which we want to focus (sex, Aeg etc..)
  if(is.null(Outcome)){
    stop('No outcome specified')
    }
  else if(Outcome %in% Cases == FALSE){
    stop('No such category in  the current dataset')
    }
  else{
      # We then check on which column we want to focus
      if(Outcome == Cases[1]){Type <- as.numeric(3)}
      else if (Outcome == Cases[2]){Type <- as.numeric(5)}
      else if (Outcome == Cases[3]){Type <- as.numeric(6)}
      else if (Outcome == Cases[4]){Type <- as.numeric(7)}
      else if (Outcome == Cases[5]){Type <- as.numeric(13)}
      else if (Outcome == Cases[6]){Type <- as.numeric(14)}
      
      # Set a vector that lists the different types of elements from our data set (type of sex, categories, countries)
      outcomeList <- outcome[,Type][!duplicated(outcome[,Type])]
      # Sort them
      outcomeList <- sort(outcomeList)
        
        # For each outcome, find the runner of the given rank
        for (i in 1:length(outcomeList)){
          data <- outcome[which(outcome[,Type] == outcomeList[i]),]
          # Order the data by position, not necessary as the dataset should already 
          # be ordered but may be worth it as an additional control
          # Again, as data$Position is a factor the function as.numeric will not work properly
          # we rather use as.numeric(levels(data$Position))[data$Position]
          ordered_data <- data[order(as.numeric(levels(data$Position))[data$Position],data[,Type]),]
          
          # Set the variable Rank depending on the ranking we want to focus on (Best, Worst or any given number)
          if (is.numeric(Num)){
            Rank <- Num
          }
          else if (Num == "best"){
            Rank <- 1
          }
          else if (Num == "worst"){
            Rank <- dim(ordered_data)[1]
          }
          
          # Compute after each iteration the data frame for wich we collect only the requested ranking for each category of the given outcome
          Requested.outcome <- rbind(Requested.outcome, ordered_data[Rank, ])
    }     
  }
  # Return a data frame with the runners of a certain rank for each category of the given outcome
  Requested.outcome <- Requested.outcome[order(as.numeric(Requested.outcome$Position)),]

}