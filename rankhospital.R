rankhospital <- function(state, outcome, num) {
  
  ## Reads the CSV file and store it as a data frame
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Activates the dplyr package for data frame manipulation
  library(dplyr)
  
  ## Indicates the column numbers that will be used from "df"
  column_index <- c(2, 7, 11, 17, 23)
  
  ## Indicates the names for the column index
  column_names <- names(df)[column_index]
  
  ## Coerce 30-day death rate columns into numeric type
  df[, column_index[3]] <- as.numeric(df[, column_index[3]])
  df[, column_index[4]] <- as.numeric(df[, column_index[4]])
  df[, column_index[5]] <- as.numeric(df[, column_index[5]])
  
  ## Creates a data frame selecting just the columns that will be in use
  sdf <- select(df, all_of(column_index))
  
  ## Creates a variable that contains valid state abbreviations
  states <- distinct(df, df$State)
  
  ## Creates a variable that contains valid outcomes
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Checks if input arguments are correct
  if (!(state %in% unlist(states))) {
    stop("invalid state")
  } 
  if (!is.element(outcome, outcomes)) {
    stop("invalid outcome")
  }
  
  ## Filter rows that satisfy the input value of the state argument
  state_df <- filter(sdf, sdf$State == state)
  
  ## Returns the best hospital name by the specified rank number
  
  if (outcome == outcomes[1]) {
    ## Creates a new column index depending on the outcome argument
    new_ci <- c(1, 2, 3)
    
    ## Creates a data frame with the new column index (new_ci)
    ha_df <- select(state_df, all_of(new_ci))
    
    ## Arrange the 30-day death rate in ascending order, deleting NA's,
    ## and if there is a tie, 
    ## rows will be sorted in alphabetical order by hospital name
    ha_df <- ha_df[order(ha_df[3], ha_df[1], na.last = NA), ]
    
    ## Set the value of "num" argument only if "best" or "worst" is the input
    if (num == "best") {
      num <- 1
    } else if (num == "worst") {
      num <- as.numeric(length(ha_df$Hospital.Name))
    }
    
    ## Creates a Rank column for the above data frame
    ha_df$Rank <- c(1:length(ha_df$Hospital.Name))
    
    ## Print the hospital with the specified ranking number
    print(ha_df[num,1])
    
  } else if (outcome == outcomes[2]) {
    
    ## Creates a new column index depending on the outcome argument
    new_ci <- c(1, 2, 4)
    
    ## Creates a data frame with the new column index (new_ci)
    hf_df <- select(state_df, all_of(new_ci))
    
    ## Arrange the 30-day death rate in ascending order, deleting NA's,
    ## and if there is a tie, 
    ## rows will be sorted in alphabetical order by hospital name
    hf_df <- hf_df[order(hf_df[3], hf_df[1], na.last = NA), ]
    
    ## Set the value of "num" argument only if "best" or "worst" is the input
    if (num == "best") {
      num <- 1
    } else if (num == "worst") {
      num <- as.numeric(length(hf_df$Hospital.Name))
    }
    
    ## Creates a Rank column for the above data frame
    hf_df$Rank <- c(1:length(hf_df$Hospital.Name))
    
    ## Print the hospital with the specified ranking number
    print(hf_df[num,1])
    
  } else if (outcome == outcomes[3]) {
    
    ## Creates a new column index depending on the outcome argument
    new_ci <- c(1, 2, 5)
    
    ## Creates a data frame with the new column index (new_ci)
    p_df <- select(state_df, all_of(new_ci))
    
    ## Arrange the 30-day death rate in ascending order, deleting NA's,
    ## and if there is a tie, 
    ## rows will be sorted in alphabetical order by hospital name
    p_df <- p_df[order(p_df[3], p_df[1], na.last = NA), ]
    
    ## Set the value of "num" argument only if "best" or "worst" is the input
    if (num == "best") {
      num <- 1
    } else if (num == "worst") {
      num <- as.numeric(length(p_df$Hospital.Name))
    }
    
    ## Creates a Rank column for the above data frame
    p_df$Rank <- c(1:length(p_df$Hospital.Name))
    
    ## Print the hospital with the specified ranking number
    print(p_df[num,1])
  }
}
