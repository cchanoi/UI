#Calculate the number of Saturdays in a month 
#Chiara Chanoi 

#We want to calculate the number of people on UI 
#But, there is no measure in the data of the number of people on UI, 
#just weeks claimed or compensated in a month. 

#We want to use number of weeks compensated divided by the number of Saturdays 
#in each month as our measure of the number of people.

#To do so we need to find the number of saturdays in a month via this function:

####

library(dplyr)
library(lubridate)

#Make function
get_saturdays_per_month <- function(dataset) {
  #Convert rptdate column to date format and get the start and end dates
  dataset <- dataset %>% 
    mutate(date = mdy(rptdate), 
           month = month(date), 
           year = year(date))  
  
  date_range <- dataset %>%
    summarize(start_date = min(date),
              end_date = max(date)) 
  
  #Extract start and end dates
  start_date <- date_range$start_date
  end_date <- date_range$end_date
  
  #Initialize an empty data frame to store saturdays_per_month
  saturdays_per_month <- data.frame(month = integer(), 
                                    year = integer(), 
                                    saturdays = integer())
  
  #Loop through each month from start_date to end_date
  current_date <- start_date
  while (current_date <= end_date) {
    year <- year(current_date)
    month <- month(current_date)
    
    #Find the first and last days of the current month
    first_day <- floor_date(current_date, "month")
    last_day <- ceiling_date(current_date, "month") - days(1)
    
    #Generate all dates within the current month
    dates <- seq.Date(first_day, last_day, by = "day")
    
    #Count the number of Saturdays
    saturdays <- sum(weekdays(dates) == "Saturday")
    
    #Append the saturdays_per_month
    saturdays_per_month <- rbind(saturdays_per_month, 
                                 data.frame(month = month, 
                                            year = year, 
                                            saturdays = saturdays))
    
    #Move to the next month
    current_date <- seq.Date(last_day + days(1), by = "month", length.out = 1)
  }
  
  #Merge saturdays_per_month back into the original dataset
  dataset <- dataset %>%
    left_join(x = dataset, 
              y = saturdays_per_month, 
              by = c("month", "year"))
  
  #Return the updated dataset
  return(dataset)
}





