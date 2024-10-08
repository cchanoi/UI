saturdays <- list(saturdays = c())

date <- list(date = c("10/31/1991","11/30/1991","12/31/1991",
                      "01/31/1992","02/29/1992","03/31/1992",
                      "04/30/1992","05/31/1992","06/30/1992",
                      "07/31/1992","08/31/1992","09/30/1992",
                      "10/31/1992","11/30/1992","12/31/1992",
                      "01/31/1993","02/28/1993","03/31/1993",
                      "04/30/1993","05/31/1993","06/30/1993",
                      "07/31/1993","08/31/1993","09/30/1993",
                      "10/31/1993","11/30/1993","12/31/1993",
                      "01/31/1994","02/28/1994","03/31/1994",
                      "04/30/1994","05/31/1994","06/30/1994",
                      "07/31/1994","08/31/1994","09/30/1994",
                      "10/31/1994","11/30/1994","12/31/1994",
                      "01/31/1995","02/28/1995","03/31/1995",
                      "04/30/1995","05/31/1995","06/30/1995",
                      "07/31/1995","08/31/1995","09/30/1995",
                      "10/31/1995","11/30/1995","12/31/1995",
                      "01/31/1996","02/29/1996","03/31/1996",
                      "04/30/1996","05/31/1996","06/30/1996",
                      "07/31/1996","08/31/1996","09/30/1996",
                      "10/31/1996","11/30/1996","12/31/1996",
                      "01/31/1997","02/28/1997","03/31/1997",
                      "04/30/1997","05/31/1997","06/30/1997",
                      "07/31/1997","08/31/1997","09/30/1997",
                      "10/31/1997","11/30/1997","12/31/1997",
                      "01/31/1998","02/28/1998","03/31/1998",
                      "04/30/1998","05/31/1998","06/30/1998",
                      "07/31/1998","08/31/1998","09/30/1998"))

saturdays_per_month <- list(date, saturdays) %>% 
  as.data.frame()



# Define the function using dplyr
get_date_range <- function(dataset, rptdate_col) {
  dataset %>%
    mutate(!!rptdate_col := mdy(!!sym(rptdate_col))) %>%  # Convert the column to Date format
    summarize(start_date = min(!!sym(rptdate_col), na.rm = TRUE),  # Find the earliest date
              end_date = max(!!sym(rptdate_col), na.rm = TRUE))  # Find the latest date
}

euc <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_EUC_1991_1994.csv")
date_range <- get_date_range(euc, "rptdate")

#
start_date <- as.Date(max)
end_date <- as.Date("1998-09-30")

#Initialize an empty data frame to store saturdays_per_month
saturdays_per_month <- data.frame(month = integer(), year = integer(), saturdays = integer())

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
  saturdays_per_month <- rbind(saturdays_per_month, data.frame(month = month, year = year, saturdays = saturdays))
  
  #Move to the next month
  current_date <- seq.Date(last_day + days(1), by = "month", length.out = 1)
}


# Define the function
get_saturdays_per_month <- function(dataset) {
  # Convert rptdate column to Date format and get the start and end dates
  date_range <- dataset %>% 
    mutate(date = mdy(rptdate)) %>%
    summarize(start_date = min(date),
              end_date = max(date)) 
  
  # Extract start and end dates
  start_date <- date_range$start_date
  end_date <- date_range$end_date
  
  # Initialize an empty data frame to store saturdays_per_month
  saturdays_per_month <- data.frame(month = integer(), 
                                    year = integer(), 
                                    saturdays = integer())
  
  # Loop through each month from start_date to end_date
  current_date <- start_date
  while (current_date <= end_date) {
    year <- year(current_date)
    month <- month(current_date)
    
    # Find the first and last days of the current month
    first_day <- floor_date(current_date, "month")
    last_day <- ceiling_date(current_date, "month") - days(1)
    
    # Generate all dates within the current month
    dates <- seq.Date(first_day, last_day, by = "day")
    
    # Count the number of Saturdays
    saturdays <- sum(weekdays(dates) == "Saturday")
    
    # Append the saturdays_per_month
    saturdays_per_month <- rbind(saturdays_per_month, 
                                 data.frame(month = month, 
                                            year = year, 
                                            saturdays = saturdays))
    
    # Move to the next month
    current_date <- seq.Date(last_day + days(1), by = "month", length.out = 1)
    
    dataset <- dataset %>% 
      mutate(date = mdy(rptdate), 
             month = month(date), 
             year = year(date))  
    
    dataset <- dataset %>%
      left_join(x = dataset, 
                y = saturdays_per_month, 
                by = c("month, year"))
  }
  
  # Return saturdays_per_month
  return(dataset)
}

euc <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_EUC_1991_1994.csv")
euc <- get_saturdays_per_month(euc)


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
library(readr)

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
  
  #select necessary vars and calculate num people on ui
  dataset <- dataset %>% 
    select(st, date, saturdays, c29) %>% 
    rename(state = st, 
           weeks_compensated = c29) %>% 
    mutate(num_on_ui = weeks_compensated/saturdays) %>% 
    select(-c(weeks_compensated))
  
  #make sure we've only got numbers for the 50 states 
  state_abvs <- list(state = c("AL", "AK", "AZ", "AR", "CA", 
                               "CO", "CT", "DE", "FL", "GA", 
                               "HI", "ID", "IL", "IN", "IA", 
                               "KS", "KY", "LA", "ME", "MD", 
                               "MA", "MI", "MN", "MS", "MO", 
                               "MT", "NE", "NV", "NH", "NJ", 
                               "NM", "NY", "NC", "ND", "OH", 
                               "OK", "OR", "PA", "RI", "SC", 
                               "SD", "TN", "TX", "UT", "VT", 
                               "VA", "WA", "WV", "WI", "WY")) %>% 
    as.data.frame()
  
  
  national_dataset <- dataset %>% 
    right_join(x = dataset, 
               y = state_abvs, 
               by = "state") %>% 
    group_by(date) %>% 
    summarise(num_on_ui = sum(num_on_ui)) %>% 
    mutate(month = month(date), 
           year = year(date)) %>% 
    select(year, month, num_on_ui)
  
  fred <- read_csv("FRED/fred_unemployment_numbers.csv") %>% 
    rename(date = DATE, 
           num_unemployed = UNEMPLOY) %>% 
    mutate(date = as.Date(date), 
           month = month(date), 
           year = year(date)) %>% 
    select(year, month, num_unemployed)
  
  national_dataset <- national_dataset %>%
    select(year, month, num_on_ui) %>% 
    left_join(x = national_dataset, 
              y = fred, 
              by = c("month", "year")) %>% 
    group_by(year) %>% 
    summarise(num_on_ui = sum(num_on_ui), 
              num_unemployed = sum(num_unemployed)) %>% 
    mutate(ui_share_of_umep = num_on_ui/num_unemployed) %>% 
    mutate(program = "dataset")
}
