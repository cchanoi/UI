
rm(list = ls())

#UI column figures 
library(dplyr)
library(lubridate)
library(readr) 
library(tidyr)
library(writexl)
library(ggplot2)

#set working directory 
setwd("/Users/Cchanoi/Desktop/UI/UI_Column/")

#download datasets. Keep only the number of all weeks compensated 
#(section b in their respective pages in the data map)
regular_program <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_regular_program.csv") %>% 
  select(st, rptdate, c38) %>% 
  rename(state = st, 
         weeks_compensated = c38)

extended_benefits <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_extended_benefits.csv") %>% 
  select(st, rptdate, c29) %>% 
  rename(state = st, 
         weeks_compensated = c29)

teuc <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_TEUC_2002_2004.csv") %>% 
  select(st, rptdate, c29) %>% 
  rename(state = st, 
         weeks_compensated = c29)

euc <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_EUC_1991_1994.csv") %>% 
  select(st, rptdate, c32) %>% 
  rename(state = st, 
         weeks_compensated = c32)

euc08 <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_EUC08_2008_2013.csv") %>% 
  select(st, rptdate, c29) %>% 
  rename(state = st, 
         weeks_compensated = c29)

peuc <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_PEUC_2020.csv") %>% 
  select(st, rptdate, c29) %>% 
  rename(state = st, 
         weeks_compensated = c29)

eta902p <- read_csv("ETA/ETA902/ETA902_data/ETA902P.csv") %>%
  select(st, rptdate, c5) %>% 
  rename(state = st, 
         weeks_compensated = c5)


#fred
fred <- read_csv("FRED/annual_avg_fred_unemployment_numbers.csv") %>% 
  mutate(UNEMPLOY = as.numeric(UNEMPLOY)) %>%
  mutate(year = year(DATE), 
         num_unemployed = UNEMPLOY*1000) %>% 
  select(year, num_unemployed) 

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



# Function to clean all datasets and process a single dataset
#We want to calculate the number of people on UI 
#But, there is no measure in the data of the number of people on UI, 
#just weeks claimed or compensated in a month. 

#We want to use number of weeks compensated divided by the number of Saturdays 
#in each month as our measure of the number of people.

#To do so we need to find the number of saturdays in a month, calculate the 
#number of people on ui, aggregate that nationally, the find the share of total
#unemployed people that makes up. 

#then to the same for each recession and add them up. 

process_dataset <- function(dataset, state_abvs, fred, dataset_name) {
  # Convert rptdate column to date format and get the start and end dates
  dataset <- dataset %>%
    mutate(date = mdy(rptdate),
           month = month(date),
           year = year(date))
  
  date_range <- dataset %>%
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
  }
  
  # Merge saturdays_per_month back into the original dataset
  dataset <- dataset %>%
    left_join(x = dataset, 
              y = saturdays_per_month, 
              by = c("month", "year"))
  
  # Select necessary variables and calculate the number of people on UI
  dataset <- dataset %>%
    select(state, date, saturdays, weeks_compensated)%>%
    mutate(num_on_ui = weeks_compensated / saturdays) %>%
    select(-weeks_compensated)
  
  # Filter to include only the 50 states
  dataset <- dataset %>%
    right_join(x = dataset, 
               y = state_abvs, 
               by = "state") %>%  # Keep only rows with valid state abbreviations
    mutate(year = year(date)) %>%
    group_by(state, year) %>%
    summarise(num_on_ui = mean(num_on_ui, na.rm = TRUE), .groups = 'drop') %>%
    select(year, state, num_on_ui) %>%  #HERE is probably where we'll want to end for the by state figure 
    group_by(year) %>% 
    summarise(num_on_ui = sum(num_on_ui, na.rm = TRUE))
  
  
  # Merge with FRED data and calculate UI share of unemployment
  
  dataset <- dataset %>%
    left_join(x = dataset, 
              y = fred, 
              by = c("year")) %>%
    mutate(ui_share_of_unemp = num_on_ui / num_unemployed,
           recession = dataset_name) %>% 
    subset(year < 2024) #no average annual unemployment for 2024 so remove
  
  return(dataset)
}

# List of datasets
datasets <- list(regular_program = "regular program", 
                 extended_benefits = "extended_benefits", 
                 euc = "euc", 
                 teuc = "teuc", 
                 euc08 = "euc08", 
                 peuc = "peuc", 
                 eta902p = "eta902p")  

# Apply the function to each dataset and bind the results together
national_ui_share <- bind_rows(lapply(names(datasets), function(name) {
  process_dataset(get(name), state_abvs, fred, name)
}))

national_ui_share_aggregated <- national_ui_share %>%
  select(year, num_on_ui, recession) %>% 
  pivot_wider(
    names_from = recession,
    values_from = num_on_ui) 
  
national_ui_share_aggregated[is.na(national_ui_share_aggregated)] <- 0

national_ui_share_aggregated <- national_ui_share_aggregated %>% 
  left_join(x = national_ui_share_aggregated, 
            y = fred, 
            by = "year") %>% 
  mutate(total_on_ui = 
           (regular_program +
           extended_benefits + 
           euc + 
           teuc + 
           euc08 + 
           peuc + 
           eta902p),
         total_share = total_on_ui / num_unemployed) 

write_xlsx(national_ui_share_aggregated, "national_ui_share_aggregated.xlsx")

national_ui_share_bind <- national_ui_share_aggregated %>% 
  select(year, total_on_ui, num_unemployed, total_share) %>% 
  rename(num_on_ui = total_on_ui, 
         ui_share_of_unemp = total_share) %>% 
  mutate(recession = "aggregate")

national_ui_share <- rbind(national_ui_share, national_ui_share_bind)

write_xlsx(national_ui_share, "national_ui_share.xlsx")

line <- ggplot(data = national_ui_share, aes(x = year, y = ui_share_of_unemp, color = recession)) +
  geom_line(size = 1) +
  labs(title = "UI Share of Unemployment by Year and Recession Category",
       x = "Year",
       y = "UI Share of Unemployment",
       color = "Recession Category") +
  theme_minimal()

line

