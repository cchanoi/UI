
rm(list = ls())

#UI column figures 
library(dplyr)
library(lubridate)
library(readr) 
library(purrr)
library(ggplot2)
library(writexl)
library(tidyr)

# Set the working directory
setwd("/Users/Cchanoi/Desktop/UI/UI_column/FRED/state_unemployment_numbers/")

# List all CSV files in the current working directory
csv_files <- list.files(pattern = "*.csv")

# Read all CSV files into a list of data frames
data_list <- lapply(csv_files, read.csv)

# Function to create a valid name from a file name
create_valid_name <- function(file_name) {
  name <- gsub("\\.csv$", "", file_name)  # Remove the .csv extension
  name <- make.names(name)                # Ensure the name is valid in R
  return(name)
}

# Read and assign each CSV file to a separate data frame
lapply(csv_files, function(file) {
  data <- read.csv(file)
  data_name <- create_valid_name(file)
  assign(data_name, data, envir = .GlobalEnv)
})

# Assuming you have a list of files or data frames
state_files <- list("UNEMPLOYAL.csv", "UNEMPLOYAK.csv", 
                    "UNEMPLOYAZ.csv", "UNEMPLOYAR.csv", 
                    "UNEMPLOYCA.csv", "UNEMPLOYCO.csv", 
                    "UNEMPLOYCT.csv", "UNEMPLOYDE.csv", 
                    "UNEMPLOYFL.csv", "UNEMPLOYGA.csv", 
                    "UNEMPLOYHI.csv", "UNEMPLOYID.csv", 
                    "UNEMPLOYIL.csv", "UNEMPLOYIN.csv", 
                    "UNEMPLOYIA.csv", "UNEMPLOYKS.csv", 
                    "UNEMPLOYKY.csv", "UNEMPLOYLA.csv", 
                    "UNEMPLOYME.csv", "UNEMPLOYMD.csv", 
                    "UNEMPLOYMA.csv", "UNEMPLOYMI.csv", 
                    "UNEMPLOYMN.csv", "UNEMPLOYMS.csv", 
                    "UNEMPLOYMO.csv", "UNEMPLOYMT.csv", 
                    "UNEMPLOYNE.csv", "UNEMPLOYNV.csv", 
                    "UNEMPLOYNH.csv", "UNEMPLOYNJ.csv", 
                    "UNEMPLOYNM.csv", "UNEMPLOYNY.csv", 
                    "UNEMPLOYNC.csv", "UNEMPLOYND.csv", 
                    "UNEMPLOYOH.csv", "UNEMPLOYOK.csv", 
                    "UNEMPLOYOR.csv", "UNEMPLOYPA.csv", 
                    "UNEMPLOYRI.csv", "UNEMPLOYSC.csv", 
                    "UNEMPLOYSD.csv", "UNEMPLOYTN.csv", 
                    "UNEMPLOYTX.csv", "UNEMPLOYUT.csv", 
                    "UNEMPLOYVT.csv", "UNEMPLOYVA.csv", 
                    "UNEMPLOYWA.csv", "UNEMPLOYWV.csv", 
                    "UNEMPLOYWI.csv", "UNEMPLOYWY.csv") 

# Function to reformat a single dataset
reformat_state_data <- function(file_path) {
  # Extract state abbreviation from file name
  state_abbr <- gsub("UNEMPLOY|\\.csv", "", basename(file_path))
  
  # Read the dataset
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Check the structure of the dataset
  str(data)
  
  # Reformat the dataset
  reformatted_data <- data %>%
    mutate(
      DATE = as.Date(DATE, format = "%Y-%m-%d"),  # Use correct column name
      year = as.numeric(format(DATE, "%Y")),
      state = state_abbr,
      num_unemployed = as.numeric(data[[2]])  # Convert unemployment data to numeric
    ) %>%
    select(year, state, num_unemployed)
  
  return(reformatted_data)
}

# Apply the reformatting function to all state datasets
fred_states <- map_dfr(state_files, reformat_state_data)

# The resulting `all_states_data` will be a single data frame with all states combined

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
fred <- read_csv("FRED/fred_unemployment_numbers.csv") %>% 
  rename(date = DATE) %>% 
  mutate(date = as.Date(date), 
         month = month(date), 
         year = year(date), 
         num_unemployed = UNEMPLOY*1000) %>% 
  select(year, month, num_unemployed)

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



state_abvs_list <- c("AL", "AK", "AZ", "AR", "CA", 
                     "CO", "CT", "DE", "FL", "GA", 
                     "HI", "ID", "IL", "IN", "IA", 
                     "KS", "KY", "LA", "ME", "MD", 
                     "MA", "MI", "MN", "MS", "MO", 
                     "MT", "NE", "NV", "NH", "NJ", 
                     "NM", "NY", "NC", "ND", "OH", 
                     "OK", "OR", "PA", "RI", "SC", 
                     "SD", "TN", "TX", "UT", "VT", 
                     "VA", "WA", "WV", "WI", "WY")

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

process_states_dataset <- function(dataset, state_abvs, fred, dataset_name) {
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
      select(year, state, num_on_ui)
    
    # Merge with FRED data and calculate UI share of unemployment
    
    dataset <- dataset %>%
      left_join(x = dataset, 
                y = fred_states, 
                by = c("year", "state")) %>% 
      mutate(ui_share_of_unemp = num_on_ui / num_unemployed,
             recession = dataset_name) %>%
      subset(year > 2003 & year < 2024) #no average annual unemployment for 2024 so remove
    
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
national_ui_share_states <- bind_rows(lapply(names(datasets), function(name) {
  process_states_dataset(get(name), state_abvs, fred, name)
}))

add_aggregate_ui_share <- function(state_to_aggregate) {
  aggregate_data <- national_ui_share_states %>% 
    subset(state == state_to_aggregate) %>% 
    select(year, state, num_on_ui, recession) %>% 
    pivot_wider(
      names_from = c(recession),
      values_from = num_on_ui)  
  
  aggregate_data[is.na(aggregate_data)] <- 0
  
  aggregate_data <- aggregate_data %>% 
    left_join(x = aggregate_data, 
              y = fred_states, 
              by = c("year", "state")) %>% 
    mutate(recession = "aggregated",
           total_on_ui = 
             (regular_program +
                extended_benefits + 
                teuc + 
                euc08 + 
                peuc + 
                eta902p),
           total_share = total_on_ui / num_unemployed)
  
  return(aggregate_data)  # Ensure the function returns the result
  
}

# Apply the function to each state and bind the results together
national_ui_share_aggregated_states <- bind_rows(lapply(state_abvs_list, add_aggregate_ui_share)) %>% 
  mutate(year_factor = as.factor(year))


national_ui_share_bind_states <- national_ui_share_aggregated_states %>% 
  select(year, state, total_on_ui, num_unemployed, total_share, recession) %>% 
  rename(num_on_ui = total_on_ui, 
         ui_share_of_unemp = total_share)

national_ui_share_states <- rbind(national_ui_share_states, 
                                  national_ui_share_bind_states) %>% 
  mutate(year_factor = as.factor(year))

write_xlsx(national_ui_share_states, "national_ui_share_states.xlsx")

write_xlsx(national_ui_share_aggregated_states, "national_ui_share_aggregated_states.xlsx")


hist <- ggplot(data = national_ui_share_aggregated_states, aes(x = year_factor, y = total_share)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Year",
       y = "Share of unemployed people on UI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


hist


