
#Figures for UI Project 
#Chiara Chanoi

#Objectives: 

# FIGURE 1: 
#A line chart showing various percentiles of maximum benefit duration, 
#in weeks, over time (let’s say 10th, 50th and 90th percentiles to start)


# FIGURE 2: 
# A box and whisker plot of maximum weekly benefit amount, by year

# FIGURE 3: 
# A box and whisker plot of our simulated benefit generosity measure, 
#by year (I suppose this should probably be aggregated to the state level 
#and then plotted; can you include generosity values and then also counts 
#for each cell that can serve as weights for this aggregation?)

rm(list = ls())

#UI column figures 
library(dplyr)
library(lubridate)
library(readr) 
library(purrr)
library(ggplot2)
library(readxl)
library(writexl)
library(tidyr)
library(haven)


# Set the working directory
setwd("/Users/Cchanoi/Desktop/UI/UI_project/")

#Download 

# simulated_UI_parameters.dta: contains average real earnings and 
#simulated UI benefits, weeks, and total generosity (benefits x weeks) 
#for each year-state-demographic cell, as well as information on which 
#demographic groups belong to the different cells. 
#There also is a count of observations in each year-state-cell 
#combination that can be used as a weight.

simulated <- read_dta("from_david/simulated_UI_parameters.dta")

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

# dol_ui_params_allyears.xlsx: contains data scraped from DOL pdfs on the 
#maximum available benefits for each state and half year (Jan-Jun, Jul-Dec). 
#The documentation for these variables can be found here:
#https://oui.doleta.gov/unemploy/content/sigpros/2020-2029/January2024.pdf

dol <- read_xlsx("from_david/dol_ui_params_allyears.xlsx") %>% 
  mutate(year_factor = as.factor(year), 
         benefit_max = as.numeric(benefit_max), 
         benefit_max_dep = as.numeric(benefit_max_dep), 
         time = ifelse(month == 1, year,year + 0.5)) %>% 
  rename(state = postal) 

dol <- dol %>% 
  right_join(x = dol, 
             y = state_abvs, 
             by = "state")

# FIGURE 1: 
#A line chart showing various percentiles of maximum benefit duration, 
#in weeks, over time (let’s say 10th, 50th and 90th percentiles to start)
years <- seq(2002, 2022, by = 0.5)


dol_7 <- dol %>% 
  subset(month == 7) 
  
percentile_by_year <- function(years) { 
  
  dol_year <- dol %>% 
    subset(time == years) %>% 
    select(year, month, time, state, weeks_u) %>% 
    mutate(decile = ntile(weeks_u, 50), 
           decile_factor = as.factor(decile))
  
  }
  
# Apply the function to each state and bind the results together
dol_percentiles <- bind_rows(lapply(years, percentile_by_year)) 

dol_fig1 <- dol_percentiles %>% 
  subset(decile == 5 | decile == 25 | decile == 45)

line <- ggplot(data = dol_fig1, 
               aes(x = time, y = weeks_u, linetype = decile_factor, color = decile_factor)) +
  geom_line(size = 1) +
  labs(x = "Year",
       y = "Maximum benefit duration",
       color = "Percentile") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2002, 2022, by = 1)) + # Add x-ticks for every year from 2002 to 2022
  scale_y_continuous(breaks = seq(15, 30, by = 1))


line

# FIGURE 2: 
# A box and whisker plot of maximum weekly benefit amount, by year

hist <- ggplot(data = dol, aes(x = year_factor, y = benefit_max_dep)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Year",
       y = "Maximum benefit (with dependents)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = as.character(2002:2022))  # Add x-ticks for every year from 2002 to 2022

hist

# FIGURE 3: 
# A box and whisker plot of our simulated benefit generosity measure, 
#by year (I suppose this should probably be aggregated to the state level 
#and then plotted; can you include generosity values and then also counts 
#for each cell that can serve as weights for this aggregation?)
# Create the data frame with state abbreviations and corresponding FIPS codes

state_data <- data.frame(
  state = c("AL", "AK", "AZ", "AR", "CA", 
            "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", 
            "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", 
            "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", 
            "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", 
            "VA", "WA", "WV", "WI", "WY"),
  statefip = c(1, 2, 4, 5, 6, 
                8, 9, 10, 12, 13, 
                15, 16, 17, 18, 19, 
                20, 21, 22, 23, 24, 
                25, 26, 27, 28, 29, 
                30, 31, 32, 33, 34, 
                35, 36, 37, 38, 39, 
                40, 41, 42, 44, 45, 
                46, 47, 48, 49, 50, 
                51, 53, 54, 55, 56)
)


simulated_states <- simulated %>% 
  right_join(x = simulated, 
             y = state_data, 
             by = "statefip") %>% 
  select(year, statefip, state, totalUI, n) %>% 
  group_by(state, year) %>% 
  summarise(generosity_state_average = weighted.mean(totalUI, n)) %>% 
  mutate(year_factor = as.factor(year))

hist2 <- ggplot(data = simulated_states, aes(x = year_factor, y = generosity_state_average)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Year",
       y = "Simulated generosity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = as.character(2005:2019))  # Add x-ticks for every year from 2002 to 2022

hist2
