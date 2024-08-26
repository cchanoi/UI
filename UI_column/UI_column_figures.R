#Make UI column figures 
#Chiara Chanoi

rm(list = ls())

library(readr)

#set working directory 
setwd("/Users/Cchanoi/Desktop/UI/UI_Column/")

#download dataset
euc <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_EUC_1991_1994.csv")

source("saturdays_per_month.R")

euc <- get_saturdays_per_month(euc) %>% 
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
               

national_euc <- euc %>% 
  right_join(x = euc, 
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

national_euc <- national_euc %>%
  select(year, month, num_on_ui) %>% 
  left_join(x = national_euc, 
            y = fred, 
            by = c("month", "year")) %>% 
  group_by(year) %>% 
  summarise(num_on_ui = sum(num_on_ui), 
            num_unemployed = sum(num_unemployed)) %>% 
  mutate(ui_share_of_umep = num_on_ui/num_unemployed) %>% 
  mutate(program = "euc")
               
               
               
               
               


