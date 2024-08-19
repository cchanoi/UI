#Replicate CPS data try
#Chiara Chanoi

rm(list = ls())

library(dplyr)
library(tidyverse)
library(readxl)
library(writexl) 
library(haven)

#set working directory 
setwd("/Users/Cchanoi/Desktop/UI/July2024_jobs_report/")

cps_07_24 <- read_dta("epi_cpsbasic_2000_2024/epi_cpsbasic_2024_7.dta") 

#Let's start recreating Table A1

#First I need to recreate the civilian non-institutional population
#CPS is at the household level, but BLS assigns a weight to each HH, 
#which equals the number of people represented by its data. 
#The variable "cmpwgt" is the BLS sample weight, and is for ages 16+. 

#See: https://microdata.epi.org/variables/weights/cmpwgt/

#To get the civilian non-institutional population (in thousands), sum cmpwgt  
civ_ni_pop_a1 = sum(cps_07_24$cmpwgt, na.rm = TRUE)/1000

#Let's get the number of employed people, and the employment population ratio
cps_employed_a1 <- cps_07_24 %>% 
  subset(emp == 1) 

employed_a1 = sum(cps_employed_a1$cmpwgt, na.rm = TRUE)/1000

employed_pop_ratio_a1 = employed_a1/civ_ni_pop_a1

#Let's get the number of unemployed people
cps_unemployed_a1 <- cps_07_24 %>% 
  subset(unemp == 1)

unemployed_a1 = sum(cps_unemployed_a1$cmpwgt, na.rm = TRUE)/1000

#The labor force is the sum of the employed and unemployed people
labor_force_a1 = employed_a1 + unemployed_a1 

#The labor force participation rate is the employed people over the labor force
participation_rate_a1 = labor_force_a1/civ_ni_pop_a1

#The unemployment rate is the number of unemp people divided by the labor force
unemployed_rate_a1 = unemployed_a1/labor_force_a1

#Everything matches!! 
#Let's make an excel sheet which includes the numbers we need 

table_A1 <- list(Total = 
                    c(
                      "Civilian noninstitutional population", 
                      "Civilian labor force", 
                      "Participation rate", 
                      "Employed", 
                      "Employment-population ratio", 
                      "Unemployed", 
                      "Unemployment rate"),
                  Not_Seasonally_Adjusted_July_2024 = 
                    c(
                      round(civ_ni_pop_a1), 
                      round(labor_force_a1),
                      round(participation_rate_a1*100, digits = 1),
                      round(employed_a1), 
                      round(employed_pop_ratio_a1*100, digits = 1),
                      round(unemployed_a1), 
                      round(unemployed_rate_a1*100, digits = 1))) %>% 
  as.data.frame() %>% 
  mutate(Not_Seasonally_Adjusted_July_2024 
         = as.numeric(Not_Seasonally_Adjusted_July_2024))

write_xlsx(table_A1, "table_A1.xlsx")

################################################################################
#TABLE A4: 25 years and older, some college or associates degree
################################################################################

cps_some_college <- cps_07_24 %>% 
  subset(age >= 25) %>% 
  subset(educ == 3)

#Let's start recreating Table A4

#To get the civilian non-institutional population (in thousands), sum cmpwgt  
civ_ni_pop_a4 = sum(cps_some_college$cmpwgt, na.rm = TRUE)/1000

#Let's get the number of employed people, and the employment population ratio
cps_employed_a4 <- cps_some_college %>% 
  subset(emp == 1) 

employed_a4 = sum(cps_employed_a4$cmpwgt, na.rm = TRUE)/1000

employed_pop_ratio_a4 = employed_a4/civ_ni_pop_a4

#Let's get the number of unemployed people
cps_unemployed_a4 <- cps_some_college %>% 
  subset(unemp == 1)

unemployed_a4 = sum(cps_unemployed_a4$cmpwgt, na.rm = TRUE)/1000

#The labor force is the sum of the employed and unemployed people
labor_force_a4 = employed_a4 + unemployed_a4 

#The labor force participation rate is the employed people over the labor force
participation_rate_a4 = labor_force_a4/civ_ni_pop_a4

#The unemployment rate is the number of unemp people divided by the labor force
unemployed_rate_a4 = unemployed_a4/labor_force_a4

#Everything matches!! 
#Let's make an excel sheet which includes the numbers we need 

table_A4 <- list(Total = 
                   c(
                     "Civilian labor force", 
                     "Participation rate", 
                     "Employed", 
                     "Employment-population ratio", 
                     "Unemployed", 
                     "Unemployment rate"),
                 Not_Seasonally_Adjusted_July_2024 = 
                   c( 
                     round(labor_force_a4),
                     round(participation_rate_a4*100, digits = 1),
                     round(employed_a4), 
                     round(employed_pop_ratio_a4*100, digits = 1),
                     round(unemployed_a4), 
                     round(unemployed_rate_a4*100, digits = 1))) %>% 
  as.data.frame() %>% 
  mutate(Not_Seasonally_Adjusted_July_2024 
         = as.numeric(Not_Seasonally_Adjusted_July_2024))

write_xlsx(table_A4, "table_A4.xlsx") 
#Okay the percentages (what I've been assigned are the same)
#But the sounts are 21 off (18 employed, 4 unemployed). 
#I don't think I'm missing anything though because the smallest weight is 245+

################################################################################
#TABLE A11: Unemployed persons by reason for unemployment.
#Unemployed as a percent of the civilian labor force
################################################################################

#job losers and persons who completed temporary jobs
job_losers_temp_jobs <- cps_07_24 %>% 
  subset(whyunemp == 1 | whyunemp == 2 | whyunemp == 3) 

job_losers_temp_jobs_a11 = sum(job_losers_temp_jobs$cmpwgt, na.rm = TRUE)/1000 
job_losers_prop_of_labor_force = job_losers_temp_jobs_a11/(labor_force_a1)

#job leavers 
job_leavers <- cps_07_24 %>% 
  subset(whyunemp == 4)

job_leavers_a11 = sum(job_leavers$cmpwgt, na.rm = TRUE)/1000
job_leavers_prop_of_labor_force = job_leavers_a11/(labor_force_a1)

#reentrants
reentrants <- cps_07_24 %>% 
  subset(whyunemp == 5)

reentrants_a11 = sum(reentrants$cmpwgt, na.rm = TRUE)/1000 
reentrants_prop_of_labor_force = reentrants_a11/(labor_force_a1)

#new entrants
new_entrants <- cps_07_24 %>% 
  subset(whyunemp == 6)

new_entrants_a11 = sum(new_entrants$cmpwgt, na.rm = TRUE)/1000 
new_entrants_prop_of_labor_force = new_entrants_a11/(labor_force_a1)

#make excel sheet 
table_A11 <- list(Percentage = 
                   c(
                     "Job losers and persons who completed temporary jobs", 
                     "Job leavers", 
                     "Reentrants", 
                     "New entrants" 
                     ),
                 Unemployed_As_a_Percent_of_The_Civilian_Labor_Force = 
                   c(
                     round(job_losers_prop_of_labor_force*100, digits = 1), 
                     round(job_leavers_prop_of_labor_force*100, digits = 1), 
                     round(reentrants_prop_of_labor_force*100, digits = 1), 
                     round(new_entrants_prop_of_labor_force*100, digits = 1)
                   )) %>% 
  as.data.frame() %>% 
  mutate(Unemployed_As_a_Percent_of_The_Civilian_Labor_Force 
         = as.numeric(Unemployed_As_a_Percent_of_The_Civilian_Labor_Force))

write_xlsx(table_A11, "table_A11.xlsx") 
#Job losers and persons who completed temporary jobs is off by 0.1. 

################################################################################
#TABLE A11: Unemployed persons by reason for unemployment.
#Unemployed as a percent of the civilian labor force
################################################################################

#unemployed for less than 5 weeks 
unemp_less_than_5_wks <- cps_unemployed_a1 %>% 
  subset(unempdur < 5) 

less_than_5_wks <- sum(unemp_less_than_5_wks$cmpwgt, na.rm = TRUE)/1000
less_than_5_wks_percentage <- less_than_5_wks/unemployed_a1 

#unemployed for 5 to 14 weeks 
unemp_5_to_14_wks <- cps_unemployed_a1 %>% 
  subset(unempdur >= 5 & unempdur <= 14) 

from_5_to_14_wks <- sum(unemp_5_to_14_wks$cmpwgt, na.rm = TRUE)/1000
from_5_to_14_wks_percentage <- from_5_to_14_wks/unemployed_a1 

#unemployed for 15 to 26 weeks 
unemp_15_to_26_wks <- cps_unemployed_a1 %>% 
  subset(unempdur >= 15 & unempdur <= 26) 

from_15_to_26_wks <- sum(unemp_15_to_26_wks$cmpwgt, na.rm = TRUE)/1000
from_15_to_26_wks_percentage <- from_15_to_26_wks/unemployed_a1 

#unemployed for more than 27 weeks  
unemp_27_wks_or_more <- cps_unemployed_a1 %>% 
  subset(unempdur >= 27) 

from_27_wks <- sum(unemp_27_wks_or_more$cmpwgt, na.rm = TRUE)/1000
from_27_wks_percentage <- from_27_wks/unemployed_a1

#make excel sheet 
table_A12 <- list(Unemployed_Persons_by_Duration_of_Unemployment = 
                    c(
                      "Less than 5 weeks", 
                      "5 to 14 weeks", 
                      "15 to 26 weeks", 
                      "27 weeks and over" 
                    ),
                  Percent_Distribution = 
                    c(
                      round(less_than_5_wks_percentage*100, digits = 1), 
                      round(from_5_to_14_wks_percentage*100, digits = 1), 
                      round(from_15_to_26_wks_percentage*100, digits = 1), 
                      round(from_27_wks_percentage*100, digits = 1)
                    )) %>% 
  as.data.frame() %>% 
  mutate(Percent_Distribution 
         = as.numeric(Percent_Distribution))

write_xlsx(table_A12, "table_A12.xlsx") 
#Some of these are 0.1 off 

################################################################################
#TABLE A15: Alternative measures of labor underutilization 

#U-6: 
#Total unemployed, plus all persons marginally unattached to the labor
#force, plus total employed part time for economic reasons, as a percent of the
#civilian labor force plus all persons marginally attached to the labor force 

#Persons marginally attached to the labor force: 

#Those who are open to work and have looked in the past 12 months 
#but are not working or looking 


################################################################################

cps_unemployed_plus <- cps_07_24 %>% 
  subset(unemp == 1 | #unemployed
           discwork == 1 | #discouraged worker
           ptecon == 1 ) #part time for economic reasons  

unemployed_plus = sum(cps_unemployed_plus$cmpwgt, na.rm = TRUE)/1000

cps_employed_plus <- cps_07_24 %>% 
  subset(emp == 1 |
           discwork == 1)

employed_plus = sum(cps_employed_plus$cmpwgt, na.rm = TRUE)/1000

labor_force_plus = unemployed_plus + employed_plus

u6 <- unemployed_plus/labor_force_plus

table_A15 <- list(Alternative_Measures_of_Labor_Underutilization = 
                  c("Total unemployed, plus all persons 
                  marginally attached to the labor force, plus
                  total employed part time for economic
                  reasons, as a percent of the civilian labor
                  force plus all persons marginally attached to
                  the labor force"),
                  U6 = round(u6*100, digits = 1)) %>% 
                    as.data.frame()

write_xlsx(table_A15, "table_A15.xlsx")

#OKAY this one is wrong. I have 7.9 when it should be 8.2. I AM however 
#0.1 off from the seasonally adjusted rate. So I'm confused. 
#I think also I may be undercounting marginally attached workers, but I'm not 
#sure how else they're coded other than discouraged workers 


################################################################################
#Make excel sheet with all of the replicated tables
################################################################################

replicated_July2024_jobs_report <- list(
  "A1" = table_A1,
  "A4" = table_A4,
  "A11" = table_A11,
  "A12" = table_A12,
  "A15" = table_A15
)

write_xlsx(replicated_July2024_jobs_report, "replicated_July2024_jobs_report.xlsx")
