#Make UI column figures 
#Chiara Chanoi

rm(list = ls())

library(readr)

#set working directory 
setwd("/Users/Cchanoi/Desktop/UI/UI_Column/")

#download dataset
euc <- read_csv("ETA/ETA5159/ETA5159_data/ETA5159_EUC_1991_1994.csv")

source("saturdays_per_month.R")

euc <- get_saturdays_per_month(euc)