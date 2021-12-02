library("dplyr")
library("ggplot2")
library("shiny")
library("rsconnect")

setwd("~/R/COVID-19/omicron_SA")

#
# load case data from Our World In Data -----
#
# https://ourworldindata.org/covid-cases
#
owid_full = read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>%
  mutate(date = as.Date(date))

owid_SA = owid_full %>%
  filter(location == 'South Africa', date >= "2021-08-26") %>%
  select(date, new_cases, new_cases_smoothed)

save(owid_SA, file="owid_SA.RData")

#
# calculate optimized RMSE
#
source('find_optimum.R')

deployApp()