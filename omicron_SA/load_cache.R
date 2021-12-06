setwd("~/Progs/COVID-19/omicron_SA")

library("dplyr")
library("ggplot2")
library("shiny")
library("rsconnect")

#
# load case data from Our World In Data -> df:owid_SA  -----
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
# calculate optimal RMSE per day -> df:rmse_df ----
#

# load functions
source('functions.R')

# results holder
result = list()

# start values
R = c('delta' = 0.75, 'omicron' = 2.5)
delta_init_cases = 10000

# iterate over days
for(omicron_first_case in owid_SA$date[20:85]) {
  
  # define 9 directions
  dir_R = c(+1, +1, +1, 0, 0, 0, -1, -1, -1)
  dir_cases = c(-1, 0, +1, -1, 0, +1, -1, 0, +1)
  
  # initial step size
  factor_R = c('delta'=0.1, 'omicron'=0)
  factor_cases = 100
  
  # find best Rdelta and initial delta cases
  while(T) {
    
    cat(R['delta'], ' ', delta_init_cases, '\n')
    
    # run 9 options
    err = c()
    for(i in seq(1,9)) {
      run = sim(owid_SA, R + dir_R[i] * factor_R, delta_init_cases + dir_cases[i] * factor_cases, omicron_first_case) %>% filter(date < omicron_first_case)
      err = c(err, rmse(run$new_cases, run$sim_cases))
    }
    
    # find best value
    min_index = which.min(err)
    
    # are we done?
    if(err[5] == err[min_index]) {
      if(factor_R['delta'] <= 0.01) {
        break
      } else {
        factor_R = factor_R / 10
        factor_cases = factor_cases / 10
      }
    }
    
    # set best values for next iteration
    R = R + dir_R[min_index] * factor_R
    delta_init_cases = delta_init_cases + dir_cases[min_index] * factor_cases
    
  }
  
  # find best Romicron
  
  # define 3 directions
  dir_R = c(-1, 0, +1)
  
  # initial step size
  factor_R = c('delta'=0, 'omicron'=0.1)
  
  while(T) {
    
    cat(R['omicron'],'\n')
    
    # run 3 options
    err = c()
    for(i in seq(1,3)) {
      run = sim(owid_SA, R + dir_R[i] * factor_R, delta_init_cases, omicron_first_case)
      err = c(err, rmse(run$new_cases[!is.na(run$new_cases)], run$sim_cases[!is.na(run$new_cases)]))
    }
    
    # find best value
    min_index = which.min(err)
    
    # are we done?
    if(err[2] == err[min_index]) {
      if(factor_R['omicron'] <= 0.01) {
        break
      } else {
        factor_R = factor_R / 10
      }
    }
    
    # set best values for next iteration
    R = R + dir_R[min_index] * factor_R
    
  }
  
  cat('first Omicron case: ',as.character(omicron_first_case),
      ', initial Delta patients: ',delta_init_cases,
      ', Rdelta: ', R['delta'],
      ', Romicron: ', R['omicron'],
      ', ratio: ', round(R['omicron']/R['delta'], digits=1),
      ', RMSE: ', round(err[2]), sep='')
  
  result = append(result, list(c(first_omicron = as.character(omicron_first_case),
                                 delta_cases = delta_init_cases,
                                 R = R['delta'],
                                 R = R['omicron'],
                                 ratio = unname(R['omicron']/R['delta']),
                                 RMSE = err[2])))
  
}

rmse_df = as.data.frame(do.call(rbind, result)) %>% mutate_all(as.numeric) %>% mutate(first_omicron = as.Date(first_omicron, origin="1970-1-1"))

#
# get optimal day -> list:optimal ----
#

optimal = rmse_df %>% arrange(RMSE) %>% head(1)
optimal = as.list(optimal[1,])

#ggplot(rmse_df, aes(x=first_omicron)) +
#  geom_point(aes(y=RMSE)) + 
#  scale_x_date(date_breaks = "1 week", minor_breaks = "1 day", date_labels="%b %d") +
#  scale_y_continuous(limits=c(0,NA))

#
# save cache file -> omicron_SA.RData ----
#

save(owid_SA, rmse_df, optimal, file="omicron_SA.RData")

#
# deploy ----
#

#deployApp()