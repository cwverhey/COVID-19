
#
# find locally optimal model parameters
#

load("owid_SA.RData")

rmse = function(v1, v2) {
  sqrt(mean((v1 - v2)^2))
}

sim = function(R, delta_init_cases, omicron_first_case) {
  
  # daily growth
  r_day = R^(1/5)
  
  # first day
  day1 = min(owid_SA$date)
  
  # delta
  owid_SA$sim_delta = delta_init_cases * r_day['delta'] ^ as.numeric(owid_SA$date - day1)
  
  # omicron
  owid_SA$sim_omicron = r_day['omicron'] ^ as.numeric(owid_SA$date - omicron_first_case)
  owid_SA$sim_omicron[owid_SA$date < omicron_first_case] = 0
  
  # total
  owid_SA$sim_cases = owid_SA$sim_delta + owid_SA$sim_omicron
  
  return(owid_SA)
  
}

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
      run = sim(R + dir_R[i] * factor_R, delta_init_cases + dir_cases[i] * factor_cases, omicron_first_case) %>% filter(date < omicron_first_case)
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
      run = sim(R + dir_R[i] * factor_R, delta_init_cases, omicron_first_case)
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

results = as.data.frame(do.call(rbind, result)) %>% mutate_all(as.numeric) %>% mutate(first_omicron = as.Date(first_omicron, origin="1970-1-1"))
ggplot(results, aes(x=first_omicron)) +
  geom_point(aes(y=RMSE)) + 
  scale_x_date(date_breaks = "1 week", minor_breaks = "1 day", date_labels="%b %d") +
  scale_y_continuous(limits=c(0,NA))

rmse_df = results
save(rmse_df, file='owid_SA_rmse.RData')
