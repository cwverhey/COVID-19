
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
  
  # initial cases
  cases = c('delta' = delta_init_cases, 'omicron' = 0)
  
  # iterate over all days
  for(d in sort(owid_SA$date)) {
    
    # add first patient with Omicron at specified date
    if(d == omicron_first_case) cases['omicron'] = 1
    
    # save total cases for this date
    owid_SA$sim_cases[owid_SA$date == d] = sum(cases)
    owid_SA$sim_delta[owid_SA$date == d] = cases['delta']
    owid_SA$sim_omicron[owid_SA$date == d] = cases['omicron']
    
    # calculate cases for next day
    cases = cases * r_day
    
  }
  
  return(owid_SA)
  
}

# results holder
result = list()

# start values
R = c('delta' = 0.75, 'omicron' = 2.5)
delta_init_cases = 10000

# iterate over days
for(omicron_first_case in tail(owid_SA$date,-19)) {
  
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
                                 Rdelta = R['delta'],
                                 Romicron = R['omicron'],
                                 ratio = R['omicron']/R['delta'],
                                 RMSE = err[2])))

}

results = as.data.frame(do.call(rbind, result)) %>% mutate_all(as.numeric) %>% mutate(first_omicron = as.Date(first_omicron, origin="1970-1-1"))
ggplot(results, aes(x=first_omicron)) + geom_point(aes(y=RMSE))
