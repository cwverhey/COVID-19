sim = function(df, R, delta_init_cases, omicron_first_case) {
  
  # daily growth
  r_day = R^(1/5)
  
  # first day
  day1 = min(df$date)
  
  # delta
  df$sim_delta = delta_init_cases * (r_day['delta'] ^ as.numeric(df$date - day1))
  
  # omicron
  df$sim_omicron = r_day['omicron'] ^ as.numeric(df$date - omicron_first_case)
  df$sim_omicron[df$date < omicron_first_case] = 0
  
  # total
  df$sim_cases = df$sim_delta + df$sim_omicron
  
  return(df)
  
}

rmse = function(v1, v2) {
  sqrt(mean((v1 - v2)^2))
}

mae = function(v1, v2) {
  mean(abs(v1-v2))
}