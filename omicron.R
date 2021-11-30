#
# load case data from Our World In Data -----
#
# https://ourworldindata.org/covid-cases
#
owid_full = read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>%
              mutate(date = as.Date(date))

owid_SA = owid_full %>%
            filter(location == 'South Africa', date >= "2021-09-01") %>%
            select(date, new_cases, new_cases_smoothed)

#
# add simulated data ----
#
# on 1 Sept 2021: start Delta variant at 9000 cases
# on 1 Okt 2021:  start Omicron variant at 1 case
#
# daily change: Delta cases * 0.944, Omicron cases * 1.143 (5-day change: delta * 0.75, omicron * 1.95)
#
# (values are selected manually)

# growth rate per day, per variant (R ≈ daily r ^ 5)
r_day = c( 0.75^(1/5), 1.95^(1/5) )

# initial cases on simulation day 1 (1 sep 2021)
cases = c(9000, 0)

# iterate over all days
for(d in sort(owid_SA$date)) {
  
  # add first patient with Omicron at specified date
  if(d == as.Date("2021-10-01")) cases[2] = 1

  # print date and cases per variant
  cat(as.character(as.Date(d, origin="1970-01-01")))
  cat(' ', cases, '\n')
  
  # save total cases for this date
  owid_SA$sim_cases[owid_SA$date == d] = sum(cases)
  
  # calculate cases for next day
  cases = cases * r_day
 
}; rm(d, cases, r_day)

#
# plot ----
#
# plot cases from OurWorldInData and simulation
#

colors = c("simulated" = "blue", "cases (raw)" = 'black', "cases (OWiD smoothed)" = 'darkgreen', "cases (geom_smooth())" = 'grey')
ggplot(owid_SA, aes(x = date)) +
  geom_smooth(aes(y=new_cases, color="cases (geom_smooth())"), lty=3) +
  geom_line(aes(y=new_cases_smoothed, color="cases (OWiD smoothed)"), lwd=.75) +
  geom_point(aes(y=new_cases, color="cases (raw)"), cex=.75, lwd=0.1) +
  geom_line(aes(y=predict_cases, color="simulated"), lwd=1) +
  #scale_y_continuous(trans='log10') +
  labs(x = 'day', y = 'new cases', title='SA cases per day', color='') +
  scale_color_manual(values = colors)
  