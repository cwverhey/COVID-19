library("dplyr")
library("tidyverse")
library("ggplot2")
library("scales")

# get excess mortality data --------------------------------------------
# https://github.com/owid/covid-19-data/tree/master/public/data/

excess_mortality = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/excess_mortality.csv") %>%
                        mutate(date = as.Date(date))

# drop unnecessary columns
excess_mortality = excess_mortality %>%
                    select(location, date, cum_excess_per_million_proj_all_ages)

# get latest row per country
excess_mortality = excess_mortality %>%
                    group_by(location) %>%
                    filter(!is.na(cum_excess_per_million_proj_all_ages)) %>%
                    arrange(desc(date)) %>%
                    slice(1)


# load COVID mortality data -----------------------------------------------
# takes ~15s

owid_full = read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>%
  mutate(date = as.Date(date))

# filter
covid_mortality = owid_full %>%
                    select(location, date, total_deaths_per_million, iso_code, continent, population) %>%
                    filter(!is.na(total_deaths_per_million))


# merge covid_mortality and excess_mortality ------------------------------

df = left_join(covid_mortality, excess_mortality, by = 'location', suffix=c('','.y')) %>%
      filter(!is.na(date.y)) %>%
      mutate(datediff = abs(date-date.y)) %>%
      group_by(location) %>%
      arrange(datediff) %>%
      slice(1) %>%
      filter(datediff < 7) %>%
      mutate(cum_covid_deaths_per_100k = total_deaths_per_million/10, cum_excess_deaths_per_100k = cum_excess_per_million_proj_all_ages/10) %>%
      select(location, date, cum_covid_deaths_per_100k, cum_excess_deaths_per_100k, iso_code, population, continent)


# linear model ------------------------------------------------------------

model <- lm(cum_excess_deaths_per_100k ~ cum_covid_deaths_per_100k, data = df, weights=population)
summary(model)

# plot --------------------------------------------------------------------

ggplot(df, aes(x=cum_covid_deaths_per_100k, y=cum_excess_deaths_per_100k, color=continent, size=population)) +
  
  labs(title="confirmed COVID mortality vs overall excess mortality, per country, per 100k population",
       x="confirmed COVID-19 mortality",
       y="excess mortality",
       caption=paste0("linear regression (weighted by population): \nexcess_per100k = ",format(model$coefficients[['cum_covid_deaths_per_100k']])," * covid_deaths_per100k + ",format(model$coefficients[['(Intercept)']]),
                     "\n\nplot generated:\n", Sys.Date())) +
  scale_size(labels=comma) +
  
  geom_abline(intercept = 1, color = "grey") +
  annotate("text", x = 500, y = 400, label = "reference line:\nexcess = covid_deaths", color = "grey") +
  
  geom_abline(slope = model$coefficients[['cum_covid_deaths_per_100k']], intercept = model$coefficients[['(Intercept)']], color = "darkgrey") +
  annotate("text", x = 450, y = 675, label = "linear regression", color = "darkgrey") +
  
  geom_point(alpha=0.6) +
  
  theme_bw() +
  theme(plot.caption=element_text(hjust = 0))


# save --------------------------------------------------------------------

ggsave("confirmedVsExcessMortality.png", width=1200, height=600, unit="px", dpi=100)
write.csv(df, "confirmedVsExcessMortality.csv")
