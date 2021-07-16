# load required libraries
library("dplyr")
library("tidyr")
library("lubridate")
library("readxl")
library("grid")

#
# get excess mortality data --------------------------------------------
#

counts_wide <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/excess_mortality.csv") # https://github.com/owid/covid-19-data/tree/master/public/data/

# drop unnecessary columns
counts_wide %>% select(!c(date, p_scores_all_ages:p_scores_85plus, deaths_2010_all_ages:deaths_2014_all_ages, deaths_2015_all_ages:deaths_2019_all_ages)) -> counts_wide

# wide to long
counts_long <- counts_wide %>% gather(period, deaths, deaths_2020_all_ages, deaths_2021_all_ages) %>% filter(!is.na(deaths))

# calculate start and end date
counts_long$year <- gsub("deaths_(\\d+)_all_ages","\\1", counts_long$period)

counts_long$data_start[counts_long$time_unit=="monthly"] <- as.Date(paste0(counts_long$year[counts_long$time_unit=="monthly"],"-",counts_long$time[counts_long$time_unit=="monthly"],"-1"))
counts_long$data_start[counts_long$time_unit=="weekly"] <- floor_date( as.Date( (counts_long$time[counts_long$time_unit=="weekly"]-1)*7+3, origin=paste0(counts_long$year[counts_long$time_unit=="weekly"],"-01-01")), unit="week", week_start=1)
counts_long$data_start <- as.Date(counts_long$data_start, origin = "1970-01-01")

counts_long$data_end[counts_long$time_unit=="monthly"] <- ceiling_date(counts_long$data_start[counts_long$time_unit=="monthly"], "month")-1
counts_long$data_end[counts_long$time_unit=="weekly"] <- counts_long$data_start[counts_long$time_unit=="weekly"] + 6
counts_long$data_end <- as.Date(counts_long$data_end, origin = "1970-01-01")

counts_long <- counts_long %>% select(!c(time,time_unit,period,year)) %>% relocate(c(data_start,data_end), .after=location)

# calculate excess mortality per row
counts_long$excess <- counts_long$deaths - counts_long$average_deaths_2015_2019_all_ages

# sum rows per country
excess <- counts_long %>% group_by(location) %>% summarise(excess=round(sum(excess)), data_start=min(data_start), data_end=max(data_end)) %>% relocate(excess, .after = last_col())

# cleanup
rm(counts_wide, counts_long)

#
# get population size (2020) --------------------------------------------
#

pop_file = tempfile()
download.file("https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel", pop_file)
pop <- read_excel(pop_file, skip = 2) %>% rename(population2020 = `2020`, name = "Country Name") %>% select(name, population2020)

# add population to df 'excess'
excess <- left_join(excess, pop, by=c("location"="name")) %>% relocate(population2020, .after=location)

excess$population2020[excess$location=="Czechia"] <- pop$population2020[pop$name=="Czech Republic"]
excess$population2020[excess$location=="Egypt"] <- pop$population2020[pop$name=="Egypt, Arab Rep."]
excess$population2020[excess$location=="Hong Kong"] <- pop$population2020[pop$name=="Hong Kong SAR, China"]
excess$population2020[excess$location=="Kyrgyzstan"] <- pop$population2020[pop$name=="Kyrgyz Republic"]
excess$population2020[excess$location=="Macao"] <- pop$population2020[pop$name=="Macao SAR, China"]
excess$population2020[excess$location=="Russia"] <- pop$population2020[pop$name=="Russian Federation"]
excess$population2020[excess$location=="Slovakia"] <- pop$population2020[pop$name=="Slovak Republic"]
excess$population2020[excess$location=="South Korea"] <- pop$population2020[pop$name=="Korea, Rep."]

excess$population2020[excess$location=="England & Wales"] <- 56286961 + 3152879
excess$population2020[excess$location=="French Guiana"] <- 294071
excess$population2020[excess$location=="Guadeloupe"] <- 395700
excess$population2020[excess$location=="Martinique"] <- 376480
excess$population2020[excess$location=="Mayotte"] <- 270372
excess$population2020[excess$location=="Northern Ireland"] <- 1885000
excess$population2020[excess$location=="Reunion"] <- 859959
excess$population2020[excess$location=="Scotland"] <- 5454000
excess$population2020[excess$location=="Taiwan"] <- 23570000
excess$population2020[excess$location=="Transnistria"] <- 469000

# check countries without population
excess$location[is.na(excess$population2020)]

# cleanup
rm(pop, pop_file)

#
# get covid deaths ----------------------------------------------------
#

deaths <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_deaths.csv") # https://github.com/owid/covid-19-data/blob/master/public/data/jhu/total_deaths.csv
deaths$date <- as.Date(deaths$date)

excess$confirmed <- NA
for(c in 1:nrow(excess)) {
  country <- make.names(excess[c,]$location)
  to <- excess[c,]$data_end
  official.deaths <- deaths[deaths$date==to,country]
  cat(paste(country, to, official.deaths, "\n", sep="\t"))
  if(length(official.deaths) > 0) excess$confirmed[c] <- official.deaths
}


# check countries without population
excess$location[is.na(excess$confirmed)]

# cleanup
rm(deaths, c, country, to, official.deaths)

#
# calculate data ------------------------------------------------
#

# prepare data to be displayed on Wikipedia
excess$excess100k = excess$excess / excess$population2020 * 100000
excess$confirmed100k = excess$confirmed / excess$population2020 * 100000

model <- lm(excess100k ~ confirmed100k, data = excess, weights=population2020)
summary(model)

#
# plot ---------------------------------------------------------
#

ggplot(excess, aes(confirmed100k, excess100k)) +
  geom_point(aes(size = population2020), alpha=0.6) +
  labs(title="confirmed vs excess mortality, per country, per 100k population", size="population", x="confirmed COVID-19 mortality", y="excess mortality") +
  scale_size(labels=comma) +
  
  geom_abline(intercept = 1, color = "grey") +
  annotate("text", x = 500, y = 400, label = "reference line:\nexcess = confirmed", color = "grey") +
  
  geom_abline(slope = model$coefficients[['confirmed100k']], intercept = model$coefficients[['(Intercept)']]) +
  annotate("text", x = 350, y = 575, label = paste("linear regression:\nexcess ~ confirmed, weighted by population\nY =",format(model$coefficients[['confirmed100k']]),"* X +",format(model$coefficients[['(Intercept)']])), color = "black") +
  
  annotation_custom(grobTree(textGrob(paste("generated:",Sys.Date()), x=0.01,  y=0.99, hjust=0, vjust=1, gp=gpar(col="black", fontsize=12)))) +
  theme_bw()

#
# save ----------------------------------------------------------
#

ggsave("confirmedVsExcessMortality.png", width=1200, height=600, unit="px", dpi=100)
write.csv(excess, "confirmedVsExcessMortality.csv")
