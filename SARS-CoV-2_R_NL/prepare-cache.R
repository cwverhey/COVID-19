library("dplyr")
library("shiny")
library("shinyWidgets")
library("shinyjs")
library("ggplot2")
library("scales")
library("rsconnect")

setwd("~/R/COVID-19/SARS-CoV-2_R_NL")

Sys.setlocale("LC_TIME","nl_NL.UTF-8")

# source files need to be in <jsondir>/reproductiegetal_*.json
jsondir = '~/R/RIVM'

# cache file
cachefile = 'R.app.cache.RData'

# load JSONs retrieved every day from https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/ed0699d1-c9d5-4436-8517-27eb993eab6e
file.ls <- list.files(path=jsondir, pattern=glob2rx("reproductiegetal_*.json"), full.names=T)

if(length(file.ls) == 0) stop("Error: no input files!")

# get data
df <- data.frame()
lastdf <- 0
for(f in file.ls) {
  print(f)
  date <- gsub('.*(\\d{4}-\\d{2}-\\d{2}).*', '\\1', f)
  print(date)
  df.temp <- jsonlite::read_json(f, simplifyDataFrame=T)
  df.temp$Rt_low <- as.numeric(df.temp$Rt_low)
  df.temp$Rt_avg <- as.numeric(df.temp$Rt_avg)
  df.temp$Rt_up  <- as.numeric(df.temp$Rt_up)
  df.temp$source <- as.Date(date)
  if(nrow(df.temp) != lastdf) {
    df <- bind_rows(df, df.temp)
    lastdf <- nrow(df.temp)
  }
}
rm(f, date, df.temp)

df %>%
  select(source, Date, Rt_low, Rt_avg, Rt_up) %>%
  rename(date=Date) %>%
  mutate(source = as.Date(source), date = as.Date(date)) -> df

print(head(df))

# list unique source / dates
all.sources = unique(df$source)
all.dates = unique(df$date)

# disease mitigation dates
# https://nl.wikipedia.org/wiki/Maatregelen_tijdens_de_coronacrisis_in_Nederland
mitigation.dates = c("2020-10-14", "2020-11-03", "2020-12-14", "2021-01-20", "2021-04-20", "2021-05-11", "2021-06-05", "2021-06-26", "2021-07-09")

save(df, colors, all.sources, all.dates, mitigation.dates, file="R.app.cache.RData")
print("saved")

# upload to shinyapps.io
# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# rsconnect::configureApp("SARS-CoV-2_variantsNL")
deployApp(appName='R_NL')