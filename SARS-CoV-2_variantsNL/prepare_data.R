#
# prepare_data.R
#
# gathers data from RIVM, parses it into a nice dataset, then saves it as 'data.RData' for loading by app.R
#

Sys.setlocale("LC_TIME","nl_NL.UTF-8")
lastupdate = format(Sys.Date(), "%d %b %Y")

setwd("~/R/COVID-19/SARS-CoV-2_variantsNL")

# install.packages(c("dplyr","jsonlite","data.table","scales","lubridate","stringr","rsconnect",'ggplot2', 'shiny', 'shinyWidgets'))

library("dplyr")
library("jsonlite")
library("data.table")
library("scales")
library("lubridate")
library("stringr")
library("rsconnect")

#
# Gather data
#

# get kiemsurveillance variant data from RIVM (df: variants)

# https://www.rivm.nl/coronavirus-covid-19/virus/varianten
# https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/4678ae0b-2580-4cdb-a50b-d229575269ae
kiemdata <- "https://data.rivm.nl/covid-19/COVID-19_varianten.csv"
variants <- read.csv2(kiemdata) %>%
              mutate(variant = ifelse(Variant_name == '', Variant_code, paste0(Variant_name, " (",Variant_code,")")),
                     week = gsub("/(\\d)$","/0\\1", paste0(isoyear(Date_of_statistics_week_start),'/',isoweek(Date_of_statistics_week_start))), # yyyy/ww (left pad week with 0)
                     value = Variant_cases,
                     samplesize = Sample_size) %>%
              select(variant, week, value, samplesize)

# get # of cases per week (temporary df: cases)
cases <- read.csv2("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv")
cases <- cases %>% group_by(Date_of_report) %>% summarise(cumsum=sum(Total_reported))
cases$week <- strftime(cases$Date_of_report, format = "%G/%V")
cases$cases <- c(NA, diff(cases$cumsum))
cases <- cases %>% group_by(week) %>% summarise(cases = sum(cases)) %>% filter(week >= min(variants$week))

# add # of cases to variant df
variants <- full_join(variants,cases,by="week")
rm(cases)

# calculate share of all kiemsurveillance samples per variant per week
variants$percentage <- variants$value / variants$samplesize

# extrapolate share in kiemsurveillance to share in all cases
# binomial estimate of total population + 95% CI
for(i in seq(1:nrow(variants))) {
  print(i)
  if(!is.na(variants[i,"value"])) {
    binom <- binom.test(as.numeric(variants[i,"value"]), as.numeric(variants[i,"samplesize"]))
    
    variants[i,"95low%"]   = binom$conf.int[1]
    variants[i,"95high%"]  = binom$conf.int[2]
    
    variants[i,"estimate"] = round(binom$estimate * variants[i,"cases"])
    variants[i,"95low"]    = ceiling(binom$conf.int[1] * variants[i,"cases"])
    variants[i,"95high"]   = floor(binom$conf.int[2] * variants[i,"cases"])
  }
}; rm(i, binom)

# fix up the variant names
variants$variant <- str_replace(variants$variant, "(.*) \\((.*)\\)", "\\2 \\(\\1\\)")
unique(variants$variant) # inspect result

# select data to save
data <- variants %>% select(!c(`95low%`,`95high%`))
all_weeks <- sort(unique(data$week))
all_variants <- na.omit(sort(unique(data$variant)))
default_selected_variants <- c(na.omit(str_match(all_variants, '.* \\(.*\\)'))) # default variants to display in app

# create color palette
colors <- c('#00FFFF', '#00FFFF', '#00FF89', '#00F700', '#B4D500', '#F8AE00', '#FF7D00', '#FF297C', '#FF00E2', '#FF00FF', '#9D35FF', '#0097FF', '#00ADE7', '#00A485', '#009000', '#007A00', '#526100', '#814300', '#9C0000', '#B8001F')
colors <- colors[c(seq(1,length(all_variants)))]
names(colors) <- all_variants

# save
save(lastupdate, data, all_weeks, all_variants, colors, default_selected_variants, file="data.RData")

# upload to shinyapps.io
# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# rsconnect::configureApp("SARS-CoV-2_variantsNL")
deployApp()
