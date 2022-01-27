library("dplyr")
library("stringr")
library("tidyverse")
library("lubridate")
library("cbsodataR")

#
# GET AND MERGE DATA ------------------------------------------------------
#

# load covid case data -----------------------------------------------------
# int df `osiris` (full data) and df `cases` (selection we'll use)
# takes ± 2 min

# https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/2c4357c8-76e4-4662-9574-1deb8a73f724
options(timeout = max(600, getOption("timeout")))
osiris = read.csv2("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv")

cases = osiris %>%
  select(date = Date_statistics, age = Agegroup, dead = Deceased) %>%
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
  group_by(date, age, dead) %>%
  summarise(count = n())


# load demographic data ---------------------------------------------------
# into df `demographics`

# define months to retrieve
Perioden = seq.Date(as.Date("2020-01-01"), Sys.Date(), by="month")

# retrieve data from CBS
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83482NED/table?dl=60959
demographics = cbs_get_data(id = '83482NED',
                            Geslacht = "T001038",
                            Leeftijd = c("70100", "70200", "70300", "70400", "70500", "70600", "70700", "70800", "70900", "71000", "71100", "71200", "71300", "71400", "71500", "71600", "71700", "71800", "71900", "72000", "22200"),
                            Migratieachtergrond = "T001040",
                            Generatie = "T001040",
                            Perioden = format(Perioden,"%YMM%m"),
                            select = c("Leeftijd", "Perioden", "BevolkingOpDeEersteVanDeMaand_1")
) %>%
  cbs_add_label_columns() %>%
  select(age = Leeftijd_label, date = Perioden, population = BevolkingOpDeEersteVanDeMaand_1)

# convert date format
demographics$date = recode(demographics$date, !!!setNames(format(Perioden,"%Y-%m"),format(Perioden,"%YMM%m")))

# merge age groups
new = c()
old = c()
for(i in 0:8) {
  new = c(new, rep(paste(i*10, i*10+9, sep='-'), 2))
  old = c(old, paste(i*10,'tot',i*10+5,'jaar'), paste(i*10+5,'tot',i*10+10,'jaar'))
}
new = c(new, rep("90+", 3))
old = c(old, "90 tot 95 jaar", "95 tot 100 jaar", "100 jaar of ouder")

demographics$age = recode(demographics$age, !!!setNames(new,old))
demographics = demographics %>% group_by(age, date) %>% summarise(population = sum(population))

# add <50 age group
groups = c("<50", "0-9", "10-19", "20-29", "30-39", "40-49")
demographics = demographics %>% filter(age %in% groups) %>% group_by(date) %>% summarise(age = '<50', population = sum(population)) %>% bind_rows(demographics)

# complete dataset for non-present months
missingmonths = setdiff( unique(format(cases$date,"%Y-%m")), demographics$date)
lastmonth = demographics %>% filter(date == max(demographics$date))
for (m in missingmonths) {
  demographics = lastmonth %>% mutate(date = m) %>% bind_rows(demographics)
}

# clean up
rm(groups, i, new, old, Perioden, missingmonths, m, lastmonth)


# load hospitalization data ------------------------------------------
# into df `all_weekly`

NICE_agegroups = c("0-14" = "0-19", "15-19" = "0-19", "90+" = "90+", "Unknown" = "Unknown")
for(i in 2:8) {
  from1 = paste0(i,'0-',i,'4')
  from2 = paste0(i,'5-',i,'9')
  to    = paste0(i,'0-',i,'9')
  NICE_agegroups[from1] = to
  NICE_agegroups[from2] = to
}; rm(i, from1, from2, to)

# fetch hospitalization per week per NICE-agegroup
all_weekly = read.csv2("https://data.rivm.nl/covid-19/COVID-19_ziekenhuis_ic_opnames_per_leeftijdsgroep.csv") %>%
  mutate(first_day_of_week = as.Date(Date_of_statistics_week_start)) %>%
  select(first_day_of_week, age=Age_group, hosp_not=Hospital_admission_notification, ic_not=IC_admission_notification) %>%
  mutate(age = NICE_agegroups[age]) %>%
  group_by(first_day_of_week, age) %>%
  summarise(hosp_not=sum(hosp_not), ic_not=sum(ic_not))

rm(NICE_agegroups)


# add total cases to weekly data -------------------------------------
# add to df `all_weekly`

# get cases from `cases` by NICE agegroup and by first day of the week
cases_NICE = cases %>%
  select(date, age, count) %>%
  mutate(age = replace(age, age == "0-9", "0-19"), age = replace(age, age == "10-19", "0-19"), first_day_of_week = as.Date(cut(date, "week"))) %>%
  group_by(first_day_of_week, age) %>%
  summarise(count = sum(count))

# merge total cases
all_weekly = left_join(all_weekly, cases_NICE) %>%
  rename(cases=count) %>%
  replace_na(list(cases=0))

# add <50 group
all_50min_temp = all_weekly %>%
  filter(age %in% c("0-19","20-29","30-39","40-49")) %>%
  group_by(first_day_of_week) %>%
  summarise(age = "<50", hosp_not = sum(hosp_not), ic_not = sum(ic_not), cases = sum(cases))

all_weekly = rbind(all_weekly, all_50min_temp)

# cleanup
rm(cases_NICE, all_50min_temp)


# add deaths to weekly data ------------------------------------------
# add to df `all_weekly`

# get cases from `cases` by NICE age group and by first day of the week
deaths = cases %>%
  filter(dead == "Yes") %>%
  select(date, age, count) %>%
  mutate(first_day_of_week = as.Date(cut(date, "week"))) %>%
  group_by(first_day_of_week, age) %>%
  summarise(count = sum(count))

# merge deaths
all_weekly = left_join(all_weekly, deaths) %>%
  rename(deaths=count) %>%
  replace_na(list(deaths=0))

# cleanup
rm(deaths, agegroups)


#
# PREPARE DATA FOR GRAPHING -----------------------------------------------
#

# convert weekly data to long ---------------------------------------------
# into `all_weekly_long`

all_weekly_long = all_weekly %>% pivot_longer(c(hosp_not, ic_not, cases, deaths), names_to = 'data')

# set order of data types
all_weekly_long$data = factor(all_weekly_long$data, levels=c("cases","hosp_not","ic_not","deaths"))

# create legible date labels
all_weekly_long$date_label = paste(format(all_weekly_long$first_day_of_week,"%d-%m"), "tot", format(all_weekly_long$first_day_of_week+6,"%d-%m-'%y"))

# remove deaths for <50
all_weekly_long = all_weekly_long %>% filter(!(age %in% c("0-19","20-29","30-39","40-49") & data == 'deaths'))


# aggregate to relative cases per agegroup per month -------------------------------
# into df `cases_relative_monthly`
# for plot 'vastgestelde besmettingen als % van de bevolking per leeftijdsgroep'

cases_relative_monthly = cases %>%
  filter(age != "Unknown", age != "<50") %>%
  mutate(date = format(date, "%Y-%m")) %>%
  group_by(date, age) %>%
  summarise(count = sum(count))

# calculate % cases omitted
cat(paste0("missing cases: ",100*(1 - sum(cases_relative_monthly$count) / sum(cases$count)),"%"))

# complete dataset with 0 for non-present groups
for(date in unique(cases_relative_monthly$date))
  for(age in unique(cases_relative_monthly$age))
    if(0 == sum(cases_relative_monthly$age == age & cases_relative_monthly$date ==  date))
      cases_relative_monthly = rbind(cases_relative_monthly, list(date=date, age=age, count=0))

# add demographic data
cases_relative_monthly = left_join(cases_relative_monthly, demographics)

# calculate proportion
cases_relative_monthly$percentage = cases_relative_monthly$count / cases_relative_monthly$population * 100

# cleanup
rm(age, date)


#
# SAVE --------------------------------------------------------------------
#

save(all_weekly_long, cases_relative_monthly, file="NLbyAge.RData")