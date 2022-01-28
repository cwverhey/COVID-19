library("dplyr")
library("stringr")
library("tidyverse")
library("lubridate")
library("cbsodataR")
library("scales")
library("svglite")
library("rsconnect")

# set script directory as working directory for cronjob (non-interactive Rscript call)
cmdargs = commandArgs()
for(arg in cmdargs){
  if(substring(arg,1,7)=="--file=") {
    dirname = dirname(substring(arg,8))
    cat("setwd('",dirname,"')\n", sep='')
    setwd(dirname)
  }
}; rm(cmdargs, arg, dirname)

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
  select(first_day_of_week, age=Age_group, hosp=Hospital_admission, ic=IC_admission) %>%
  mutate(age = NICE_agegroups[age]) %>%
  group_by(first_day_of_week, age) %>%
  summarise(hosp=sum(hosp), ic=sum(ic))

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
  summarise(age = "<50", hosp = sum(hosp), ic = sum(ic), cases = sum(cases))

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
rm(deaths)


#
# PREPARE DATA FOR GRAPHING -----------------------------------------------
#

# convert weekly data to long ---------------------------------------------
# into `all_weekly_long`

all_weekly_long = all_weekly %>% pivot_longer(c(hosp, ic, cases, deaths), names_to = 'data')

# set order of data types
all_weekly_long$data = factor(all_weekly_long$data, levels=c("cases","hosp","ic","deaths"))

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
# PLOTS -------------------------------------------------------------------------------
#

# overall plot ------------------------------------------------------------

plot <- ggplot(cases_relative_monthly, aes(date, age, fill=percentage)) + 
  geom_tile(colour="gray20", size=1.5, stat="identity") +
  geom_text(aes(label=paste0(sprintf(percentage, fmt = '%#.1f'),'%'), color=percentage), show.legend = F, size = 4.5) + 
  
  scale_fill_viridis_c(option = "inferno", begin=0.05, end=0.85) +
  scale_color_viridis_c(option = "inferno", begin=0.55) +
  labs(title="Vastgestelde besmettingen", subtitle=paste0("als percentage van de gehele leeftijdsgroep (update: ",format(Sys.Date(),"%Y-%m-%d"),")"), x="maand", y="leeftijd") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    text = element_text(size = 20, color="white"),
    
    plot.subtitle = element_text(color="white",hjust=0, margin=margin(5,0,10,0), vjust=1, size=rel(0.8)),
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(fill="gray20"),
    
    panel.background = element_rect(fill="gray30"),
    #panel.grid.major = element_line(color="black"),
    #panel.grid.minor = element_line(color="gray10"),
    panel.spacing = unit(1, "lines"),
    
    axis.text    = element_text(color="white"),
    axis.text.x  = element_text(angle=45, hjust=1, margin=margin(5,0,10,0)),
    axis.text.y  = element_text(hjust=1, margin=margin(0,5,0,10)),
    
    legend.position = "none"
  )

svg("www/plots/cases.svg", width=20, height=10)
print(plot)
dev.off()


# age-group plot ----------------------------------------------------------

# function to format date-axis labels
mklab = function(dates) {
  
  retval = c()
  lastyear = ""
  
  for(i in seq_along(dates)) {
    
    d = dates[i]
    
    if(is.na(d)) {
      retval = c(retval, NA)
      next
    }
    
    # year
    if(format(d,"%Y ") != lastyear)
      str = lastyear = format(d,"%Y ")
    else
      str = ""
    
    # begin date
    str = paste0(str, format(d,"%b %d"), "-")
    
    # end date
    if(format(d,"%m") == format(d+6,"%m"))
      str = paste0(str, format(d+6,"%d"))
    else
      str = paste0(str, format(d+6,"%b %d"))
    
    retval = c(retval, str)
    
  }
  
  return(retval)
  
}

# age groups to render an individual plot for
all_ages = c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+","<50")

for (agegr in all_ages) {

    if(agegr %in% c("0-19","20-29","30-39","40-49"))
      df_plot = filter(all_weekly_long, age == agegr, data != "deaths")
    else
      df_plot = filter(all_weekly_long, age == agegr)
    
    plot = ggplot(df_plot, aes(x=first_day_of_week, y = value)) +
      geom_vline(xintercept = as.Date(cut(Sys.Date(), "week")), color='red3', size=1.25) +
      geom_bar(color="grey50", fill="white", stat="identity") +
      facet_grid(data ~ ., scales = "free_y", labeller = labeller(data = c("cases"="besmet","hosp"="opnames totaal","ic"="opnames IC","deaths"="overleden"))) +
      labs(title=paste("Leeftijd",agegr), x = "week", y = "aantal") +
      scale_x_date(labels = mklab, date_breaks = "4 weeks", date_minor_breaks = "1 week", expand=c(0,0)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 3), labels = label_number(accuracy=1)) +
      theme(
        text = element_text(size = 20, color="white"),
        plot.margin = margin(30,30,30,30),
        plot.background = element_rect(fill="gray20"),
        panel.background = element_rect(fill="gray30"),
        panel.grid.major = element_line(color="black"),
        panel.grid.minor = element_line(color="gray10"),
        axis.text    = element_text(color="white"),
        axis.text.x  = element_text(angle=45, hjust=1, margin=margin(5,0,0,0)),
        axis.text.y  = element_text(hjust=1, margin=margin(0,5,0,0)),
        legend.position = "none",
        panel.spacing = unit(1, "lines")
      )
    
    svg(paste0("www/plots/detail_",make.names(agegr),".svg"), width=20, height=10)
    print(plot)
    dev.off()
  
}

#
# SAVE DATA ---------------------------------------------------------------
#

save(all_weekly_long, cases_relative_monthly, file="NLbyAge.RData")
deployApp(account="cwverhey", server="shinyapps.io")
