#
# prepare_data.R
#
# gathers data from RIVM, parses it into a nice dataset, then saves it as 'data.RData' for loading by app.R
#

Sys.setlocale("LC_TIME","nl_NL.UTF-8")
lastupdate = format(Sys.Date(), "%d %b %Y")

setwd("~/Progs/COVID-19/SARS-CoV-2_variantsNL")

library("tidyverse")
library("jsonlite")
library("data.table")
library("scales")
library("lubridate")
library("grid")

#
# Gather data
#

# get kiemsurveillance variant data from RIVM (df: variants)

#library("tabulizer")
#variants <- extract_tables(file = "https://www.rivm.nl/sites/default/files/2021-07/Varianten%20coronavirus%20tabel%20Nederlands%202%20juli%202021%20%282%29.pdf", method="stream")
#variants <- data.frame(variants)
#variants <- variants[,-c(2)] # remove totals column
#colnames(variants) <- variants[1,] # set colnames
#colnames(variants)[1] <- "variant" # set colname
#samplesize <- variants[2,] # store sample size in separate vector
#variants <- variants[-c(1,2),] # remove rows with sample size & titles
#variants <- melt(setDT(variants), id.vars = c("variant"), variable.name = "week") # reshape to long
#variants$value <- as.numeric(variants$value) # to numeric
#as.numeric(as.list(samplesize)[as.character(variants$week)]) -> variants$samplesize # reinsert sample size
#rm(samplesize) # remove temporary data storage
#variants$week <- gsub("/(\\d)$","/0\\1", variants$week) # fix single digit week numbers: left-pad with 0

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
variantsbackup <- variants
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
#variant_rename <- c("Alfa* (B.1.1.7) (Verenigd Koninkrijk)" = "B.1.1.7 (Alpha, UK)",
#                    "Alfa met E484K mutatie" = "B.1.1.7, mutatie E484K (Alpha)",
#                     "Beta (B.1.351) (Zuid-Afrika)" = "B.1.351 (Beta, ZA)",
#                     "Gamma (P.1) (Brazilië)" = "P.1 (Gamma, BR)",
#                     "Delta (B.1.617.2) (India)" = "B.1.617.2 (Delta, IN)",
#                     "Eta (B.1.525)" = "B.1.525 (Eta)",
#                     "Epsilon (B.1.427/4.29) (Californië)" = "B.1.427/429 (Epsilon, CA USA)",
#                     "Theta (P.3) (Filipijnen)" = "P.3 (Theta, PH)",
#                     "Kappa (B.1.617.1) (India)" = "B.1.617.1 (Kappa, IN)",
#                     "Variant Bretagne (B.1.616)" = "B.1.616 (Bretagne FR)",
#                     "Colombiaanse variant (B.1.621)" = "B.1.621 (CO)",
#                     "Lambda (C.37)" = "C.37 (Lambda)",
#                     "Iota (B.1.526)" = "B.1.526 (Iota)",
#                     "Zeta (P.2)" = "P.2 (Zeta)"
#                     )
# 
# variant_rename <- c("Alfa* (B.1.1.7)" = "Alfa (B.1.1.7)",
#                     "B.1.1.7, mutatie E484K (Alpha)" = "Alpha (B.1.1.7), mutatie E484K",
#                     "B.1.427/4.29 **" = "B.1.427/4.29",
#                     "B.1.525 (Eta)" = "Eta (B.1.525)",
#                     "Theta  P.3**" = "Theta (P.3)",
#                     "Colombian variant (B.1.621)" = "B.1.621 (Colombia)",
#                     "C.37 (Lambda)" = "Lambda (C.37)",
#                     "B.1.526 (Iota)" = "Iota (B.1.526)",
#                     "Variant Bretagne (B.1.616)" = "B.1.616 (Bretagne)")
#                     
# 
# for(i in seq(1, length(variant_rename))) {
#   variants$variant[variants$variant == names(variant_rename[i])] <- variant_rename[i]
# }; rm(i, variant_rename)

unique(variants$variant) # inspect result

# select data to save
data <- variants %>% select(!c(`95low%`,`95high%`))
all_weeks <- sort(unique(data$week))
default_selected_variants <- c("Delta (B.1.617.2)") # default variants to display in app

# create color palette
all_variants <- na.omit(sort(unique(data$variant)))
colors <- c('#00FFFF', '#00FFFF', '#00FF89', '#00F700', '#B4D500', '#F8AE00', '#FF7D00', '#FF297C', '#FF00E2', '#FF00FF', '#9D35FF', '#0097FF', '#00ADE7', '#00A485', '#009000', '#007A00', '#526100', '#814300', '#9C0000', '#B8001F')
colors <- colors[c(seq(1,length(all_variants)))]
names(colors) <- all_variants

# save
save(lastupdate, data, all_weeks, all_variants, colors, default_selected_variants, file="data.RData")
