library(ciisr)
library(covidr)
library(dbplyr)
library(ggplot2)
library(googlesheets4)
library(jsonlite)
library(lubridate)
library(magrittr)
library(openxlsx)
library(readr)
library(readxl)
library(RSocrata)
library(scales)
library(stringi)
library(tidyverse)

# 7-day adms are counted Wednesday-Tuesday
#https://covid.cdc.gov/covid-data-tracker/#county-view?list_select_state=all_states&list_select_county=all_counties&data-type=CommunityLevels

# set wd to the folder this code lives in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

seven_day_total <- function(x){
  (lag(x,6) + lag(x,5) + lag(x,4) + lag(x,3) + lag(x,2) + lag(x,1) + x)}

connect("covid19", server = "DPHE144")

# get county populations ------------------------------
county_pop <- tbl(conn, in_schema("dbo", "populations")) %>% 
  filter(metric == "county",
         year == 2020) %>% 
  select(group, population) %>% 
  collect()

county_pop$group <- stri_trans_totitle(county_pop$group)

# HHS Hospitalizations -------------------------------
# https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-Community-Levels-by-County/3nnm-4jni

hhs <- read.socrata("https://data.cdc.gov/resource/3nnm-4jni.json")

hosp0 <- hhs %>% 
  filter(state == "Colorado",
         date_updated == max(date_updated)) %>% 
  select(county, covid_hospital_admissions_per_100k, date_updated) %>%
  mutate(county = gsub(" County.*", "", county),
         covid_hospital_admissions_per_100k = as.numeric(covid_hospital_admissions_per_100k))

updated <- max(hosp0$date_updated)

county_hosp_rank <- ggplot(hosp0, aes(x = reorder(county, covid_hospital_admissions_per_100k), 
                                     y = covid_hospital_admissions_per_100k)) + 
  geom_bar(stat = "identity", 
           color = "grey",
           fill = "grey",
           width = .8) + 
  scale_y_continuous(limits = c(0,max(hosp0$covid_hospital_admissions_per_100k + 1))) +
  coord_flip() +
  geom_text(aes(label = covid_hospital_admissions_per_100k),  
            hjust = -0.5, vjust = 0.5, size = 2.5) + 
  labs(y = "7-Day Cumulative Hospitalization Admissions per 100,000", x= "") + 
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        title = element_text(size = 8),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 10), 
        axis.text.x = element_text(size = 12)) + 
  scale_color_identity()  +
  labs(caption = paste0("Source: US Dept. of Health and Human Services. Data updated ", format(updated, "%b %d, %Y")))

ggsave("~/../Downloads/county_hosp_adm.png", width = 6, height = 10)

# HHSProtect Teletracker Data ---------------------------
tele <- read.csv("Teletracker/2022-06-07 HHSProtect Teletracker.csv") %>% 
  select(entry_date,
         admits_last_24_hrs_covid_admissions_confirmed_adult,
         admits_last_24_hrs_covid_admissions_confirmed_pediatric,
         hospital_county) %>% 
  mutate(entry_date = ymd(entry_date)) %>% 
  mutate(adms = as.numeric(admits_last_24_hrs_covid_admissions_confirmed_adult) + 
                as.numeric(admits_last_24_hrs_covid_admissions_confirmed_pediatric),
         hospital_county = gsub(" County", "", hospital_county)) %>% 
  filter(entry_date >= "2022-06-01",
         entry_date <= "2022-06-07") %>% 
  group_by(hospital_county) %>% 
  summarize(tot_adms = sum(adms)) %>% 
  left_join(county_pop, by = c("hospital_county" = "group")) %>% 
  mutate(cumul_adm_per_100k = round(100000*tot_adms/population, 1)) %>% 
  ungroup() %>% 
  select(hospital_county, cumul_adm_per_100k) %>% 
  distinct() %>% 
  arrange(hospital_county) %>% 
  rename(County = hospital_county,
         `Cumulative Admissions Per 100k` = cumul_adm_per_100k)


hsa <- read.csv("Teletracker/2022-06-07 HHSProtect Teletracker.csv") %>% 
  select(entry_date,
         admits_last_24_hrs_covid_admissions_confirmed_adult,
         admits_last_24_hrs_covid_admissions_confirmed_pediatric,
         hospital_county) %>% 
  mutate(entry_date = ymd(entry_date)) %>% 
  mutate(adms = as.numeric(admits_last_24_hrs_covid_admissions_confirmed_adult) + 
                as.numeric(admits_last_24_hrs_covid_admissions_confirmed_pediatric),
         hospital_county = gsub(" County", "", hospital_county)) %>% 
  filter(entry_date >= "2022-06-01",
         entry_date <= "2022-06-07") %>% 
  select(hospital_county, adms) %>% 
  mutate(hsa_cdc = case_when(hospital_county == "Alamosa" |
                             hospital_county == "Conejos" |
                             hospital_county == "Costilla" |
                             hospital_county == "Mineral" |
                             hospital_county == "Rio Grande" |
                             hospital_county == "Saguache" ~ 731,
                           
                           hospital_county == "Jackson"  ~ 771,
                           
                           hospital_county == "Boulder" |
                             hospital_county == "Broomfield" ~ 795,
                           
                           hospital_county == "Chaffee" |
                             hospital_county == "Lake" ~ 786,
                           
                           hospital_county == "Adams" |
                             hospital_county == "Arapahoe" |
                             hospital_county == "Clear Creek" |
                             hospital_county == "Denver" |
                             hospital_county == "Douglas" |
                             hospital_county == "Elbert" |
                             hospital_county == "Gilpin" |
                             hospital_county == "Grand" |
                             hospital_county == "Jefferson" |
                             hospital_county == "Park" |
                             hospital_county == "Summit" ~ 688,
                           
                           hospital_county == "Cheyenne" |
                             hospital_county == "El Paso" |
                             hospital_county == "Kit Carson" |
                             hospital_county == "Lincoln" |
                             hospital_county == "Teller" ~ 754,
                           
                           hospital_county == "Custer" |
                             hospital_county == "Fremont" ~ 812,
                           
                           hospital_county == "Baca" ~ 562,
                           
                           hospital_county == "Larimer" ~ 796,
                           
                           hospital_county == "Logan" |
                             hospital_county == "Phillips" |
                             hospital_county == "Sedgwick" ~ 763,
                           
                           hospital_county == "Eagle" |
                             hospital_county == "Garfield" |
                             hospital_county == "Mesa" |
                             hospital_county == "Pitkin" |
                             hospital_county == "Rio Blanco" ~ 711,
                           
                           hospital_county == "Delta" |
                             hospital_county == "Gunnison" |
                             hospital_county == "Hinsdale" |
                             hospital_county == "Montrose" |
                             hospital_county == "Ouray" |
                             hospital_county == "San Miguel" ~ 761,
                           
                           hospital_county == "Bent" |
                             hospital_county == "Crowley" |
                             hospital_county == "Kiowa" |
                             hospital_county == "Otero" |
                             hospital_county == "Prowers" ~ 745,
                           
                           hospital_county == "Huerfano" |
                             hospital_county == "Las Animas" |
                             hospital_county == "Pueblo" ~ 704,
                           
                           hospital_county == "Moffat" |
                             hospital_county == "Routt" ~ 735,
                           
                           hospital_county == "Archuleta" |
                             hospital_county == "Dolores" |
                             hospital_county == "La Plata" |
                             hospital_county == "Montezuma" |
                             hospital_county == "San Juan" ~ 740,
                           
                           hospital_county == "Morgan" |
                             hospital_county == "Washington" |
                             hospital_county == "Weld" |
                             hospital_county == "Yuma" ~ 760)) %>% 
  group_by(hsa_cdc) %>% 
  summarize(tot_adms = sum(adms)) %>% 
  left_join(county_pop %>% 
              mutate(hsa_cdc = case_when(group == "Alamosa" |
                                           group == "Conejos" |
                                           group == "Costilla" |
                                           group == "Mineral" |
                                           group == "Rio Grande" |
                                           group == "Saguache" ~ 731,
                                         
                                         group == "Jackson"  ~ 771,
                                         
                                         group == "Boulder" |
                                           group == "Broomfield" ~ 795,
                                         
                                         group == "Chaffee" |
                                           group == "Lake" ~ 786,
                                         
                                         group == "Adams" |
                                           group == "Arapahoe" |
                                           group == "Clear Creek" |
                                           group == "Denver" |
                                           group == "Douglas" |
                                           group == "Elbert" |
                                           group == "Gilpin" |
                                           group == "Grand" |
                                           group == "Jefferson" |
                                           group == "Park" |
                                           group == "Summit" ~ 688,
                                         
                                         group == "Cheyenne" |
                                           group == "El Paso" |
                                           group == "Kit Carson" |
                                           group == "Lincoln" |
                                           group == "Teller" ~ 754,
                                         
                                         group == "Custer" |
                                           group == "Fremont" ~ 812,
                                         
                                         group == "Baca" ~ 562,
                                         
                                         group == "Larimer" ~ 796,
                                         
                                         group == "Logan" |
                                           group == "Phillips" |
                                           group == "Sedgwick" ~ 763,
                                         
                                         group == "Eagle" |
                                           group == "Garfield" |
                                           group == "Mesa" |
                                           group == "Pitkin" |
                                           group == "Rio Blanco" ~ 711,
                                         
                                         group == "Delta" |
                                           group == "Gunnison" |
                                           group == "Hinsdale" |
                                           group == "Montrose" |
                                           group == "Ouray" |
                                           group == "San Miguel" ~ 761,
                                         
                                         group == "Bent" |
                                           group == "Crowley" |
                                           group == "Kiowa" |
                                           group == "Otero" |
                                           group == "Prowers" ~ 745,
                                         
                                         group == "Huerfano" |
                                           group == "Las Animas" |
                                           group == "Pueblo" ~ 704,
                                         
                                         group == "Moffat" |
                                           group == "Routt" ~ 735,
                                         
                                         group == "Archuleta" |
                                           group == "Dolores" |
                                           group == "La Plata" |
                                           group == "Montezuma" |
                                           group == "San Juan" ~ 740,
                                         
                                         group == "Morgan" |
                                           group == "Washington" |
                                           group == "Weld" |
                                           group == "Yuma" ~ 760)) %>%
              group_by(hsa_cdc) %>% 
              summarize(tot_pop = sum(population)), by = "hsa_cdc") %>% 
  filter(hsa_cdc != 740, hsa_cdc != 771, hsa_cdc != 562) %>% # filter regions with OOS counties
  mutate(cumul_adm_per_100k = round(100000*tot_adms/tot_pop, 1)) %>% 
  ungroup() %>% 
  select(hsa_cdc, cumul_adm_per_100k) %>% 
  distinct() %>% 
  arrange(hsa_cdc) %>% 
  rename(HSA = hsa_cdc,
         `Cumulative Admissions Per 100k` = cumul_adm_per_100k)
  
co_pop <- tbl(conn, in_schema("dbo", "populations_state")) %>% 
  filter(State == "Colorado",
         Age == "Total Population") %>% 
  select(`2019_Population`) %>% 
  collect()

co_rate <- read.csv("Teletracker/2022-06-07 HHSProtect Teletracker.csv") %>% 
  select(entry_date,
         admits_last_24_hrs_covid_admissions_confirmed_adult,
         admits_last_24_hrs_covid_admissions_confirmed_pediatric) %>% 
  mutate(entry_date = ymd(entry_date)) %>% 
  mutate(adms = as.numeric(admits_last_24_hrs_covid_admissions_confirmed_adult) + 
           as.numeric(admits_last_24_hrs_covid_admissions_confirmed_pediatric)) %>% 
  filter(entry_date >= "2022-06-01",
         entry_date <= "2022-06-07") %>%
  summarize(tot_adms = sum(adms)) %>% 
  ungroup() %>% 
  mutate(cumul_adm_per_100k = round(100000*tot_adms/co_pop$`2019_Population`, 1)) %>% 
  select(-tot_adms) %>% 
  rename(`Cumulative Admissions Per 100k` = cumul_adm_per_100k)

# Save Data ---------------------------------------------------------------
adm_rates <- list(
  "By County" = tele, 
  "By HSA" = hsa,
  "Statewide" = co_rate)

write.xlsx(adm_rates, "~/../Downloads/cumulative admission rates.xlsx")

# write to google sheets -----------------------------

sheet_write(tele, ss = "https://docs.google.com/spreadsheets/d/1DfDrq5OZW9QBpz1UoEe9wluGTlbMG8ENrZkFqOXVOqI/edit#gid=18976810",
            sheet = "County Level")

sheet_write(hsa, ss = "https://docs.google.com/spreadsheets/d/1DfDrq5OZW9QBpz1UoEe9wluGTlbMG8ENrZkFqOXVOqI/edit#gid=18976810",
            sheet = "HSA Level")

sheet_write(co_rate, ss = "https://docs.google.com/spreadsheets/d/1DfDrq5OZW9QBpz1UoEe9wluGTlbMG8ENrZkFqOXVOqI/edit#gid=18976810",
            sheet = "State Level")
