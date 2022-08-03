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

hsa_crosswalk <- data.frame(county = c("Alamosa", "Conejos", "Costilla", "Mineral", "Rio Grande", "Saguache",
                                       "Jackson",
                                       "Boulder", "Broomfield",
                                       "Chaffee", "Lake",
                                       "Adams", "Arapahoe", "Clear Creek", "Denver", "Douglas", "Elbert",
                                       "Gilpin", "Grand", "Jefferson", "Park", "Summit",
                                       "Cheyenne", "El Paso", "Kit Carson", "Lincoln", "Teller",
                                       "Custer", "Fremont",
                                       "Baca",
                                       "Larimer",
                                       "Logan", "Phillips", "Sedgwick",
                                       "Eagle", "Garfield", "Mesa", "Pitkin", "Rio Blanco",
                                       "Delta", "Gunnison", "Hinsdale", "Montrose", "Ouray", "San Miguel",
                                       "Bent", "Crowley", "Kiowa", "Otero", "Prowers",
                                       "Huerfano", "Las Animas", "Pueblo",
                                       "Moffat", "Routt",
                                       "Archuleta", "Dolores", "La Plata", "Montezuma", "San Juan",
                                       "Morgan", "Washington", "Weld", "Yuma"),
                            hsa_cdc = c(rep(731, 6),
                                        771,
                                        rep(795, 2), 
                                        rep(786, 2),
                                        rep(688, 11),
                                        rep(754, 5),
                                        rep(812, 2),
                                        562,
                                        796,
                                        rep(763, 3),
                                        rep(711, 5),
                                        rep(761, 6),
                                        rep(745, 5),
                                        rep(704, 3),
                                        rep(735, 2),
                                        rep(740, 5),
                                        rep(760, 4)))



# get county populations ------------------------------
county_pop <- tbl(conn, in_schema("dbo", "populations")) %>% 
  filter(metric == "county",
         year == 2020) %>% 
  select(group, population) %>% 
  collect() 

county_pop$group <- stri_trans_totitle(county_pop$group)

county_pop %<>% left_join(hsa_crosswalk, by = c("group" = "county"))

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
tele <- read.csv("Teletracker/2022-08-03 HHSProtect Teletracker.csv") %>% 
  select(entry_date,
         admits_last_24_hrs_covid_admissions_confirmed_adult,
         admits_last_24_hrs_covid_admissions_confirmed_pediatric,
         hospital_county) %>% 
  mutate(entry_date = ymd(entry_date)) %>% 
  mutate(adms = as.numeric(admits_last_24_hrs_covid_admissions_confirmed_adult) + 
                as.numeric(admits_last_24_hrs_covid_admissions_confirmed_pediatric),
         hospital_county = gsub(" County", "", hospital_county)) %>% 
  filter(entry_date >= "2022-07-27",
         entry_date <= "2022-08-02") %>% 
  group_by(hospital_county) %>% 
  summarize(tot_adms = sum(adms)) %>% 
  left_join(county_pop %>% 
              select(-hsa_cdc), by = c("hospital_county" = "group")) %>% 
  mutate(cumul_adm_per_100k = round(100000*tot_adms/population, 1)) %>% 
  ungroup() %>% 
  select(hospital_county, cumul_adm_per_100k) %>% 
  distinct() %>% 
  arrange(hospital_county) %>% 
  rename(County = hospital_county,
         `Cumulative Admissions Per 100k` = cumul_adm_per_100k)


hsa_pop <- county_pop %>% 
  group_by(hsa_cdc) %>% 
  summarize(hsa_pop = sum(population)) %>% 
  ungroup() %>% 
  mutate(hsa_pop = case_when(hsa_cdc == 740 ~ hsa_pop + 125608 + 15295,
                                hsa_cdc == 771 ~ hsa_pop + 38664 + 15073,
                                hsa_cdc == 562 ~ hsa_pop + 7324 + 2030,
                                TRUE ~ hsa_pop))

hsa <- read.csv("Teletracker/2022-08-03 HHSProtect Teletracker.csv") %>% 
  select(entry_date,
         admits_last_24_hrs_covid_admissions_confirmed_adult,
         admits_last_24_hrs_covid_admissions_confirmed_pediatric,
         hospital_county) %>% 
  mutate(entry_date = ymd(entry_date)) %>% 
  mutate(adms = as.numeric(admits_last_24_hrs_covid_admissions_confirmed_adult) + 
                as.numeric(admits_last_24_hrs_covid_admissions_confirmed_pediatric),
         hospital_county = gsub(" County", "", hospital_county)) %>% 
  filter(entry_date >= "2022-07-27",
         entry_date <= "2022-08-02") %>% 
  select(hospital_county, adms) %>% 
  group_by(hospital_county) %>%
  summarize(tot_adms = sum(adms)) %>%
  ungroup() %>% 
  left_join(hsa_crosswalk, by = c("hospital_county" = "county")) %>%
  group_by(hsa_cdc) %>% 
  summarize(hsa_adms = sum(tot_adms)) %>% 
  ungroup() %>% 
  left_join(hsa_pop) %>% 
  mutate(cumul_adm_per_100k = round(100000*hsa_adms/hsa_pop, 1)) %>% 
  ungroup() %>% 
  select(hsa_cdc, cumul_adm_per_100k) %>% 
  distinct() %>% 
  arrange(hsa_cdc) %>% 
  left_join(hsa_crosswalk %>% 
              group_by(hsa_cdc) %>% 
              summarise(counties_in_hsa = paste(unique(county), collapse = ', '))) %>% 
  mutate(OOS_counties = case_when(hsa_cdc == 740 ~ "San Juan (NM), San Juan (UT)",
                                  hsa_cdc == 771 ~ "Albany (WY), Carbon (WY)",
                                  hsa_cdc == 562 ~ "Grant (KS), Stanton (KS)",
                                  TRUE ~ "")) %>% 
  rename(HSA = hsa_cdc,
         `Cumulative Admissions Per 100k` = cumul_adm_per_100k,
         `Counties in HSA` = counties_in_hsa,
         `OOS Counties in HSA` = OOS_counties)
  
co_pop <- tbl(conn, in_schema("dbo", "populations_state")) %>% 
  filter(State == "Colorado",
         Age == "Total Population") %>% 
  select(`2019_Population`) %>% 
  collect()

co_rate <- read.csv("Teletracker/2022-08-03 HHSProtect Teletracker.csv") %>% 
  select(entry_date,
         admits_last_24_hrs_covid_admissions_confirmed_adult,
         admits_last_24_hrs_covid_admissions_confirmed_pediatric) %>% 
  mutate(entry_date = ymd(entry_date)) %>% 
  mutate(adms = as.numeric(admits_last_24_hrs_covid_admissions_confirmed_adult) + 
           as.numeric(admits_last_24_hrs_covid_admissions_confirmed_pediatric)) %>% 
  filter(entry_date >= "2022-07-27",
         entry_date <= "2022-08-02") %>%
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
