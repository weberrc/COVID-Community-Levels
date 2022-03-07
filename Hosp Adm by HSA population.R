library(ciisr)
library(covidr)
library(dbplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(magrittr)
library(readr)
library(RSocrata)
library(tidyverse)


seven_day_total <- function(x){
  (lag(x,6) + lag(x,5) + lag(x,4) + lag(x,3) + lag(x,2) + lag(x,1) + x)}

connect("covid19", server = "DPHE144")

# HHS Hospitalizations -------------------------------

# https://beta.healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh
hhs <- read.socrata("https://healthdata.gov/resource/g62h-syeh.json")

hosp0 <- hhs %>% 
  as_tibble() %>% 
  select(state, date, previous_day_admission_adult_covid_confirmed,
         previous_day_admission_pediatric_covid_confirmed) %>% 
  filter(state == "CO") %>% 
  select(-state) %>%
  mutate_at(c(2:3), ~replace(., is.na(.), 0)) %>% 
  mutate(date = ymd(str_sub(date, 1, 10)),
         admissions = as.numeric(previous_day_admission_adult_covid_confirmed) + 
           as.numeric(previous_day_admission_pediatric_covid_confirmed)) %>%
  arrange(date) %>% 
  select(date, admissions) %>% 
  mutate(adm_7 = seven_day_total(admissions)) %>% 
  filter(date >= ymd("2020-10-01"),
         date <= Sys.Date() - days(7))

county_pop <- tbl(conn, in_schema("dbo", "populations")) %>% 
  filter(metric == "county",
         year == 2020) %>% 
  select(group, population) %>% 
  collect()

county_pop$group <- str_to_title(county_pop$group)


hhs_county <- read.csv(file.choose())

hhs_county %<>% 
  select(County, conf_covid_admit_last_7_d) %>% 
  mutate(County = gsub(" County, CO", "", County)) %>% 
  left_join(county_pop, by = c("County" = "group")) %>%
  mutate(hsa_cdc = case_when(County == "Alamosa" |
                               County == "Conejos" |
                               County == "Costilla" |
                               County == "Mineral" |
                               County == "Rio Grande" |
                               County == "Saguache" ~ 731,
                             
                             County == "Jackson"  ~ 771,
                             
                             County == "Boulder" |
                               County == "Broomfield" ~ 795,
                             
                             County == "Chaffee" |
                               County == "Lake" ~ 786,
                             
                             County == "Adams" |
                               County == "Arapahoe" |
                               County == "Clear Creek" |
                               County == "Denver" |
                               County == "Douglas" |
                               County == "Elbert" |
                               County == "Gilpin" |
                               County == "Grand" |
                               County == "Jefferson" |
                               County == "Park" |
                               County == "Summit" ~ 688,
                             
                             County == "Cheyenne" |
                               County == "El Paso" |
                               County == "Kit Carson" |
                               County == "Lincoln" |
                               County == "Teller" ~ 754,
                             
                             County == "Custer" |
                               County == "Fremont" ~ 812,
                             
                             County == "Baca" ~ 562,
                               
                               County == "Larimer" ~ 796)) %>% 
  group_by(hsa_cdc) %>% 
  summarise(total_adm_hhs = sum(conf_covid_admit_last_7_d),
            total_pop = sum(population)) %>% 
  mutate(rate = round((total_adm_hhs/total_pop)*100000))








