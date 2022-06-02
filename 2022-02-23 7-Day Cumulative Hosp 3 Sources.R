library(ciisr)
library(covidr)
library(dbplyr)
library(DescTools)
library(ggplot2)
library(ggrepel)
library(jsonlite)
library(latex2exp)
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

lab <- hosp0 %>% 
  filter(date == max(date))

ggplot(hosp0, aes(x = date, y = adm_7)) +
  geom_line(size = 1.1, col = "#89617c") +
  theme_covid() +
  ylab("7-Day\nCumulative\nHospital\nAdmissions") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(min(hosp0$date), max(hosp0$date) + days(4))) +
  geom_text_repel(data = lab,
                  aes(x = date + days(3), y = adm_7,
                      label = adm_7),
                  fontface = "bold",
                  size = 4.2,
                  min.segment.length = Inf,
                  color = "#89617c") +
  labs(caption = paste0("Updated ", format(Sys.Date(), "%b %d, %Y"), ". Source: ", 
                        "U.S. Department of Health and Human Services (daily confirmed ", 
                        "Covid-19 hospital admissions).\nData are lagged by 7 days. ")) +
  ggtitle("HHS")

# COPHS -----------------------------------------------

cophs_tidy <- tbl(conn, in_schema("hospital", "cophs_tidy")) %>% 
  filter(hospital_admission_date >= "2020-03-02",
         hospital_admission_date <= "2022-06-02") %>% 
  collect() %>% 
  distinct() %>% 
  group_by(hospital_admission_date) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(hospital_admission_date) %>% 
  complete(hospital_admission_date = seq.Date(min(hospital_admission_date), max(hospital_admission_date), by="day")) %>% 
  mutate_at(c(2), ~replace(., is.na(.), 0)) %>% 
  mutate(adm_7 = seven_day_total(n)) %>% 
  filter(hospital_admission_date >= ymd("2020-10-01"),
         hospital_admission_date <= Sys.Date() - days(7))


lab2 <- cophs_tidy %>% 
  filter(hospital_admission_date == max(hospital_admission_date))

ggplot(cophs_tidy, aes(x = hospital_admission_date, y = adm_7)) +
  geom_line(size = 1.1, col = "#89617c") +
  theme_covid() +
  ylab("7-Day\nCumulative\nHospital\nAdmissions") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(min(cophs_tidy$hospital_admission_date), max(cophs_tidy$hospital_admission_date) + days(4))) +
  geom_text_repel(data = lab2,
                  aes(x = hospital_admission_date + days(3), y = adm_7,
                      label = adm_7),
                  fontface = "bold",
                  size = 4.2,
                  min.segment.length = Inf,
                  color = "#89617c") +
  labs(caption = paste0("Updated ", format(Sys.Date(), "%b %d, %Y"), ". Source: COPHS. ",
                        "Data are lagged by 7 days.")) +
  ggtitle("COPHS")


cophs_count <- tbl(conn, in_schema("hospital", "cophs_tidy")) %>% 
  filter(hospital_admission_date >= "2022-05-01",
         hospital_admission_date <= "2022-06-02") %>% 
  collect() %>% 
  distinct() %>% 
  group_by(hospital_admission_date) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(hospital_admission_date) %>% 
  complete(hospital_admission_date = seq.Date(min(hospital_admission_date), max(hospital_admission_date), by="day")) %>% 
  mutate_at(c(2), ~replace(., is.na(.), 0)) %>% 
  mutate(week = epiweek(hospital_admission_date), 
         year = year(hospital_admission_date)) %>% 
  add_epiweek_dates(which_dates = "start") %>% 
  group_by(start) %>% 
  summarize(cumul_adm = sum(n))

# EMR -------------------------------------------------------

emr <- tbl(conn, in_schema("hospital", "emrstate")) %>% 
  select(date, currently_hospitalized) %>%
  filter(date >= "2020-10-01") %>% 
  collect() %>% 
  filter(date <= Sys.Date() - days(7))

lab3 <- emr %>% 
  filter(date == max(date))


ggplot(emr, aes(x = date, y = currently_hospitalized)) +
  geom_line(size = 1.1, col = "#89617c") +
  theme_covid() +
  ylab("Currently\nHospitalized") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(min(emr$date), max(emr$date) + days(4))) +
  geom_text_repel(data = lab3,
                  aes(x = date + days(3), y = currently_hospitalized,
                      label = currently_hospitalized),
                  fontface = "bold",
                  size = 4.2,
                  min.segment.length = Inf,
                  color = "#89617c") +
  labs(caption = paste0("Updated ", format(Sys.Date(), "%b %d, %Y"), ". Source: EMResource. ",
                        "Data are lagged by 7 days.")) +
  ggtitle("EMR")

# Layer all on top of each other --------------------

hosp0 %<>% 
  mutate(source = "HHS") %>% 
  select(date, adm_7, source)
  
cophs_tidy %<>% 
  mutate(source = "COPHS") %>% 
  rename(date = hospital_admission_date) %>% 
  select(date, adm_7, source)

emr %<>%
  mutate(source = "EMR") %>% 
  rename(adm_7 = currently_hospitalized) %>% 
  select(date, adm_7, source)

all <- rbind(hosp0, cophs_tidy, emr) 

all$source <- factor(all$source, levels = c("COPHS", "EMR", "HHS"))
  
lab4 <- all %>% 
  filter(date == max(date)) %>% 
  mutate(label = paste0(source, ": ", adm_7))

ggplot(all, aes(x = date, y = adm_7, color = source)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c("#ea9999", "#35647e", "#a6a6a6")) +
  theme_covid() +
  ylab("7-Day\nCumulative\nHospital\nAdmissions") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(min(hosp0$date), max(hosp0$date) + days(18))) +
  geom_text_repel(data = lab4,
                  aes(x = date + days(12), y = adm_7,
                      label = label),
                  fontface = "bold",
                  size = 4.2,
                  nudge_x = 8,
                  min.segment.length = Inf) +
  geom_text(x = as.Date("2021-12-01"), y = 150,
            label = "EMR data are those currently hospitalized, not cumulative",
            color = "#a6a6a6") +
  labs(caption = paste0("Updated ", format(Sys.Date(), "%b %d, %Y"),".",
                        " Data are lagged by 7 days."))


# Estimating Admissions in EMR --------------------------------------

emr2 <- tbl(conn, in_schema("hospital", "emrstate")) %>% 
  filter(date >= "2020-10-01") %>% 
  collect() %>% 
  filter(date <= Sys.Date() - days(7)) %>% 
  arrange(date) %>% 
  mutate(adm_7 = seven_day_total(admissions))

lab5 <- emr2 %>% 
  filter(date == max(date))


ggplot(emr2, aes(x = date, y = adm_7)) +
  geom_line(size = 1.1, col = "#89617c") +
  theme_covid() +
  ylab("7-Day\nEstimated\nCumulative\nHospital\nAdmissions") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(min(emr$date), max(emr$date) + days(4))) +
  geom_text_repel(data = lab5,
                  aes(x = date + days(3), y = adm_7,
                      label = adm_7),
                  fontface = "bold",
                  size = 4.2,
                  min.segment.length = Inf,
                  color = "#89617c") +
  labs(caption = paste0("Updated ", format(Sys.Date(), "%b %d, %Y"), ". Source: EMResource. ",
                        "Data are lagged by 7 days. Daily admissions are estimated based on ",
                        "those currently hospitalized, discharges, and deaths.")) +
  ggtitle("EMR (estimated)")

# Layer all (with EMR estimates) on top of each other --------------------

hosp0 %<>% 
  mutate(source = "HHS") %>% 
  select(date, adm_7, source)

cophs_tidy %<>% 
  mutate(source = "COPHS") %>% 
  rename(date = hospital_admission_date) %>% 
  select(date, adm_7, source)

emr2 %<>%
  mutate(source = "EMR") %>%
  select(date, adm_7, source)

all2 <- rbind(hosp0, cophs_tidy, emr2) 

all2$source <- factor(all2$source, levels = c("COPHS", "EMR", "HHS"))

lab4 <- all2 %>% 
  filter(date == max(date)) %>% 
  mutate(label = paste0(source, ": ", adm_7))



ggplot(all2, aes(x = date, y = adm_7, color = source)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c("#ea9999", "#35647e", "#a6a6a6")) +
  theme_covid() +
  ylab("7-Day\nCumulative\nHospital\nAdmissions") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(min(all2$date), max(all2$date) + days(18))) +
  geom_text_repel(data = lab4,
                  aes(x = date + days(12), y = adm_7,
                      label = label),
                  fontface = "bold",
                  size = 4.2,
                  nudge_x = 8,
                  min.segment.length = Inf) +
  geom_text(x = as.Date("2021-11-01"), y = 150, parse = TRUE,
            label = "'EMR admissions' == confirmed - ('confirmed yesterday' - discharges - deaths)",
            color = "#35647e",
            size = 3.2) +
  labs(caption = paste0("Updated ", format(Sys.Date(), "%b %d, %Y"),".",
                        " Data are lagged by 7 days."))

# HHS county level ---------------------------------------------------

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
  mutate(hs_reg = case_when(County == "Morgan" |
                              County == "Logan" |
                              County == "Washington" |
                              County == "Yuma" |
                              County == "Phillips" |
                              County == "Sedgwick" ~ 1,
                            
                            County == "Larimer" ~ 2,
                            County == "Douglas" ~ 3,
                            County == "El Paso" ~ 4,
                            
                            County == "Lincoln" |
                              County == "Elbert" |
                              County == "Kit Carson" |
                              County == "Cheyenne" ~ 5,
                            
                            County == "Las Animas" |
                              County == "Baca" |
                              County == "Prowers" |
                              County == "Bent" |
                              County == "Kiowa" |
                              County == "Otero" |
                              County == "Crowley" |
                              County == "Huerfano" ~ 6,
                            
                            County == "Pueblo" ~ 7,
                            
                            County == "Mineral" |
                              County == "Costilla" |
                              County == "Conejos" |
                              County == "Alamosa" |
                              County == "Saguache" |
                              County == "Rio Grande" ~ 8,
                            
                            County == "Dolores" |
                              County == "Montezuma" |
                              County == "La Plata" |
                              County == "San Juan" |
                              County == "Archuleta" ~ 9,
                            
                            County == "Gunnison" |
                              County == "Delta" |
                              County == "Montrose" |
                              County == "Ouray" |
                              County == "San Miguel" |
                              County == "Hinsdale" ~ 10,
                            
                            County == "Jackson" |
                              County == "Routt" |
                              County == "Moffat" |
                              County == "Rio Blanco" ~ 11,
                            
                            County == "Garfield" |
                              County == "Pitkin" |
                              County == "Eagle" |
                              County == "Summit" |
                              County == "Grand" ~ 12,
                            
                            County == "Lake" |
                              County == "Chaffee" |
                              County == "Fremont" |
                              County == "Custer" ~ 13,
                            
                            County == "Adams" ~ 14,
                            County == "Arapahoe" ~ 15,
                            
                            County == "Boulder" |
                              County == "Broomfield" ~ 16,
                            
                            County == "Park" |
                              County == "Teller" |
                              County == "Clear Creek" |
                              County == "Gilpin" ~ 17,
                            
                            County == "Weld" ~ 18,
                            County == "Mesa" ~ 19,
                            County == "Denver" ~ 20,
                            County == "Jefferson" ~ 21)) %>% 
  group_by(hs_reg) %>% 
  summarise(total_adm_hhs = sum(conf_covid_admit_last_7_d),
            total_pop = sum(population)) %>% 
  mutate(rate = round((total_adm_hhs/total_pop)*100000))

hhs_county %>% 
  summarise(tot_adms = sum(total_adm_hhs))  

# COPHS county level ------------------------------------------------

crosswalk <- read.csv("C:/Users/rweber/Desktop/COVID/Surveillance/Hospital Crosswalk.csv") %>% 
  select(COPHS_Facility, County)


cophs_county <- tbl(conn, in_schema("hospital", "cophs_tidy")) %>% 
  filter(hospital_admission_date >= "2020-03-02",
         hospital_admission_date <= "2022-06-02") %>% 
  collect() %>% 
  distinct() %>% 
  mutate(facility_name = case_when(facility_name == "heart Of The Rockies Regional Medical Center" |
                                     facility_name == "Heart of the Rockies Regional Medical Center" |
                                     facility_name == "Heart Of The Rockies Regional Medical Center230580" |
                                     facility_name == "Heart Of The Rockies Regional Medical Center" ~ "Heart of the Rockies Regional Medical Center",
                                   TRUE ~ facility_name)) %>% 
  select(facility_name, hospital_admission_date) %>% 
  group_by(facility_name, hospital_admission_date) %>%
  count() %>% 
  ungroup() %>%
  group_by(facility_name) %>% 
  complete(hospital_admission_date = seq.Date(min(hospital_admission_date), max(hospital_admission_date), by="day")) %>% 
  mutate_at(c(3), ~replace(., is.na(.), 0)) %>% 
  filter(hospital_admission_date >= Sys.Date() - days(14),
         hospital_admission_date <= Sys.Date() - days(7)) %>% 
  left_join(crosswalk, by = c("facility_name" = "COPHS_Facility")) %>% 
  mutate(hs_reg = case_when(County == "Morgan" |
                              County == "Logan" |
                              County == "Washington" |
                              County == "Yuma" |
                              County == "Phillips" |
                              County == "Sedgwick" ~ 1,
                            
                            County == "Larimer" ~ 2,
                            County == "Douglas" ~ 3,
                            County == "El Paso" ~ 4,
                            
                            County == "Lincoln" |
                              County == "Elbert" |
                              County == "Kit Carson" |
                              County == "Cheyenne" ~ 5,
                            
                            County == "Las Animas" |
                              County == "Baca" |
                              County == "Prowers" |
                              County == "Bent" |
                              County == "Kiowa" |
                              County == "Otero" |
                              County == "Crowley" |
                              County == "Huerfano" ~ 6,
                            
                            County == "Pueblo" ~ 7,
                            
                            County == "Mineral" |
                              County == "Costilla" |
                              County == "Conejos" |
                              County == "Alamosa" |
                              County == "Saguache" |
                              County == "Rio Grande" ~ 8,
                            
                            County == "Dolores" |
                              County == "Montezuma" |
                              County == "La Plata" |
                              County == "San Juan" |
                              County == "Archuleta" ~ 9,
                            
                            County == "Gunnison" |
                              County == "Delta" |
                              County == "Montrose" |
                              County == "Ouray" |
                              County == "San Miguel" |
                              County == "Hinsdale" ~ 10,
                            
                            County == "Jackson" |
                              County == "Routt" |
                              County == "Moffat" |
                              County == "Rio Blanco" ~ 11,
                            
                            County == "Garfield" |
                              County == "Pitkin" |
                              County == "Eagle" |
                              County == "Summit" |
                              County == "Grand" ~ 12,
                            
                            County == "Lake" |
                              County == "Chaffee" |
                              County == "Fremont" |
                              County == "Custer" ~ 13,
                            
                            County == "Adams" ~ 14,
                            County == "Arapahoe" ~ 15,
                            
                            County == "Boulder" |
                              County == "Broomfield" ~ 16,
                            
                            County == "Park" |
                              County == "Teller" |
                              County == "Clear Creek" |
                              County == "Gilpin" ~ 17,
                            
                            County == "Weld" ~ 18,
                            County == "Mesa" ~ 19,
                            County == "Denver" ~ 20,
                            County == "Jefferson" ~ 21)) %>% 
  group_by(hs_reg) %>% 
  summarise(total_adm_cophs = sum(n))

cophs_county %>% 
  summarise(tot_adms = sum(total_adm_cophs))
  
# EMR county level ---------------------------------------------

emr_county <- tbl(conn, in_schema("hospital", "emrcounty")) %>%
  select(county, date, admissions) %>% 
  collect() %>% 
  filter(date >= Sys.Date() - days(14),
         date <= Sys.Date() - days(7)) %>%
  mutate(admissions = case_when(admissions < 0 ~ 0,
                                TRUE ~ admissions)) %>% 
  group_by(county) %>%
  summarise(total_adm = sum(admissions)) %>% 
  ungroup()

# can't run this in piping for some reason
emr_county$county <- str_to_title(emr_county$county)

emr_county %<>% 
  mutate(hs_reg = case_when(county == "Morgan" |
                              county == "Logan" |
                              county == "Washington" |
                              county == "Yuma" |
                              county == "Phillips" |
                              county == "Sedgwick" ~ 1,
                            
                            county == "Larimer" ~ 2,
                            county == "Douglas" ~ 3,
                            county == "El Paso" ~ 4,
                            
                            county == "Lincoln" |
                              county == "Elbert" |
                              county == "Kit Carson" |
                              county == "Cheyenne" ~ 5,
                            
                            county == "Las Animas" |
                              county == "Baca" |
                              county == "Prowers" |
                              county == "Bent" |
                              county == "Kiowa" |
                              county == "Otero" |
                              county == "Crowley" |
                              county == "Huerfano" ~ 6,
                            
                            county == "Pueblo" ~ 7,
                            
                            county == "Mineral" |
                              county == "Costilla" |
                              county == "Conejos" |
                              county == "Alamosa" |
                              county == "Saguache" |
                              county == "Rio Grande" ~ 8,
                            
                            county == "Dolores" |
                              county == "Montezuma" |
                              county == "La Plata" |
                              county == "San Juan" |
                              county == "Archuleta" ~ 9,
                            
                            county == "Gunnison" |
                              county == "Delta" |
                              county == "Montrose" |
                              county == "Ouray" |
                              county == "San Miguel" |
                              county == "Hinsdale" ~ 10,
                            
                            county == "Jackson" |
                              county == "Routt" |
                              county == "Moffat" |
                              county == "Rio Blanco" ~ 11,
                            
                            county == "Garfield" |
                              county == "Pitkin" |
                              county == "Eagle" |
                              county == "Summit" |
                              county == "Grand" ~ 12,
                            
                            county == "Lake" |
                              county == "Chaffee" |
                              county == "Fremont" |
                              county == "Custer" ~ 13,
                            
                            county == "Adams" ~ 14,
                            county == "Arapahoe" ~ 15,
                            
                            county == "Boulder" |
                              county == "Broomfield" ~ 16,
                            
                            county == "Park" |
                              county == "Teller" |
                              county == "Clear Creek" |
                              county == "Gilpin" ~ 17,
                            
                            county == "Weld" ~ 18,
                            county == "Mesa" ~ 19,
                            county == "Denver" ~ 20,
                            county == "Jefferson" ~ 21)) %>% 
  group_by(hs_reg) %>% 
  summarise(total_adm_emr = sum(total_adm))


emr_county %>% 
  summarise(tot_adms = sum(total_adm_emr))






