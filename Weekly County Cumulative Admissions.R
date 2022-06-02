library(ciisr)
library(covidr)
library(dbplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(readr)
library(jsonlite)
library(readxl)
library(RSocrata)
library(scales)
library(stringi)
library(tidyverse)

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
tele <- read_excel("Teletracker/2022-05 HHSProtect Teletracker.xlsx") %>% 
  select(entry_date,
         admits_last_24_hrs_covid_admissions_confirmed_adult,
         admits_last_24_hrs_covid_admissions_confirmed_pediatric,
         hospital_county) %>% 
  mutate(week = epiweek(entry_date), 
         year = year(entry_date),
         adms = admits_last_24_hrs_covid_admissions_confirmed_adult + admits_last_24_hrs_covid_admissions_confirmed_pediatric) %>% 
  add_epiweek_dates(which_dates = "start") %>% 
  group_by(start) %>% 
  summarize(cumul_adm = sum(adms))
