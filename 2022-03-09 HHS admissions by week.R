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
  filter(date <= Sys.Date() - days(7),
         date >= as.Date("2020-04-01")) %>% 
  mutate(week = floor_date(date, unit = "week")) %>% 
  group_by(week) %>% 
  summarise(week_total = sum(admissions)) 
  

ggplot(hosp0, aes(x = week, y = week_total)) +
  geom_col(fill = "steelblue") +
  theme_covid() +
  ylab("") +
  xlab("") +
  ggtitle("New Hospital Admissions by Week of Admission") +
  scale_x_date(date_breaks = "2 month", date_labels = "'%y %b",
               limits = c(min(hosp0$week), max(hosp0$week))) +
  labs(caption = paste0("Updated ", format(Sys.Date(), "%b %d, %Y"), ". Source: ", 
                        "U.S. Department of Health and Human Services (daily confirmed ", 
                        "Covid-19 hospital admissions).\nData are lagged by 7 days. "))
