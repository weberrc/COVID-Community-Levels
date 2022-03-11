library(ciisr)
library(covidr)
library(dbplyr)
library(geojsonR)
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

county_pop <- tbl(conn, in_schema("dbo", "populations")) %>% 
  filter(metric == "county",
         year == 2020) %>% 
  select(group, population) %>% 
  collect()

county_pop$group <- str_to_title(county_pop$group)

# HHS Hospitalizations -------------------------------

hhs_county <- read.csv(file.choose())

test <- rgdal::readOGR("https://opendata.arcgis.com/datasets/cad5b70395a04b95936f0286be30e282_0.geojson")

week_rate <- hhs_county %>% 
  select(State_Abbreviation, County, conf_covid_admit_last_7_d) %>% 
  filter(State_Abbreviation == "CO") %>% 
  select(-State_Abbreviation) %>%
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
                             
                             County == "Larimer" ~ 796,
                             
                             County == "Logan" |
                               County == "Phillips" |
                               County == "Sedgwick" ~ 763,
                             
                             County == "Eagle" |
                               County == "Garfield" |
                               County == "Mesa" |
                               County == "Pitkin" |
                               County == "Rio Blanco" ~ 711,
                             
                             County == "Delta" |
                               County == "Gunnison" |
                               County == "Hinsdale" |
                               County == "Montrose" |
                               County == "Ouray" |
                               County == "San Miguel" ~ 761,
                             
                             County == "Bent" |
                               County == "Crowley" |
                               County == "Kiowa" |
                               County == "Otero" |
                               County == "Prowers" ~ 745,
                             
                             County == "Huerfano" |
                               County == "Las Animas" |
                               County == "Pueblo" ~ 704,
                             
                             County == "Moffat" |
                               County == "Routt" ~ 735,
                             
                             County == "Archuleta" |
                               County == "Dolores" |
                               County == "La Plata" |
                               County == "Montezuma" |
                               County == "San Juan" ~ 740,
                             
                             County == "Morgan" |
                               County == "Washington" |
                               County == "Weld" |
                               County == "Yuma" ~ 760)) %>% 
  group_by(hsa_cdc) %>% 
  mutate(total_adm_hhs = sum(conf_covid_admit_last_7_d),
            total_pop = sum(population),
         rate = round((total_adm_hhs/total_pop)*100000)) %>% 
  ungroup() %>% 
  filter(hsa_cdc != 740, hsa_cdc != 771, hsa_cdc != 562) # filter regions with OOS counties


write.csv(week_rate, "week_rate.csv")

# HSA 740 is assigned to NM and AZ counties
# HSA 771 is assigned to WY
# HSA 562 is assigned to KS







