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

# make a sequence of hospitalizations to calculate rates ----------------------
hosp_counts <- seq(0,200, by = 3)
full_hosp_seq <- tibble(hosp_seq = rep(hosp_counts, length(unique(county_pop$hsa_cdc))))

seq_hsa <- tibble(hsa_cdc = rep(unique(county_pop$hsa_cdc), each = length(hosp_counts)))

seq_hsa <- cbind(seq_hsa, full_hosp_seq)

# read county pop and add sequence of hosp counts ------------------------------
seven_day_total <- function(x){
  (lag(x,6) + lag(x,5) + lag(x,4) + lag(x,3) + lag(x,2) + lag(x,1) + x)}

connect("covid19", server = "DPHE144")

county_pop <- tbl(conn, in_schema("dbo", "populations")) %>% 
  filter(metric == "county",
         year == 2020) %>% 
  select(group, population) %>% 
  collect() %>% 
  mutate(group = str_to_title(group),
    hsa_cdc = case_when(group == "Alamosa" |
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
  mutate(total_pop = sum(population)) %>% 
  ungroup() %>% 
  filter(total_pop <= 100000) %>% 
  right_join(seq_hsa) %>% 
  mutate(rate = round((hosp_seq/total_pop)*100000),
         com_level = case_when(rate < 10 ~ "Low",
                               rate >= 10 & rate <= 19.9 ~ "Medium",
                               rate >= 20 ~ "High"))
  


