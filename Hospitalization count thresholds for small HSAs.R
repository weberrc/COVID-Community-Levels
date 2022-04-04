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
hosp_counts <- seq(0,30, by = 1)
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
  filter(!is.na(group)) %>% 
  mutate(rate = round((hosp_seq/total_pop)*100000),
         com_level = case_when(rate < 10 ~ "Low",
                               rate >= 10 & rate <= 19.9 ~ "Medium",
                               rate >= 20 ~ "High"))


county_pop$com_level <- factor(county_pop$com_level, levels = c("Low", "Medium", "High"))  

ggplot(county_pop[county_pop$hsa_cdc == 731,], aes(x = hosp_seq, y = rate)) +
  theme_covid() +
  ylab("Hospitalization\nRate per\n100k") +
  xlab("Hospitalizations") +
  scale_x_continuous(breaks = seq(0,30, by = 5)) +
  annotate("rect", xmin = min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "Medium",]$hosp_seq),
           xmax=min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "High",]$hosp_seq),
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="gold") +
  annotate("rect", xmin = min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "Low",]$hosp_seq),
           xmax=min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "Medium",]$hosp_seq),
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "High",]$hosp_seq),
           xmax=max(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "High",]$hosp_seq),
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 731", subtitle = "Counties: Alamosa, Conejos, Costilla, Mineral, Rio Grande and Saguache") +
  geom_point()

ggplot(county_pop[county_pop$hsa_cdc == 731,], aes(x = hosp_seq, y = rate)) +
  theme_covid() +
  ylab("Hospitalization\nRate per\n100k") +
  xlab("Hospitalizations") +
  scale_x_continuous(breaks = seq(0,30, by = 5)) +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 10, 
           ymax = 19.9, alpha=0.2, fill="gold") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 0, 
           ymax = 10, alpha=0.2, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 20, 
           ymax = Inf, alpha=0.2, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 731", subtitle = "Counties: Alamosa, Conejos, Costilla, Mineral, Rio Grande and Saguache") +
  geom_point()
