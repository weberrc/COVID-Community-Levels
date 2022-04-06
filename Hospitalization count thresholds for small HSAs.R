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
  filter(total_pop <= 100000)

# make a sequence of hospitalizations to calculate rates ----------------------
hosp_counts <- seq(0,30, by = 1)
full_hosp_seq <- tibble(hosp_seq = rep(hosp_counts, length(unique(county_pop$hsa_cdc))))

seq_hsa <- tibble(hsa_cdc = rep(unique(county_pop$hsa_cdc), each = length(hosp_counts)))

seq_hsa <- cbind(seq_hsa, full_hosp_seq)

county_pop %<>% 
  right_join(seq_hsa) %>% 
  filter(!is.na(group),
         hsa_cdc != 740, 
         hsa_cdc != 771, 
         hsa_cdc != 562) %>% 
  mutate(rate = round((hosp_seq/total_pop)*100000),
         com_level = case_when(rate < 10 ~ "Low",
                               rate >= 10 & rate <= 19.9 ~ "Medium",
                               rate >= 20 ~ "High"))


county_pop$com_level <- factor(county_pop$com_level, levels = c("Low", "Medium", "High"))  

# ggplot(county_pop[county_pop$hsa_cdc == 731,], aes(x = hosp_seq, y = rate)) +
#   theme_covid() +
#   ylab("Hospitalization\nRate per\n100k") +
#   xlab("Hospitalizations") +
#   scale_x_continuous(breaks = seq(0,30, by = 5)) +
#   annotate("rect", xmin = min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "Medium",]$hosp_seq),
#            xmax=min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "High",]$hosp_seq),
#            ymin=-Inf, ymax=Inf, alpha=0.2, fill="gold") +
#   annotate("rect", xmin = min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "Low",]$hosp_seq),
#            xmax=min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "Medium",]$hosp_seq),
#            ymin=-Inf, ymax=Inf, alpha=0.2, fill="lightgreen") +
#   annotate("rect", xmin = min(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "High",]$hosp_seq),
#            xmax=max(county_pop[county_pop$hsa_cdc == 731 & county_pop$com_level == "High",]$hosp_seq),
#            ymin=-Inf, ymax=Inf, alpha=0.2, fill="darkorange2") +
#   ggtitle("HSA 731", subtitle = "Counties: Alamosa, Conejos, Costilla, Mineral, Rio Grande and Saguache") +
#   geom_point()

# HSA 731 -------------------------------------

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
           ymax = 10, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 20, 
           ymax = Inf, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 731 (Alamosa, Conejos, Costilla, Mineral, Rio Grande and Saguache)",
          subtitle = paste0("Total Population: ", prettyNum(county_pop[county_pop$hsa_cdc == 731,]$total_pop[1], big.mark = ","))) +
  geom_point()


# HSA 745 ---------------------------------------

ggplot(county_pop[county_pop$hsa_cdc == 745,], aes(x = hosp_seq, y = rate)) +
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
           ymax = 10, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 20, 
           ymax = Inf, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 745 (Bent, Crowley, Kiowa, Otero and Prowers)",
          subtitle = paste0("Total Population: ", prettyNum(county_pop[county_pop$hsa_cdc == 745,]$total_pop[1], big.mark = ","))) +
  geom_point()

# HSA 786 ---------------------------------------

ggplot(county_pop[county_pop$hsa_cdc == 786,], aes(x = hosp_seq, y = rate)) +
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
           ymax = 10, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 20, 
           ymax = Inf, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 786 (Chaffee and Lake)",
          subtitle = paste0("Total Population: ", prettyNum(county_pop[county_pop$hsa_cdc == 786,]$total_pop[1], big.mark = ","))) +
  geom_point()

# HSA 812 ---------------------------------------

ggplot(county_pop[county_pop$hsa_cdc == 812,], aes(x = hosp_seq, y = rate)) +
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
           ymax = 10, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 20, 
           ymax = Inf, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 812 (Custer and Fremont)",
          subtitle = paste0("Total Population: ", prettyNum(county_pop[county_pop$hsa_cdc == 812,]$total_pop[1], big.mark = ","))) +
  geom_point()


# HSA 763 ---------------------------------------

ggplot(county_pop[county_pop$hsa_cdc == 763,], aes(x = hosp_seq, y = rate)) +
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
           ymax = 10, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 20, 
           ymax = Inf, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 763 (Logan, Phillips and Sedgwick)",
          subtitle = paste0("Total Population: ", prettyNum(county_pop[county_pop$hsa_cdc == 763,]$total_pop[1], big.mark = ","))) +
  geom_point()

# HSA 735 ---------------------------------------

ggplot(county_pop[county_pop$hsa_cdc == 735,], aes(x = hosp_seq, y = rate)) +
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
           ymax = 10, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 20, 
           ymax = Inf, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 735 (Moffat and Routt)",
          subtitle = paste0("Total Population: ", prettyNum(county_pop[county_pop$hsa_cdc == 735,]$total_pop[1], big.mark = ","))) +
  geom_point()

# HSA 740 ---------------------------------------

oos_counties <- tibble(group = c('Navajo', 'San Juan AZ', 'San Juan UT'),
                       population = c(111606, 121661, 14518),
                       hsa_cdc = c(740, 740, 740))

county_pop <- tbl(conn, in_schema("dbo", "populations")) %>% 
  filter(metric == "county",
         year == 2020) %>% 
  select(group, population) %>% 
  collect() %>% 
  mutate(group = str_to_title(group),
         hsa_cdc = case_when(group == "Archuleta" |
                               group == "Dolores" |
                               group == "La Plata" |
                               group == "Montezuma" |
                               group == "San Juan" ~ 740)) %>% 
  filter(!is.na(hsa_cdc)) %>% 
  full_join(oos_counties) %>% 
  mutate(total_pop = sum(population))

hosp_counts <- seq(0,100, by = 1)
full_hosp_seq <- tibble(hosp_seq = rep(hosp_counts, length(unique(county_pop$hsa_cdc))))

seq_hsa <- tibble(hsa_cdc = rep(unique(county_pop$hsa_cdc), each = length(hosp_counts)))

seq_hsa <- cbind(seq_hsa, full_hosp_seq)


county_pop %<>% 
  right_join(seq_hsa) %>% 
  mutate(rate = round((hosp_seq/total_pop)*100000))

ggplot(county_pop, aes(x = hosp_seq, y = rate)) +
  theme_covid() +
  ylab("Hospitalization\nRate per\n100k") +
  xlab("Hospitalizations") +
  scale_x_continuous(breaks = seq(0,100, by = 20)) +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 10, 
           ymax = 19.9, alpha=0.2, fill="gold") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 0, 
           ymax = 10, alpha=0.2, fill="lightgreen") +
  annotate("rect", xmin = -Inf,
           xmax = Inf,
           ymin = 19.9, 
           ymax = Inf, alpha=0.2, fill="darkorange2") +
  ggtitle("HSA 740 (Archuleta, Dolores, La Plata, Montezuma, San Juan (CO), San Juan (AZ), San Juan (UT), Navajo (UT))",
          subtitle = paste0("Total Population: ", prettyNum(county_pop[county_pop$hsa_cdc == 740,]$total_pop[1], big.mark = ","))) +
  geom_point()

