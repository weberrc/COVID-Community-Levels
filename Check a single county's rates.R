library(covidr)
library(DBI)
library(dbplyr)
library(ggrepel)
library(lubridate)
library(magrittr)
library(odbc)
library(tidyverse)

conn <- connect("covid19", server = "DPHE144", global_env = F)

county_pop <- tbl(conn, in_schema("dbo", "populations")) %>% 
  filter(metric == "county",
         group == "SAN JUAN", 
         year == 2020) %>% 
  select(group, population) %>% 
  collect()
county_pop$group <- str_to_title(county_pop$group)


sj_total <- tbl(conn, in_schema("cases", "covid19_cedrs_dashboard_constrained")) %>%
  filter(reporteddate >= "2022-03-31", 
         reporteddate <= "2022-04-06",
         countyassigned == "San Juan") %>%
  collect()
