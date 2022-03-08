library(ciisr)
library(covidr)
library(ggplot2)
library(dbplyr)
library(lubridate)
library(tidyverse)
library(scales)

# set up connection
connect("covid19", server = "DPHE144")

# get county populations
county_pop <- tbl(conn, in_schema("dbo", "populations")) %>% 
  filter(metric == "county",
         year == 2020) %>% 
  select(group, population) %>% 
  collect()


cases <- tbl(conn, in_schema("cases", "county")) %>% 
  filter(reporteddate >= "2022-02-24",
         reporteddate <= "2022-03-02") %>% 
  select(countyassigned, n) %>% 
  collect() %>%
  group_by(countyassigned) %>% 
  summarise(cases = sum(n)) %>% 
  left_join(county_pop, by = c("countyassigned" = "group")) %>% 
  mutate(rate = round((cases/population)*100000))


write.csv(cases, "case_rate.csv")
