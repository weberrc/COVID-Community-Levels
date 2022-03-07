library(ciisr)
library(covidr)
library(dbplyr)
library(googlesheets4)
library(readr)
library(RSocrata)
library(tidyverse)

hhs_county <- read.csv(file.choose())

latest <- hhs_county %>% 
  filter(State_Abbreviation == "CO") %>% 
  mutate(date = Sys.Date())

# first writing
# sheet_write(latest, ss = "https://docs.google.com/spreadsheets/d/1f6-9CequqtvWEMCKwpxqzkhPfnhlXYyV8jBmk-d8PEM/edit#gid=0",
#             sheet = "Sheet1")

# subsequent data weeks
sheet_append(latest, ss = "https://docs.google.com/spreadsheets/d/1f6-9CequqtvWEMCKwpxqzkhPfnhlXYyV8jBmk-d8PEM/edit#gid=0",
             sheet = "Sheet1")


