library(ciisr)
library(covidr)
library(dbplyr)
library(googlesheets4)
library(readr)
library(RSocrata)
library(tidyverse)

# https://beta.healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh
hhs <- read.socrata("https://healthdata.gov/resource/di4u-7yu6.json")

latest <- hhs %>% 
  as_tibble() %>% 
  filter(state == "CO") 

# first writing
# sheet_write(latest, ss = "https://docs.google.com/spreadsheets/d/1f6-9CequqtvWEMCKwpxqzkhPfnhlXYyV8jBmk-d8PEM/edit#gid=0",
#             sheet = "Sheet1")

# subsequent data weeks
sheet_append(latest, ss = "https://docs.google.com/spreadsheets/d/1f6-9CequqtvWEMCKwpxqzkhPfnhlXYyV8jBmk-d8PEM/edit#gid=0",
             sheet = "Sheet1")


