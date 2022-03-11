library(magrittr)
library(RSocrata)
library(tidyverse)


# Community Levels -------------------------------

cdc_cl <- read.socrata("https://data.cdc.gov/resource/3nnm-4jni.json")

cdc_cl %<>% 
  filter(state == "Colorado",
         !is.na(health_service_area)) %>% 
  select(county, health_service_area, health_service_area_number) %>% 
  mutate(county = gsub(" County, CO", "", county),
         DOC_facility = case_when(county == "Fremont" |               # Arrowhead , Centennial , Fremont, CO State Pen
                                    county == "Crowley" |             # Arkansas Valley Correctional Facility
                                    county == "Bent" |                # Bent County Correctional Facility
                                    county == "Chaffee" |             # Buena Vista Correctional Facility
                                    county == "Jefferson" |           # Colorado Correctional Facility
                                    county == "Delta" |               # Delta Correctional Center
                                    county == "Denver" |              # Denver Reception and Diagnostic; Denver Women's
                                    county == "Lincoln" |             # Limon Correctional
                                    county == "Pueblo" |              # La Vista Correctional Facility; Youthful Offender System
                                    county == "Garfield" |            # Rifle Correctional Facility; San Carlos Correctional Facility
                                    county == "Logan" |               # Sterling Correctional Facility
                                    county == "Las Animas" ~ 1,       # Trinidad Correctional Facility
                                  TRUE ~ 0))             
