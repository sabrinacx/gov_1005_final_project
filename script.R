library(tidyverse)
library(janitor)
library(readxl)
library(maps)
us_states <- map_data("state")

c10_11 <- read_xls("crime_2010_2011.xls") %>% 
  clean_names() 

c12_13 <- read_xls("crime_2012_2013.xls") %>% 
  clean_names() 

c14_15 <- read_xls("crime_2014_2015.xls") %>% 
  clean_names() 

c16_17 <- read_xls("crime2016.xls") %>% 
  clean_names() 

all <- bind_rows(c10_11, c12_13, c14_15, c16_17, .id = "dataset") %>% 
  gather(crime_type, value, c(violent_crime:rape_revised_definition_rate_per_100_000))

all_states <- all %>% filter(!area %in% c("United States Total", "Northeast", "New England", "Middle Atlantic", "Midwest", 
                                          "East North Central", "West North Central", "South", "South Atlantic", "East South Central",
                                          "West South Central", "West", "Mountain", "Pacific"))

write_rds(all, path = "crime1.rds")
write_rds(all_states, path = "crime1_only_states.rds")


