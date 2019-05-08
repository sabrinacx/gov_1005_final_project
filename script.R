library(tidyverse)
library(janitor)
library(readxl)
library(maps)
library(gganimate)
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

all_states_formatted <- all_states %>%
  filter(!is.na(area),
         crime_type == "violent_crime_rate_per_100_000") %>% 
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9))) %>%
  group_by(area) %>% 
  filter(rank <=10) %>%
  ungroup() %>% 
  mutate(value = as.integer(value))

violent_gif <- 
  all_states_formatted %>% 
  ggplot(aes(rank, group = area, 
             fill = as.factor(area), color = as.factor(area))) +
  geom_tile(aes(y = as.integer(value/2),
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(area, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = value, label = as.character(value), hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  #scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) + 
  transition_states(year, transition_length = 4, state_length = 4) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Violent Crime Rates per Year : {closest_state}',  
       subtitle  =  "Top 10 States",
       caption  = "")

anim_save("violent.gif", animate(violent_gif)) 



