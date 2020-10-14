# Tue Oct 13 16:27:38 2020 ------------------------------
# Data Visualization in R
# SOC500 Workshop
# John A. Bernau
# Data wrangling with the tidyverse
# -------------------------------------------------------
require(tidyverse)

# EXERCISE ANSWERS --------------------------------------



# starwars: what is the average mass of each species?
starwars %>% 
  filter(!is.na(mass)) %>% 
  group_by(species) %>% 
  summarize(avg_mass = mean(mass))

# starwars: how many humans have brown eyes?
starwars %>% 
  filter(species == "Human",
         eye_color == "brown")

# starwars: how many brown-eyed humans are male / female?
starwars %>% 
  filter(species == "Human",
         eye_color == "brown") %>% 
  count(gender)

# storms: what was the average windspeed for hurricanes in each year?
storms %>% 
  filter(status == "hurricane") %>% 
  group_by(year) %>% 
  summarize(avg_wind = mean(wind))