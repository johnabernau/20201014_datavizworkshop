# Tue Oct 13 16:27:38 2020 ------------------------------
# Data Visualization in R
# SOC500 Workshop
# John A. Bernau
# Internal examples
# -------------------------------------------------------
require(tidyverse)
require(RColorBrewer)
require(scales)
require(ggthemes)
# 10 examples using built-in datasets
# combining data wrangling with ggplot grammar

# starwars --------------------------------------------------------------
starwars %>% 
  filter(species %in% c("Droid", "Human", "Wookiee", "Gungan", "Zabrak")) %>% 
  ggplot(aes(height, mass)) + 
  geom_point(aes(color = species), size = 5, alpha = 0.8) +
  labs(title = "Star Wars characters by height / mass") +
  theme_minimal()
    
starwars %>% 
  filter(mass < 1000, gender %in% c("male", "female")) %>% 
  ggplot(aes(height, mass)) + 
  geom_point(aes(color = species, shape = gender), size = 5) +
  scale_color_discrete(guide = F) +
  labs(title = "Star Wars characters by height / mass") +
  theme_clean()

starwars %>% 
  filter(eye_color %in% c("black", "blue", "brown", "yellow")) %>% 
  count(eye_color) %>% 
  ggplot(aes(eye_color, n)) +
  geom_bar(stat = "identity", aes(fill = eye_color)) +
  scale_fill_manual(name = NULL, values = c("black", "blue", "saddlebrown", "gold")) +
  labs(x = "Eye color", y = "N", title = "Star Wars characters by eye-color") +
  theme_minimal()

starwars %>% 
  filter(gender %in% c("male", "female")) %>% 
  ggplot(aes(height)) +
  geom_histogram(bins = 10, color = "white", fill = "darkred") +
  facet_wrap(~gender) +
  labs(x = "height", y = "N", title = "Star Wars characters", subtitle = "Height varies by gender") +
  theme_fivethirtyeight()

# storms --------------------------------------------------------------
storms %>% 
  ggplot(aes(lat, long)) + 
  geom_point(alpha = 0.2) +
  labs(title = "Tropical storm coordinates") +
  facet_wrap(~year)

storms %>% 
  filter(status == "hurricane") %>% 
  ggplot(aes(wind, pressure)) + 
  geom_point(aes(color = category)) +
  scale_color_brewer(palette = "Blues") +
  labs(y = "Air pressure at center (millibars)",
       x = "Max wind speed (knots)",
       title = "Hurricane wind / pressure") +
  theme_minimal()

# PS refine color scale
blue_colors <- brewer_pal(palette = "Blues")(7)
storms %>% 
  filter(status == "hurricane") %>% 
  ggplot(aes(wind, pressure)) + 
  geom_point(aes(color = category)) +
  scale_color_manual(values = blue_colors[3:7]) +
  labs(y = "Air pressure at center (millibars)",
       x = "Max wind speed (knots)",
       title = "Hurricane wind / pressure") +
  theme_minimal()

# PS refine color scale again
emphasize_c3 <- c(blue_colors[3:4], "red", blue_colors[6:7])
storms %>% 
  filter(status == "hurricane") %>% 
  ggplot(aes(wind, pressure)) + 
  geom_point(aes(color = category)) +
  scale_color_manual(values = emphasize_c3) +
  labs(y = "Air pressure at center (millibars)",
       x = "Max wind speed (knots)",
       title = "Hurricane wind / pressure") +
  theme_minimal()

# mpg --------------------------------------------------------------
mpg %>% 
  ggplot(aes(cty, hwy)) + 
  geom_point(aes(color = cty), alpha = 0.7, size = 7, position = 'jitter') + 
  scale_color_continuous(low = "blue", high = "orange") +
  geom_text(aes(label = manufacturer)) +
  theme_minimal()

mpg %>% 
  ggplot(aes(cty, hwy)) + 
  geom_point(aes(color = cty), alpha = 0.7, size = 7, position = 'jitter') + 
  scale_color_continuous(low = "blue", high = "orange") +
  geom_text(aes(label = ifelse(cty > 25, manufacturer, '')), hjust = 1, nudge_x = 0.5, nudge_y = 0.5) +
  theme_minimal()
