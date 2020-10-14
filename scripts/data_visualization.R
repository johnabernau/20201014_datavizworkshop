# Tue Oct 13 16:27:38 2020 ------------------------------
# Data Visualization in R
# SOC500 Workshop
# John A. Bernau
# Visualizing data with ggplot2
# -------------------------------------------------------

# setup
require(tidyverse)

# built-in dataset in ggplot2
diamonds
mpg
dsample <- diamonds %>% sample_n(5000)

# ggplot2 builds plots using additive layers

# (most) every plot has four layers:
# data and axes
# geoms
# scales
# annotations
# theme



# building a basic plot layer-by-layer

# data and axes
ggplot(dsample, aes(x = carat, y = price))
# geom
ggplot(dsample, aes(x = carat, y = price)) + geom_point()
# annotations
ggplot(dsample, aes(x = carat, y = price)) + geom_point() + labs(x = "Carat", y = "Price", title = "Figure 1: Diamond Prices")
# theme
ggplot(dsample, aes(x = carat, y = price)) + 
  geom_point() + 
  labs(x = "Carat", y = "Price", title = "Figure 1: Diamond Prices") + 
  theme_minimal()


# data and axes ------------------------------------------------
# data can come fromm end of pipe function
ggplot(starwars, aes(height, mass)) + geom_point()

starwars %>% 
  filter(gender == "male") %>% 
  mutate(feet = height / 30.48) %>% 
  ggplot(aes(feet, mass)) + geom_point()

# x and y axes (position) reserved for most important variables
# other features (size, shape, color, etc.) add detail


# geom --------------------------------------------------------
# many geoms available: 
# dots, lines, histogram, bars, text, boxplot, violinplot, abline, hline, vline, etc. 
#ggplot(dsample, aes(x = carat, y = price)) + geom_
# each have own attributes to tweak appearance (color, size, shape, fill, etc.)
# start with scatterplot using geom_point()

# size, shape, alpha
ggplot(dsample, aes(x = carat, y = price)) + geom_point()
ggplot(dsample, aes(x = carat, y = price)) + geom_point(size = 7)
ggplot(dsample, aes(x = carat, y = price)) + geom_point(shape = 8)
ggplot(dsample, aes(x = carat, y = price)) + geom_point(alpha = 0.2)

# color
ggplot(dsample, aes(carat, price)) + geom_point(color = "red")
ggplot(dsample, aes(carat, price)) + geom_point(color = "saddlebrown")
ggplot(dsample, aes(carat, price)) + geom_point(color = rgb(.5,.7,.6))
ggplot(dsample, aes(carat, price)) + geom_point(color = rgb(67,96,156, max = 256))
ggplot(dsample, aes(carat, price)) + geom_point(color="#582C81")

# when using geom attributes to express other variables, use aes()
ggplot(dsample, aes(carat, price)) + geom_point(aes(size = carat))
ggplot(dsample, aes(carat, price)) + geom_point(aes(shape = cut))
ggplot(dsample, aes(carat, price)) + geom_point(aes(alpha = price))
ggplot(dsample, aes(carat, price)) + geom_point(aes(color = price))
ggplot(dsample, aes(carat, price)) + geom_point(aes(color = clarity))

# scales ------------------------------------------------------
# adjust scale attributes 

# alpha
ggplot(dsample, aes(carat, price)) + geom_point(aes(alpha = price)) +
  scale_alpha_continuous(name = "Alpha title here", range = c(0.01, 1))

# color continuous
ggplot(dsample, aes(carat, price)) + 
  geom_point(aes(color = price)) +
  scale_color_gradient(name = "Title here", low = "darkblue", high = "orange")

# color discrete
# install.packages("RColorBrewer")
require(RColorBrewer)
display.brewer.all()
ggplot(dsample, aes(carat, price)) + geom_point(aes(color = clarity)) +
  scale_color_brewer(name = "Clarity", palette = "RdYlGn")

# y axis
ggplot(dsample, aes(carat, price)) + geom_point(aes(color = clarity)) +
  scale_color_brewer(name = "Clarity", palette = "RdYlGn") +
  scale_y_log10()

# annotations -------------------------------------------------
ggplot(dsample, aes(carat, price)) + 
  geom_point(aes(color = clarity)) +
  scale_color_brewer(name = "Clarity", palette = "RdYlGn") +
  scale_y_log10() +
  labs(x = "Carat", y = "Price (log10)",
       title = "Figure 1: Diamond Prices",
       subtitle = "Larger diamonds are expensive",
       caption = "Source: Subset of ggplot2 dataset (N = 5000)")

# theme ---------------------------------------------------------
graph1 <- ggplot(dsample, aes(carat, price)) + 
  geom_point(aes(color = clarity)) +
  scale_color_brewer(name = "Clarity", palette = "RdYlGn") +
  scale_y_log10() +
  labs(x = "Carat", y = "Price (log10)",
       title = "Figure 1: Diamond Prices",
       subtitle = "Larger diamonds are expensive",
       caption = "Source: Subset of ggplot2 dataset (N = 5000)")

# built-in themes
graph1 + theme_bw() 
graph1 + theme_gray() # This is default
graph1 + theme_dark()
graph1 + theme_classic()
graph1 + theme_light()
graph1 + theme_linedraw()
graph1 + theme_minimal()
graph1 + theme_void()


# ggthemes: https://jrnold.github.io/ggthemes/reference/index.html
# install.packages("ggthemes")
require(ggthemes)
graph1 + theme_tufte()
graph1 + theme_economist()
graph1 + theme_wsj()
graph1 + theme_stata()


# you can specify theme elements manually and save for future use
# https://ggplot2.tidyverse.org/reference/theme.html
john_theme <- theme(text = element_text(size = 14, family="Times New Roman"),
                    axis.text = element_text(size = 14),
                    panel.background = element_rect(fill="white"),
                    panel.grid.minor = element_line(color="grey90"),
                    panel.grid.major = element_line(color="grey90"),
                    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                    plot.caption = element_text(size = 9))

graph1 + john_theme

# EXERCISES ---------------------------------------

# DV EX1
# Use the “mpg” dataset (built-in) to make a scatterplot of “city mpg” and “highway mpg” (two quantitative variables in the dataset). Change axes labels and give your plot a title. Change the points to dark blue triangles and change size to 6 and opacity to 0.3.


# DV EX2
# Use the “mtcars” dataset (built-in) to make a scatterplot of “weight” and “mpg”. Change axes labels and give your plot a title. Set the alpha of all points to 0.7.
# 
# Set the color of the points to the “mpg” variable (continuous). Also, set this color parameter so that high mpgs are colored green and low mpgs are colored blue. Title the legend “MPG”.
# 
# Set the size of the points to the “wt” variable (continuous). Also, to make the small points more readable, set the range of sizes to go from 3-10. Title this legend “Weight”.


