# Tue Oct 13 16:27:38 2020 ------------------------------
# Data Visualization in R
# SOC500 Workshop
# John A. Bernau
# Visualizing data with ggplot2
# -------------------------------------------------------
require(tidyverse)

# EXERCISE ANSWERS --------------------------------------


# DV EX1
# Use the “mpg” dataset (built-in) to make a scatterplot of “city mpg” and “highway mpg” (two quantitative variables in the dataset). Change axes labels and give your plot a title. Change the points to dark blue triangles and change size to 6 and opacity to 0.3.

ggplot(mpg, aes(cty, hwy)) + 
  geom_point(color="darkblue", shape=2, size=6, alpha=0.3) +
  labs(x="City MPG", y="Highway MPG", title="Fuel Economy")


# DV EX2
# Use the “mtcars” dataset (built-in) to make a scatterplot of “weight” and “mpg”. Change axes labels and give your plot a title. Set the alpha of all points to 0.7.
# 
# Set the color of the points to the “mpg” variable (continuous). Also, set this color parameter so that high mpgs are colored green and low mpgs are colored blue. Title the legend “MPG”.
# 
# Set the size of the points to the “wt” variable (continuous). Also, to make the small points more readable, set the range of sizes to go from 3-10. Title this legend “Weight”.


ggplot(mtcars, aes(wt, mpg)) + 
  geom_point(alpha=0.7, aes(size=wt, color=mpg)) +
  scale_size_continuous(name="Weight", range=c(3,10)) +
  scale_color_gradient(name="MPG", low="blue", high="green") +
  labs(x="Weight in lbs (x 1000)", y="MPG", title="Fuel efficiency")

