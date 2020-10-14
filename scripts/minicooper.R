# Tue Oct 13 16:27:38 2020 ------------------------------
# Data Visualization in R
# SOC500 Workshop
# John A. Bernau
# External examples
# -------------------------------------------------------
require(tidyverse)
require(readxl)
require(broom)


john_theme <- theme(text=element_text(size = 12, family="Times New Roman"),
                    axis.text = element_text(size = 10),
                    panel.background = element_rect(fill="white"),
                    panel.grid.minor = element_line(color="grey90"),
                    panel.grid.major = element_line(color="grey90"),
                    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                    plot.caption = element_text(size = 9))

# a few examples using "real-world" data

# atlanta mini-cooper market -----------------------------------------
# read data
mini <- read_excel(file.choose()) # mini.xlsx
mini$mile1000 <- mini$mileage/1000

# Descriptive plot
ggplot(mini, aes(mileage, price)) + 
  geom_point(aes(color = factor(year)), size =4, alpha = 0.8) +
  geom_smooth(method = "lm", se = F) +
  scale_color_discrete(name = "Year") +
  scale_y_continuous(limits = c(5000, 31000)) +
  labs(title = "Mini Cooper Market", x = "Mileage", y = "Price ($)") +
  theme_minimal()

# With separate lines
ggplot(mini, aes(mileage, price)) + 
  geom_point(aes(color = factor(year)), size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = F, fullrange = T, aes(color = factor(year))) +
  scale_color_discrete(name = "Year") +
  scale_y_continuous(limits = c(5000, 31000)) +
  labs(title = "Mini Cooper Market", x = "Mileage", y = "Price ($)") +
  john_theme


# Regression: model price with covariates
# run model
lm <- lm(price ~ mile1000 + year + sornos, data = mini)
# inspect model
summary(lm)
stargazer(lm, 
          covariate.labels = c("1000miles", "Year", "Sport"),
          type = "html", 
          out = "~/Desktop/mini.html")
lmoutput <- tidy(lm)
lmoutput$term[lmoutput$term == "sornoss"] <- "Sport model"
lmoutput$term[lmoutput$term == "mile1000"] <- "1000miles"
lmoutput$term[lmoutput$term == "year"] <- "Year"

# plot
lmoutput %>% 
  filter(term != "(Intercept)") %>% 
  # data and axes
  ggplot(aes(estimate, term)) +
  # geoms
  geom_point(aes(color = term), size = 5) +
  geom_errorbarh(aes(xmin = estimate - (std.error*1.96),
                     xmax = estimate + (std.error*1.96),
                     color = term),
                 height = 0) +
  geom_vline(xintercept = 0) +
  # scales
  scale_color_discrete(name = NULL) +
  # annotations
  labs(x = "OLS estimate", y = NULL, title = "Mini Cooper price predictions") +
  theme_minimal()

# Augment original data with predictions
mini2 <- augment(lm)
mini2$overpriced <- "no"
mini2$overpriced[mini2$.resid > 0 ] <- "yes"
# plot
mini2 %>% 
  ggplot(aes(mile1000, price)) +
  geom_point(aes(color = overpriced), size = 5, alpha = 0.8) +
  scale_color_manual(values = c("green", "red")) +
  labs(title = "Mini Cooper prices", subtitle = "Colored according to residuals",
       x = "1000miles", y = "Price ($)") +
  theme_minimal()
