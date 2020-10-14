# Tue Oct 13 16:27:38 2020 ------------------------------
# Data Visualization in R
# SOC500 Workshop
# John A. Bernau
# External examples
# -------------------------------------------------------

#######################################
# Text Analysis with JSTOR Archives
# John A. Bernau 2018
#######################################
require(broom)
require(ggplot2)
require(RColorBrewer)
require(scales)
require(gridExtra)
require(stringr)
require(dplyr)
require(tidyr)

##############################################################################
# American Sociological Rreview - keyword search
##############################################################################

# # "Holy Trinity" search
# asr_arts$race <- str_count(asr_arts$text, "race")
# asr_arts$class <- str_count(asr_arts$text, "class")
# asr_arts$gender <- str_count(asr_arts$text, "gender")
# 
# asr_smaller <- asr_arts %>% 
#   select(year, race, class, gender)

load(file.choose()) # find "asr_smaller.RData"

# Create yearly totals
ht_plot <- asr_smaller %>% 
  group_by(year) %>% 
  summarize(race = sum(race),
            class = sum(class),
            gender = sum(gender)) %>% 
  ungroup() %>% 
  gather("ht", "n", race:gender)

ht_plot_p <- filter(ht_plot, n > 0)

# Save variable as factor
ht_plot_p$ht <- factor(ht_plot_p$ht, levels = c("class", "race", "gender"))

# Choosing colors
colors <- hue_pal()(3)
colors2 <- c(colors[2], colors[3], colors[1])

# Final Tweaking
asr_ht <- ggplot(ht_plot_p, aes(year, log2(n))) +
  # geoms
  geom_point(aes(color = ht, alpha = n), size = 3) +
  geom_smooth(aes(color = ht), se = F, size = 2) +
  # scales
  scale_alpha_continuous(range = c(0.3, 0.8), guide = F) +
  scale_color_manual(values = colors2, name = "") +
  scale_y_continuous(breaks = seq(0, 13, by = 1), 
                     labels = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192))  +
  scale_x_continuous(breaks = seq(1900, 2000, by = 20)) +
  # annotations
  labs(title = "American Sociological Review", subtitle = "Word frequencies per year (log2 scale)", y = "Yearly Mentions", x = NULL, caption = "") +
  theme(text=element_text(size = 14, family="Times New Roman"),
        plot.subtitle = element_text(size = 12),
        #legend.position = "bottom",
        legend.title=element_blank(),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill="white"),
        panel.grid.minor = element_line(color="grey90"),
        panel.grid.major = element_line(color="grey90"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = c(.9,.5))
asr_ht


########################################################
# American Journal of Sociology - Page length as function of date * author 
########################################################
load(file.choose()) # find ajs_smaller.RData

# Create authorship variable from number of author columns != NA
ajs_smaller$nauth <- 1
ajs_smaller$nauth[!is.na(ajs_smaller$auth2)] <- 2
ajs_smaller$nauth[!is.na(ajs_smaller$auth3)] <- 3
ajs_smaller$nauth[!is.na(ajs_smaller$auth4)] <- 4
ajs_smaller$nauth <- as.integer(ajs_smaller$nauth)
ajs_smaller$title <- as.character(ajs_smaller$title)

# Run linear model with interaction term
ajs_mod <- lm(length ~ year + factor(nauth) + year:factor(nauth), data = ajs_smaller)
# Save R-squared
rsq <- summary(ajs_mod)$r.squared
# Add to existing data
ajs_res <- augment(ajs_mod)

ajs_res$.resid <- ajs_res$length - ajs_res$.fitted
# Choose colors
colz <- brewer.pal(8, "Blues")
colz <- colz[5:8]

# Plot results
inter <- ggplot(ajs_res, aes(year, length)) +
  # geoms
  geom_point(position = "jitter", aes(alpha = .resid, fill = .resid, size = .resid), pch = 21) +
  geom_line(size = 2, aes(y = .fitted, color = `factor(nauth)`)) +
  # scales
  scale_size(guide = F) +
  scale_alpha(range = c(0.3, 0.8), guide = F) +
  scale_fill_continuous(low = "black", high = "red", guide = F) +
  scale_color_manual(values = colz, name = "Authors") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  scale_x_continuous(limits = c(1897, 2015)) +
  # annotations
  labs(title = "American Journal of Sociology", subtitle = "Article length as function of \"date x author\"", caption = paste("R-Squared =", round(rsq, 2)), x = NULL, y = "Page Length") +
  theme(text=element_text(size = 14, family="Times New Roman"),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill="white"),
        panel.grid.minor = element_line(color="grey90"),
        panel.grid.major = element_line(color="grey90"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  geom_text(aes(label = ifelse(length>125, "Albion Small (1916, 21:6)", "")), 
            nudge_x = 43)

inter



g <- grid.arrange(asr_ht, inter, ncol = 2)

ggsave(g, file = "~/Desktop/socius.tiff", width = 10, height = 5, units = "in", dpi = 1200)

ggsave(g, file = "~/Desktop/socius.tiff", width = 10, height = 5, units = "in", dpi = 600)
