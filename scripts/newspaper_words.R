# Tue Oct 13 16:27:38 2020 ------------------------------
# Data Visualization in R
# SOC500 Workshop
# John A. Bernau
# External examples
# -------------------------------------------------------
require(tidyverse)

john_theme <- theme(text=element_text(size = 12, family="Times New Roman"),
                    axis.text = element_text(size = 10),
                    panel.background = element_rect(fill="white"),
                    panel.grid.minor = element_line(color="grey90"),
                    panel.grid.major = element_line(color="grey90"),
                    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
                    plot.caption = element_text(size = 9))

load(file.choose()) # find newspaper_wf.RData
names(results)
# what are we looking at?
# welch's t-test difference in means
# estimate 1 is ADW (black press)
# estimate 2 is AJC (mainstream press)
# p.value to see if this difference in means is significant

# clean dataset
results$sig <- NA
results$sig[results$p.value < 0.05] <- "y"
results$sig[results$p.value > 0.05] <- "n"
results$word <- str_to_title(gsub("_", " ", results$word))
results$color <- NA
results$color[results$estimate < 0] <- "AJC (Mainstream Press)"
results$color[results$estimate > 0] <- "ADW (Black Press)"
results$color[results$sig == "n"] <- "p > 0.05"
results$color <- factor(results$color, 
                        levels = c("ADW (Black Press)", "AJC (Mainstream Press)", "p > 0.05"))


# plot
results %>% 
  ggplot(aes(estimate, word)) + 
  # geoms
  geom_text(aes(label = word, color = color, size = abs(estimate))) +
  geom_vline(xintercept = 0) +
  # scales
  scale_color_manual(values = c("#00BA38", "#619CFF", "grey60"),
                     name = NULL) +
  scale_x_continuous(limits = c(-3.5, 3)) +
  scale_size_continuous(range = c(4, 7), guide = F) +
  facet_wrap(~frame, scales = "free_y") +
  # annotations
  labs(x = "Difference in means (Welch's t-test)", y = NULL,
       title = "Word frequency distributions across newspapers", 
       subtitle = "Difference in means = AJC - ADW") +
  john_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# save
ggsave(filename = "./20200929/wf_plot_short.jpg", width = 8.5, height = 8, units = "in", dpi = 300)



# THE SHORT WAY (end) ---------------------------------------------------
# Tue Sep 29 11:40:02 2020 ------------------------------