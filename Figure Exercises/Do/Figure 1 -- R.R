########################################################
# Title: R Sample Figure Code
#
# Date Created: Sep 25th, 2023
# Purpose: Exercise in creating and customizing figures in R
# Author: Diana Horvath
########################################################

# Set Environment 
rm(list = ls()) #Clean up workspace
library(ggplot2)
library(dplyr)
library(tidyverse)
library(haven)

setwd("[INSERT WORKING DIRECTORY HERE]")

# Import .dta data file and create subset data frame
data <- read_dta("Data/baroda_0102_1obs.dta")
data <- data[, c("pre_tot", "post_tot", "bal")] 
data <- filter(data, !is.na(pre_tot), !is.na(post_tot))


# Calculate difference between pre- and post-test scores
data$diff_scores <- data$post_tot - data$pre_tot

# Create bins of total pre-test scores in intervals of 10
data$bins <- 10 * floor(data$pre_tot/10)

# Collapse data by bins and treatment status
collapse <- data %>% group_by(bins, bal) %>% 
  summarise(sd_scores = sd(diff_scores), 
            pre_tot = mean(pre_tot),
            diff_scores = mean(diff_scores), 
            n = n())

# Create confidence intervals
collapse$ub_score = (collapse$diff_scores - qt(p = .025, df = collapse$n-1)*(collapse$sd_scores / sqrt(collapse$n))) 
collapse$lb_score = (collapse$diff_scores + qt(p = .025, df = collapse$n-1)*(collapse$sd_scores / sqrt(collapse$n))) 


# Create figure 
figure <- ggplot(collapse, aes(x = pre_tot, y = diff_scores, size = n, color=as.factor(bal))) +
  geom_point(shape = 21) + scale_color_manual(values = c("#E45C24", "#2CAC9C")) + 
  geom_errorbar(aes(ymin = lb_score, ymax = ub_score), linewidth = .4, width = 1.5) +
  scale_color_discrete(labels=c("Non-Balsakhi", "Balsakhi"), name ="") +  
  scale_size(range = c(0, 8), guide = 'none') +
  scale_x_continuous(name = "Pre-test Scores", n.breaks = 6, limits = c(0,100), expand = c(0,0)) +
  scale_y_continuous(name = "Average Test Score Improvement (by Bin)", n.breaks = 4, expand = c(0,0)) +
  coord_cartesian(ylim=c(-12, 20)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
               axis.title = element_text(size =10), plot.caption = element_text(hjust = 0)) +
  labs(caption = "Note: Markers are scaled by the number of students in each bin. The average bin size is 421 students.")


ggsave('Figures/Figure 1 - R.png',figure, width = 8, height = 4)


