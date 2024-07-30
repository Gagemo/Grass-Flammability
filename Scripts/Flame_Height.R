################################################################################
################################################################################
#########################   Data - Flammability   #############################
#########################      Flame Height        #############################
#########################  University of Florida   #############################
#########################     Gage LaPierre        #############################
#########################          2023            #############################
################################################################################
################################################################################

######################### Clears Environment & History  ########################

rm(list=ls(all=TRUE))
cat("\014") 
#
#########################     Installs Packages   ##############################

list.of.packages <- c("tidyverse", "vegan", "agricolae", "tables", "plotrix",
                      "ggpubr", "rstatix", "multcompView", "emmeans")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################     Loads Packages     ##############################

library(tidyverse)
library(vegan)
library(agricolae)
library(tables)
library(plotrix)
library(ggpubr)
library(rstatix)
library(multcompView)
library(emmeans)

##########################     Read in Data  ###################################

Data = read.csv("Data/Flammability Project - Time.csv")

str(Data)
summary(Data)

Data$Species = as.character(Data$Species)
Data$Max_Height = as.numeric(Data$Max_Height)
Data$FB = as.numeric(Data$FB)

Data$Species = factor(Data$Species)

Data %>% anova_test(Max_Height ~ FB + Species)
cor.test(Data$Max_Height, Data$FB)

# Check Assumptions #
model  <- lm(Max_Height ~ FB + Species, data = Data)
# Inspect the model diagnostic metrics
model.metrics <- augment(model)
head(model.metrics, 3)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

model.metrics %>% levene_test(.resid ~ Species)
plot(model, 1)

#Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

# Test for Significance #
anova_ = Data %>% anova_test(Max_Height ~ FB + Species) %>% 
  add_significance()
anova_

pwc <- Data %>% 
  emmeans_test(
    Max_Height ~ Species, covariate = FB,
    p.adjust.method = "bonferroni"
  )
pwc

MEANS = get_emmeans(pwc)

# Visualization: line plots with p-values
pwc <- pwc %>% add_xy_position(x = "Species", fun = "mean_se")
ggline(get_emmeans(pwc), x = "Species", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(anova_, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Fit the ANOVA model
anova <- aov(Max_Height ~ FB + Species, data = Data)

# Perform post-hoc tests on the Species factor
posthoc_results <- emmeans(anova, pairwise ~ Species)

# Generate compact letter displays for significance testing
cld_results <- multcomp::cld(posthoc_results$emmeans, Letters = letters, adjust = "tukey")
cld_results

# Convert cld_results to a data frame
cld_df <- as.data.frame(cld_results)

# Merge with the original data
fuel_data <- merge(Data, cld_df, by = "Species")

# The palette without black:
cbbPalette <- c("#BE0032", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

box = 
  ggplot(Data, aes(x = Species, y = Max_Height, fill = Species)) +
  geom_boxplot() +
  geom_text(data = cld_df, aes(x = Species, label = .group, y = 60), size = 10) +
  geom_point(shape = 16, show.legend = FALSE, size = 2) +
  labs(subtitle = get_test_label(anova_, detailed = TRUE)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", colour = "black"),
    text = element_text(size = 16),
    axis.title.x = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 20, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 18, face = "italic", color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 18, face = "bold", color = "black"),
    strip.text = element_text(color = "black", size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18),
    axis.ticks = element_line(size = 1.25),  # Adjusted size here
    legend.position = "none") +
  scale_fill_manual(values = cbbPalette) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Max flame height (cm)")
box

ggsave("Figures/Box_FlameHeight.png", 
       width = 10, height = 7)

# Create the table
print(MEANS)
MEANS = as.data.frame(MEANS)
write.csv(MEANS, "Figures/FlameHeight.csv", row.names = FALSE)

