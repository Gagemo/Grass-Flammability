################################################################################
################################################################################
#########################   Grass - Flammability   #############################
#########################         Max Temp         #############################
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

GRASS = read.csv("Data/Flammability Project - Temp.csv")

str(GRASS)
summary(GRASS)

GRASS$Species = as.character(GRASS$Species)
GRASS$Species = as.factor(GRASS$Species)
GRASS$T1 = as.numeric(GRASS$T1)
GRASS$T2 = as.numeric(GRASS$T2)

GRASS$Species = factor(GRASS$Species)


##### Filter for max temperature for T1 (Bottom Probe) and T2 (Top Probe) ######

MxT1 <- GRASS %>% 
  group_by(ID, Species) %>%                 
  summarise(MaxTemp = max(T1)) %>%
  arrange(ID, Species)

MxT2 <- GRASS %>% 
  group_by(ID, Species) %>%                 
  summarise(MaxTemp = max(T2)) %>%
  arrange(ID, Species)

GRASS <- cbind(MxT1, MxT2)
GRASS <- dplyr::select(GRASS, -ID...4, -Species...5)
colnames(GRASS) <- c("ID", "Species", "MaxT1", "MaxT2")


data = read.csv("Data/Flammability Project - Time.csv")

data <- cbind(GRASS, data)
data <- data[-c(52), ]
data$FB = as.numeric(data$FB)
data <- data[ , !duplicated(colnames(data))]
data <- data %>%
  dplyr::select("ID", "FB", "Species", "MaxT1", "MaxT2")

# Check Assumptions #
model  <- lm(MaxT1 ~ FB + Species, data = data)
cor.test(data$MaxT1, data$FB)

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

# Test for Significance #
anova_ = data %>% anova_test(MaxT1 ~ FB + Species) %>% 
  add_significance()
anova_

lm(formula = MaxT1 ~ FB + Species, data)

pwc <- data %>% 
  emmeans_test(
    MaxT1 ~ Species, covariate = FB,
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
anova <- aov(MaxT1 ~ FB + Species, data = data)

# Perform post-hoc tests on the Species factor
posthoc_results <- emmeans(anova, pairwise ~ Species)

# Generate compact letter displays for significance testing
cld_results <- multcomp::cld(posthoc_results$emmeans, Letters = letters, adjust = "tukey")
cld_results

# Convert cld_results to a data frame
cld_df <- as.data.frame(cld_results)

box = 
  ggplot(data, aes(x = Species, y = MaxT1, fill = Species)) +
  geom_boxplot() +
  geom_text(data = cld_df, aes(x = Species, label = .group, y = 925), size = 10) +
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
    legend.position = "none"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Max temperature (C)")
box

ggsave("Figures/Box_Max_T1.png", 
       width = 10, height = 7)

# Create the table
print(MEANS)
MEANS = as.data.frame(MEANS)
write.csv(MEANS, "Figures/TempT1.csv", row.names = FALSE)


################################################################################
################################################################################
################################ t2 (top probe) ################################
################################################################################
################################################################################

# Check Assumptions #
model  <- lm(MaxT2 ~ FB + Species, data = data)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Test for Significance #
anova_ = data %>% anova_test(MaxT2 ~ FB + Species) %>% 
  add_significance()
anova_

lm(formula = MaxT2 ~ FB + Species, data)

pwc <- data %>% 
  emmeans_test(
    MaxT2 ~ Species, covariate = FB,
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
anova <- aov(MaxT2 ~ FB + Species, data = data)

# Perform post-hoc tests on the Species factor
posthoc_results <- emmeans(anova, pairwise ~ Species)

# Generate compact letter displays for significance testing
cld_results <- multcomp::cld(posthoc_results$emmeans, Letters = letters, adjust = "tukey")
cld_results

# Convert cld_results to a data frame
cld_df <- as.data.frame(cld_results)

box = 
  ggplot(data, aes(x = Species, y = MaxT2, fill = Species)) +
  geom_boxplot() +
  geom_text(data = cld_df, aes(x = Species, label = .group, y = 925), size = 10) +
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
    legend.position = "none"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Max temperature (C)")
box

ggsave("Figures/Box_Max_T2.png", 
       width = 10, height = 7)

# Create the table
print(MEANS)
MEANS = as.data.frame(MEANS)
write.csv(MEANS, "Figures/TempT2.csv", row.names = FALSE)
