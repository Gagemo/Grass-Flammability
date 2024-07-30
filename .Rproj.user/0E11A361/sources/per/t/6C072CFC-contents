################################################################################
################################################################################
#########################   data - Flammability   #############################
#########################      Fuel Bed  Height    #############################
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

data = read.csv("Data/Flammability Project - Time.csv")

str(data)
summary(data)

data$Species = as.character(data$Species)
data$FB = as.numeric(data$FB)

data$Species = factor(data$Species)
data <- data[-c(52), ]

data %>% anova_test(FB ~ Species)

# Check Assumptions #
model  <- lm(FB ~ Species, data = data)
# Inspect the model diagnostic metrics
model.metrics <- augment(model)
head(model.metrics, 3)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Compute Shapiro-Wilk test of normality
model.metrics %>% levene_test(.resid ~ Species)
plot(model, 1)

#Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

# Test for Significance #
anova_ = data %>% anova_test(FB ~ Species) %>% 
  add_significance()
anova_

anova <- 
  aov(FB ~ Species, data = data) %>% add_significance()
summary(anova)
capture.output(summary(anova), file="Figures/FuelBed.doc")

########################## Tukey Test - Multiple Comparisons ###################

tukey <-TukeyHSD(anova) 
tukey

tukey_ <- data %>% 
  tukey_hsd(FB ~ Species) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_

HSD = HSD.test(anova, trt = c("Species"))
HSD

## SIGNIFICANCE: SUGARCANE: BX VS A1 --- INDIAN: BX VS A1 ##
## GROWTH HEIGHT WAS SIGNIFICANTLY AFFECTED BY SOIL   ##

tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

dt <- data %>% 
  group_by(Species) %>%
  summarise(w=mean(exp(FB)), 
            sd = sd(exp(FB)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() 

# extracting the compact letter display and adding to the Tk table
cld2 <- data.frame(letters = tukey.cld$`Species`$Letters)
dt$tukey.cld <- cld2$letters

# The palette without black:
cbbPalette <- c("#BE0032", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

fb_box = 
  ggplot(data, aes(x = Species, y = FB, fill = Species)) + 
  geom_boxplot() +
  geom_point(shape=16, show.legend = FALSE, size =2) +
  geom_text(data = dt, aes(label = tukey.cld, y = 20), size=10, vjust = 0.5) +
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
  ylab("Fuel bed height (cm)")
fb_box

ggsave("Figures/FB_Height.png", 
       width = 10, height = 7)

tmp <- tabular(Species ~ FB * (mean+sd+std.error), data=data)
tmp

write.csv.tabular(tmp, "Figures/FB.csv")
