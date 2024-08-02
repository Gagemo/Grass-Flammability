################################################################################
################################################################################
#########################   Grass - Flammability   #############################
#########################          PCA             #############################
#########################  University of Florida   #############################
#########################     Gage LaPierre        #############################
#########################          2023            #############################
################################################################################
################################################################################

######################### Clears Environment & History  ########################

rm(list=ls(all=TRUE))
cat("\014") 

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

TIME <- read.csv("Data/Flammability Project - Time.csv")
LOSS <- read.csv("Data/Flammability Project - Weight.csv")

data <- cbind(TIME, LOSS)
data$FB = as.numeric(data$FB)
data <- data[ , !duplicated(colnames(data))]
data <- data %>%
  dplyr::select(Species, FB, Flame_Total, Smld_Total, Max_Height, Mass_Loss)
colnames(data) <- c("Species", "Fuel Bed Height", "Flame Duration", 
                    "Smolder Duration", "Flame Height", "Mass Loss")

# Fit linear models with fuel bed height as a covariate
flame_duration_model <- lm(`Flame Duration` ~ Species + `Fuel Bed Height`, data = data)
smolder_duration_model <- lm(`Smolder Duration` ~ Species + `Fuel Bed Height`, data = data)
flame_height_model <- lm(`Flame Height` ~ Species + `Fuel Bed Height`, data = data)
mass_loss_model <- lm(`Mass Loss` ~ Species + `Fuel Bed Height`, data = data)

# Calculate EMMs for each variable
flame_duration_emm <- emmeans(flame_duration_model, ~ Species)
smolder_duration_emm <- emmeans(smolder_duration_model, ~ Species)
flame_height_emm <- emmeans(flame_height_model, ~ Species)
mass_loss_emm <- emmeans(mass_loss_model, ~ Species)

# Summarize EMMs
flame_duration_summary <- summary(flame_duration_emm)
smolder_duration_summary <- summary(smolder_duration_emm)
flame_height_summary <- summary(flame_height_emm)
mass_loss_summary <- summary(mass_loss_emm)

# Combine the summaries into a single data frame
emm_results <- data.frame(
  Species = flame_duration_summary$Species,
  `Flame Duration EMM` = flame_duration_summary$emmean,
  `Smolder Duration EMM` = smolder_duration_summary$emmean,
  `Flame Height EMM` = flame_height_summary$emmean,
  `Mass Loss EMM` = mass_loss_summary$emmean
)

print(emm_results)


data <- emm_results %>%
  group_by(Species) %>%
  summarise(across(`Flame Duration`:`Smolder Duration`: `Flame Height`:`Mass Loss`, mean, na.rm = TRUE))

# The palette without black:
cbbPalette <- c("#BE0032", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

ggplot(emm_results, aes(x = Flame.Duration.EMM, y = Flame.Height.EMM)) +
  geom_point(aes(fill = Species), shape = 21, size = 8) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(face="bold", colour = "black"),    
        axis.title.y = element_text(face="bold", colour = "black"),   
        axis.text.x=element_text(size = 20, face = "bold", color = "black"),
        axis.text.y=element_text(size = 20, face = "bold", color = "black"),
        strip.text.x = element_text(size = 20, colour = "black", face = "bold"),
        legend.text = element_text(size = 20, face = "italic")) +  
  scale_fill_manual(values = cbbPalette, labels = function(x) str_wrap(x, width = 10)) +
  theme(legend.position = "bottom", legend.spacing.x = unit(1.0, 'cm'),
        legend.spacing.y = unit(1.0, 'cm'), legend.spacing = unit(1.0, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE))

ggplot(emm_results, aes(x = Smolder.Duration.EMM, y = Mass.Loss.EMM)) +
  geom_point(aes(fill = Species), shape = 21, size = 8) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(face="bold", colour = "black"),    
        axis.title.y = element_text(face="bold", colour = "black"),   
        axis.text.x=element_text(size = 20, face = "bold", color = "black"),
        axis.text.y=element_text(size = 20, face = "bold", color = "black"),
        strip.text.x = element_text(size = 20, colour = "black", face = "bold"),
        legend.text = element_text(size = 20, face = "italic")) +  
  scale_fill_manual(values = cbbPalette, labels = function(x) str_wrap(x, width = 10)) +
  theme(legend.position = "bottom", legend.spacing.x = unit(1.0, 'cm'),
        legend.spacing.y = unit(1.0, 'cm'), legend.spacing = unit(1.0, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE))
