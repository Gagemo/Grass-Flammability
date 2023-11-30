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

list.of.packages <- c("tidyverse", "vegan", "agricolae")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################     Loads Packages     ##############################

library(tidyverse)
library(vegan)
library(agricolae)

##########################     Read in Data  ###################################

GRASS = read.csv("Data/Flammability Project - Temp.csv")
weight = read.csv("Data/Flammability Project - Weight.csv")

str(GRASS)
summary(GRASS)

##### Filter for max temperature for T1 (Bottom Probe) and T2 (Top Probe) ######

MxT1 <- GRASS %>% 
  group_by(ID, Species) %>%                 
  summarise(MaxTemp = max(T1)) %>%
  arrange(ID, Species)

MxT2 <- GRASS %>% 
  group_by(ID, Species) %>%                 
  summarise(MaxTemp = max(T2)) %>%
  arrange(ID, Species)

MxT1 <- full_join(MxT1, weight, by=c("ID", "Species"))

## Max Temp1 ##

boxT1 = 
  ggplot(MxT1, aes(x = Species, y = MaxTemp, fill = Species)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(size=3, alpha = 0.5, color="black", width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=16,  family = "Roboto Mono"))+
  theme_classic() 
boxT1

ggsave("Figures/Box_MaxTemp1.png", 
       width = 10, height = 7)

# Test for Significances#
anova = aov(MaxTemp ~ Species+Hood_Humidity+Hood_Temp, data = MxT1)
summary(anova)
tukey.one.way<-TukeyHSD(anova)
tukey.one.way

## Max Temp2 ##

boxT2 = 
  ggplot(MxT2, aes(x = Species, y = MaxTemp, fill = Species)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(size=3, alpha = 0.5, color="black", width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=16,  family = "Roboto Mono"))+
  theme_classic() 
boxT2

ggsave("Figures/Box_MaxTemp2.png", 
       width = 10, height = 7)

# Test for Significances#
anova = aov(MaxTemp ~ Species+Humidity, data = MxT2)
summary(anova)
tukey.one.way<-TukeyHSD(anova)
tukey.one.way


head(MxT1)


sd(MxT1$MaxTemp)/mean(MxT1$MaxTemp)
