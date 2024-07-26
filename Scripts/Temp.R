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
                      "ggpubr", "rstatix", "multcompView")
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

##########################     Read in Data  ###################################

GRASS = read.csv("Data/Flammability Project - Temp.csv")

str(GRASS)
summary(GRASS)

GRASS$Species = as.character(GRASS$Species)
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
GRASS <- select(GRASS, -ID...4, -Species...5)

# Check Assumptions #
model  <- lm(MaxTemp...3 ~ Species...2, data = GRASS)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Compute Levene's Test
GRASS %>% levene_test(MaxTemp...3 ~ Species...2)

########################## Tukey Test - Multiple Comparisons ###################
anova <- 
  aov(MaxTemp...3 ~ Species...2, data = GRASS) %>% add_significance()
summary(anova)

tukey <-TukeyHSD(anova) 
tukey

HSD = HSD.test(anova, trt = c("Species"))
HSD

tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

dt <- GRASS %>% 
  group_by(Species...2) %>%
  summarise(w=mean(exp(MaxTemp...3)), 
            sd = sd(exp(MaxTemp...3)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() 

# extracting the compact letter display and adding to the Tk table
cld2 <- data.frame(letters = tukey.cld$`Species`$Letters)
dt$tukey.cld <- cld2$letters

################### Box Plot - Max Growth Heights ######################
box = 
  ggplot(GRASS, aes(x = Species...2, y = MaxTemp...3, fill = Species...2)) + 
  geom_boxplot(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2)  +
  geom_text(data = dt, aes(label = tukey.cld, y = 25), size=10, vjust = 0.5) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 15),
        axis.title.y =  element_text(margin = unit(c(0, 5, 0, 0), "mm"),
                                     size = 30, face="bold"),
        axis.text.y = element_text(size=30, face="bold", color = "black"),
        panel.background = element_rect(color=NA),
        plot.background = element_rect(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(color=NA),
        legend.box.background = element_rect(color=NA),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", size = 30, face="bold"),
        legend.text = element_text(color = "black", size = 30),
        strip.text = element_text(color = "black", size = 30, face="bold"),
        text = element_text(family = "sans")) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Max temperature (C)")
box

ggsave("Figures/Box_Max_T1.png", 
       width = 10, height = 7)

################################################################################
################################################################################
################################ t2 (top probe) ################################
################################################################################
################################################################################

# Check Assumptions #
model  <- lm(MaxTemp...6 ~ Species...2, data = GRASS)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Compute Levene's Test
GRASS %>% levene_test(MaxTemp...6 ~ Species...2)

########################## Tukey Test - Multiple Comparisons ###################
anova <- 
  aov(MaxTemp...6 ~ Species...2, data = GRASS) %>% add_significance()
summary(anova)

tukey <-TukeyHSD(anova) 
tukey

HSD = HSD.test(anova, trt = c("Species"))
HSD

tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

dt <- GRASS %>% 
  group_by(Species...2) %>%
  summarise(w=mean(exp(MaxTemp...6)), 
            sd = sd(exp(MaxTemp...6)) / sqrt(n())) %>%
  arrange(desc(w)) %>% 
  ungroup() 

# extracting the compact letter display and adding to the Tk table
cld2 <- data.frame(letters = tukey.cld$`Species`$Letters)
dt$tukey.cld <- cld2$letters

################### Box Plot - Max Growth Heights ######################
box = 
  ggplot(GRASS, aes(x = Species...2, y = MaxTemp...6, fill = Species...2)) + 
  geom_boxplot(size = 0.5, color="black", show.legend = FALSE) +
  geom_point(shape=16, show.legend = FALSE, size =2)  +
  geom_text(data = dt, aes(label = tukey.cld, y = 25), size=10, vjust = 0.5) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 15),
        axis.title.y =  element_text(margin = unit(c(0, 5, 0, 0), "mm"),
                                     size = 30, face="bold"),
        axis.text.y = element_text(size=30, face="bold", color = "black"),
        panel.background = element_rect(color=NA),
        plot.background = element_rect(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(color=NA),
        legend.box.background = element_rect(color=NA),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", size = 30, face="bold"),
        legend.text = element_text(color = "black", size = 30),
        strip.text = element_text(color = "black", size = 30, face="bold"),
        text = element_text(family = "sans")) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Max temperature (C)")
box

ggsave("Figures/Box_Max_T2.png", 
       width = 10, height = 7)
