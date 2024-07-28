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

TIME <- read.csv("Data/Flammability Project - Time.csv")
LOSS <- read.csv("Data/Flammability Project - Weight.csv")

data <- cbind(TIME, LOSS)
data <- select(data, Flame_Total, Smld_Total, Max_Height, Mass_Loss)
colnames(data) <- c( "Flame Duration", "Smolder Time", "Flame Height", "Mass Loss")
data$Species <- TIME$Species  # Assuming the 'Species' column is in the TIME data frame

pca <- rda(data %>% select(-Species), scale = TRUE)  # Remove 'Species' column for PCA

pca_scores <- as.data.frame(vegan::scores(pca, choices = c(1, 2), display = "sites"))
pca_scores$Species <- data$Species  # Add species information to scores

# Extract the loadings for plotting arrows
loadings <- as.data.frame(vegan::scores(pca, display = "species"))
loadings$Flammability <- rownames(loadings)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 1, yend = PC2 * 1),
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  geom_text(data = loadings, aes(x = PC1 * 1, y = PC2 * 1, label = Flammability),
            color = "black", vjust = -0.5, hjust = 0.5) +
  xlab(paste("PC1 (", round(summary(pca)$cont$importance[2, 1] * 100, 1), "%)", sep = "")) +
  ylab(paste("PC2 (", round(summary(pca)$cont$importance[2, 2] * 100, 1), "%)", sep = "")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=18),
        axis.title.x = element_text(size=15, face="bold", colour = "black"),    
        axis.title.y = element_text(size=15, face="bold", colour = "black"),   
        axis.text.x=element_text(size=15, face = "bold", color = "black"),
        axis.text.y=element_text(size=15, face = "bold", color = "black"),
        strip.text.x = element_text(size = 15, colour = "black", face = "bold"),
        legend.text = element_text(face = "italic")) +  
  theme(legend.position = "bottom")
 
ggsave("Figures/Box_PCA.png", 
       width = 10, height = 7)