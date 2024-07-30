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
  dplyr::select(Species, Flame_Total, Smld_Total, Max_Height, Mass_Loss)
colnames(data) <- c("Species", "Flame Duration", "Smolder Time", "Flame Height", "Mass Loss")

# Extract Species column for later use
species <- data$Species

# Remove the Species column for clustering
data_no_species <- data[, -1]

# Perform K-Means clustering with 3 clusters
set.seed(123)
kmeans_result <- kmeans(data_no_species, centers = 3, nstart = 20)

# View the result
print(kmeans_result)

# Add the cluster assignment and species back to the original data
data$Cluster <- as.factor(kmeans_result$cluster)

# Plot the clusters with species labels
ggplot(data, aes(x = `Flame Duration`, y = `Flame Height`, 
                 color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering of Grass Flammability Data",
       x = "Flame Duration", y = "Flame Height") +
  theme_minimal()

