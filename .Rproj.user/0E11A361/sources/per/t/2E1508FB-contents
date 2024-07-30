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
                      "ggpubr", "rstatix", "multcompView", "factoextra",
                      "FactoMineR")

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
library(factoextra)
library(FactoMineR)

##########################     Read in Data  ###################################

TIME <- read.csv("Data/Flammability Project - Time.csv")
LOSS <- read.csv("Data/Flammability Project - Weight.csv")

data <- cbind(TIME, LOSS)
data$FB = as.numeric(data$FB)
data <- data[ , !duplicated(colnames(data))]
data <- data %>%
  dplyr::select(Flame_Total, Smld_Total, Max_Height, Mass_Loss, Mass_Rate)
colnames(data) <- c("Flame Duration", "Smolder Time", 
                    "Flame Height", "Mass Loss", "Mass Rate")

data$Species <- TIME$Species
data$Status <- TIME$Ruderal

pca <- data %>% dplyr::select(-Species, -Status) %>%
  rda(scale = TRUE)  # Remove 'Species' and 'Status' columns for PCA

pca_scores <- as.data.frame(vegan::scores(pca, choices = c(1, 2), display = "sites"))
pca_scores$Species <- data$Species  # Add species information to scores
pca_scores$Status <- data$Status    # Add status information to scores

# Extract the loadings for plotting arrows
loadings <- as.data.frame(vegan::scores(pca, display = "species"))
loadings$Flammability <- rownames(loadings)

# The palette without black:
cbbPalette <- c("#BE0032", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = Species), shape = 21, size = 8) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 0.5, yend = PC2 * 0.5),
               arrow = arrow(length = unit(0.3, "cm")), size = 1.5, color = "black") +
  geom_text(data = loadings, aes(x = PC1 * 0.5, y = PC2 * 0.5, label = Flammability),
            color = "black", vjust = -0.5, hjust = 0.5, size = 8) +
  xlab(paste("PC1 (", round(summary(pca)$cont$importance[2, 1] * 100, 1), "%)", sep = "")) +
  ylab(paste("PC2 (", round(summary(pca)$cont$importance[2, 2] * 100, 1), "%)", sep = "")) +
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

ggsave("Figures/Box_PCA.png", 
       width = 19, height = 12)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Status)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 2, yend = PC2 * 2),
               arrow = arrow(length = unit(0.3, "cm")), color = "red") +
  geom_text(data = loadings, aes(x = PC1 * 2, y = PC2 * 2, label = Flammability),
            color = "red", vjust = -0.5, hjust = 0.5) +
  stat_ellipse(aes(color = Status), type = "norm", level = 0.95) +  # Add ellipses
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
  scale_fill_manual(values = cbbPalette) +
  theme(legend.position = "bottom")
 
ggsave("Figures/Box_PCA_ruderal.png", 
       width = 10, height = 7)

write.table(loadings, file = "Figures/loadings.csv", sep=",")

res.pca <- data %>% dplyr::select(-Species, -Status) %>% PCA(graph = FALSE)

var <- get_pca_var(res.pca)
var

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
