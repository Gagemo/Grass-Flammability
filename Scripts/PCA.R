################################################################################
################################################################################
#########################      Grass - Flammability      ###########################
#########################              PCA             ###########################
#########################      University of Florida       #######################
#########################         Gage LaPierre          #######################
#########################               2023               #######################
################################################################################
################################################################################

######################### Clears Environment & History #########################

rm(list=ls(all=TRUE))
cat("\014")

#########################      Installs Packages      ###########################
# This code checks if the necessary packages are installed. If not, it installs
# them automatically to ensure the script runs smoothly.
# The `janitor`, `ggdendro` and `ggfortify` packages have been added.

list.of.packages <- c("tidyverse", "vegan", "agricolae", "tables", "plotrix",
                      "ggpubr", "rstatix", "multcompView", "factoextra",
                      "FactoMineR", "janitor", "ggdendro", "ggfortify")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################      Loads Packages       ###########################

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
library(janitor)
library(ggdendro)
library(ggfortify)

##########################        Read in Data       ###########################
# Read in all three CSV files, correcting the file paths.
# We will merge the dataframes by their `ID` column for robustness.

TIME_raw <- read.csv("Data/Flammability Project - Time.csv")
LOSS_raw <- read.csv("Data/Flammability Project - Weight.csv")
TEMP_raw <- read.csv("Data/Flammability Project - Temp.csv")

# Clean column names using janitor::clean_names()
TIME_clean <- TIME_raw %>% clean_names()
LOSS_clean <- LOSS_raw %>% clean_names()
TEMP_clean <- TEMP_raw %>% clean_names()

# Aggregate the temperature data by taking the maximum temperature for each ID.
# The previous version had a coercion warning. We'll explicitly convert
# to numeric first to handle any non-numeric values gracefully.
data_temp <- TEMP_clean %>%
  mutate(t1 = as.numeric(t1),
         t2 = as.numeric(t2)) %>%
  filter(!is.na(t1) & !is.na(t2)) %>%
  group_by(id) %>%
  summarise(Temp_Fuel_Bed = max(t1, na.rm = TRUE),
            Temp_10cm_Above = max(t2, na.rm = TRUE),
            .groups = 'drop')

# Merge the dataframes into a single data frame based on the 'id' column
# We first select the columns we need from each dataframe to avoid duplicate
# column names like `species.x` and `species.y`.
data_final <- TIME_clean %>%
  dplyr::select(id, species, ruderal, fb, max_height, flame_total, smld_total) %>%
  inner_join(dplyr::select(LOSS_clean, id, mass_loss, mass_rate), by = "id") %>%
  inner_join(data_temp, by = "id")

# Define the species order explicitly once
species_order <- c("Aristida beyrichiana",
                   "Andropogon ternarius",
                   "Andropogon virginicus",
                   "Andropogon glomeratus",
                   "Sorghastrum secundum",
                   "Schizachyrium stoloniferum",
                   "Sporobolus junceus",
                   "Eustachys petraea",
                   "Eragrostis spectabilis")

# Ensure correct factor levels for all plots
data_final$species <- factor(data_final$species, levels = species_order)

# Now, we will create the dataset for the PCA.
# The `max_temp` variables (Temp_Fuel_Bed and Temp_10cm_Above) are now included.
pca_data <- data_final %>%
  dplyr::select(
    flame_total,
    smld_total,
    max_height,
    mass_loss,
    mass_rate,
    Temp_Fuel_Bed,
    Temp_10cm_Above
  )

# Rename columns for better readability in the plot
colnames(pca_data) <- c(
  "Flame Duration",
  "Smolder Time",
  "Flame Height",
  "Mass Loss",
  "Mass Rate",
  "Fuel Bed Max Temp",
  "10cm Max Temp"
)

# Perform the PCA using the rda() function from the vegan package
pca <- pca_data %>%
  rda(scale = TRUE)

# Extract the scores for the sites (data points)
pca_scores <- as.data.frame(vegan::scores(pca, choices = c(1, 2), display = "sites"))
pca_scores$Species <- data_final$species  # Add species information
pca_scores$Status <- data_final$ruderal    # Add status information

# Extract the loadings for plotting arrows
loadings <- as.data.frame(vegan::scores(pca, display = "species"))
loadings$Flammability <- rownames(loadings)

# Define a consistent color palette for all plots
cbbPalette <- c("#BE0032", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

# Plot 1: PCA biplot colored by Species
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

# Plot 2: PCA biplot colored by Ruderal Status with ellipses
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Status)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 2, yend = PC2 * 2),
               arrow = arrow(length = unit(0.3, "cm")), color = "red") +
  geom_text(data = loadings, aes(x = PC1 * 2, y = PC2 * 2, label = Flammability),
            color = "red", vjust = -0.5, hjust = 0.5) +
  stat_ellipse(aes(color = Status), type = "norm", level = 0.95) +
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

res.pca <- pca_data %>% PCA(graph = FALSE)

var <- get_pca_var(res.pca)
var

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


################################################################################
#####################   NEW: Hierarchical Cluster Analysis  ####################
################################################################################
# The reviewer requested a cluster analysis to see how species group based on
# their overall flammability profiles.

# We will use the same standardized data as for the PCA.
# First, we need to create a species-level average for each metric
species_flam_metrics <- data_final %>%
  dplyr::select(species, flame_total, smld_total, max_height,
                mass_loss, mass_rate, Temp_Fuel_Bed, Temp_10cm_Above) %>%
  group_by(species) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop') %>%
  column_to_rownames("species")

# Now, standardize the data to ensure each metric is weighted equally
scaled_species_metrics <- scale(species_flam_metrics)

# Calculate the distance matrix using Euclidean distance
distance_matrix <- dist(scaled_species_metrics, method = "euclidean")

# Perform hierarchical clustering using Ward's method
hc_result <- hclust(distance_matrix, method = "ward.D2")
hc_result

# Plot the dendrogram using the ggdendro package
# The dendrogram visually represents which species are most similar.
ggdendro::ggdendrogram(hc_result, rotate = TRUE, size = 2, color = as.factor(col)) +
  labs(title = "Hierarchical Cluster Analysis of Species Flammability Profiles",
       x = "Species",
       y = "Distance") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(size = 12, face = "italic", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

# Save the dendrogram figure
ggsave("Figures/Dendrogram.png",
       width = 10, height = 7)
