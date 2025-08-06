# R Script for Cluster Analysis and Flammability Index

# Your existing script to load and prepare data...
# ... (all the code from your previous script up to the point of creating flam_metrics) ...
#########################      Installs Packages      ##########################
# This code checks if the necessary packages are installed. If not, it installs
# them automatically to ensure the script runs smoothly.
# The `janitor` and `openxlsx` packages have been added.

list.of.packages <- c("dplyr", "vegan", "multcomp", "janitor", "openxlsx", "ggdendro")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Install and load necessary packages
library(dplyr)
library(vegan)
library(multcomp)
library(janitor)
library(openxlsx)
library(ggplot2)
library(ggdendro)

# --- Step 1: Load and prepare the data ---
# (Your code for loading and merging data)

data_time_raw <- read.csv("Data/Flammability Project - Time.csv")
data_weight_raw <- read.csv("Data/Flammability Project - Weight.csv")
data_temp_raw <- read.csv("Data/Flammability Project - Temp.csv")

data_time_clean <- data_time_raw %>%
  clean_names() %>%
  dplyr::select(id, species, ruderal, fb, max_height, flame_total, smld_total) %>%
  rename(Fuel_Bed_Height = fb,
         Max_Flame_Height = max_height,
         Flame_Duration = flame_total,
         Smoldering_Duration = smld_total)

data_weight_clean <- data_weight_raw %>%
  clean_names() %>%
  dplyr::select(id, mass_loss, mass_rate) %>%
  rename(Mass_Loss = mass_loss,
         Mass_Loss_Rate = mass_rate)

data_temp_clean <- data_temp_raw %>%
  clean_names() %>%
  filter(!is.na(t1) & !is.na(t2)) %>%
  group_by(id) %>%
  summarise(Temp_Fuel_Bed = max(t1, na.rm = TRUE),
            Temp_10cm_Above = max(t2, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(Temp_Fuel_Bed = as.numeric(Temp_Fuel_Bed),
         Temp_10cm_Above = as.numeric(Temp_10cm_Above))

data_final <- data_time_clean %>%
  inner_join(data_weight_clean, by = "id") %>%
  inner_join(data_temp_clean, by = "id")

# Select the flammability metrics for the multivariate analyses.
flam_metrics <- data_final %>%
  dplyr::select(
    id,
    species,
    Fuel_Bed_Height,
    Flame_Duration,
    Smoldering_Duration,
    Mass_Loss,
    Mass_Loss_Rate,
    Max_Flame_Height,
    Temp_Fuel_Bed,
    Temp_10cm_Above
  ) %>%
  rename(
    Mass_Rate = Mass_Loss_Rate
  )

# Convert the Species column to a factor
flam_metrics$species <- as.factor(flam_metrics$species)
flam_metrics <- na.omit(flam_metrics)

# --- NEW: Step 2: Perform Cluster Analysis ---
cat("\n--- Performing Cluster Analysis on Flammability Metrics ---\n")

# Select the metrics for clustering, excluding Fuel Bed Height as it's a covariate
clustering_metrics <- flam_metrics %>%
  dplyr::select(Flame_Duration, Smoldering_Duration, Mass_Loss, Mass_Rate, Max_Flame_Height, Temp_Fuel_Bed, Temp_10cm_Above)

# Standardize the data
scaled_data <- scale(clustering_metrics)

# Calculate the Euclidean distance matrix
dist_matrix <- dist(scaled_data, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
# This can be saved to a file and included in the paper
dendro_data <- dendro_data(hc, type = "rectangle")
ggplot(segment(dendro_data)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = label(dendro_data), aes(x = x, y = y, label = label, hjust = 0),
            angle = 90, size = 3) +
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_flip() +
  theme_dendro() +
  labs(title = "Hierarchical Cluster Analysis of Flammability Metrics")

# --- NEW: Step 3: Create a Flammability Index (based on PCA) ---
cat("\n--- Calculating Flammability Index based on PCA ---\n")

# Select only the response variables (flammability metrics)
response_vars <- flam_metrics %>%
  dplyr::select(Flame_Duration, Smoldering_Duration, Mass_Loss, Mass_Rate, Max_Flame_Height, Temp_Fuel_Bed, Temp_10cm_Above)

# Perform PCA
pca_result <- prcomp(response_vars, scale = TRUE)

# The first principal component (PC1) often explains the largest amount of variance
# and can be used as a composite flammability index.
# Higher values of PC1 correspond to higher flammability characteristics.
# We will create a new data frame with species and their PC1 scores.
flammability_index_df <- flam_metrics %>%
  mutate(Flammability_Index = pca_result$x[,1]) %>%
  group_by(species) %>%
  summarise(
    Mean_Index = mean(Flammability_Index, na.rm = TRUE),
    SE_Index = sd(Flammability_Index, na.rm = TRUE) / sqrt(n())
  ) %>%
  arrange(desc(Mean_Index)) # Rank species from highest to lowest index

# Print the resulting ranked table
print(flammability_index_df)