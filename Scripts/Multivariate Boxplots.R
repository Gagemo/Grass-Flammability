################################################################################
################################################################################
######################  R Script for Flammability Plots  #######################
######################       Updated for MANCOVA       #########################
######################     University of Florida      ##########################
######################          Gage LaPierre          #########################
######################             2023               ##########################
################################################################################
################################################################################

######################### Clears Environment & History #########################

rm(list=ls(all=TRUE))
cat("\014")

#########################      Installs Packages      ##########################
# This code checks if the necessary packages are installed. If not, it installs
# them automatically to ensure the script runs smoothly.
# The `janitor` package has been added to handle problematic column names.

list.of.packages <- c("tidyverse", "vegan", "multcomp", "multcompView",
                      "ggpubr", "cowplot", "janitor")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################      Loads Packages       ###########################

library(tidyverse)
library(vegan)
library(multcomp)
library(multcompView)
library(ggpubr)
library(cowplot)
library(janitor)

##########################        Read in Data       ###########################
# Read in the three CSV files from the current directory.
# The previous version had an incorrect file path (e.g., "Data/...")
# We first clean and aggregate the data from each file before merging.

data_time_raw <- read.csv("Data/Flammability Project - Time.csv")
data_weight_raw <- read.csv("Data/Flammability Project - Weight.csv")
data_temp_raw <- read.csv("Data/Flammability Project - Temp.csv")

# Use janitor::clean_names() on each dataframe to standardize column names.
# This is a robust way to handle inconsistent or duplicate column headers.
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

# Aggregate the temperature data by taking the maximum temperature for each ID.
# We first filter out any rows with missing temperature values (NAs) to prevent
# the max() function from returning -Inf and generating warnings.
data_temp_clean <- data_temp_raw %>%
  clean_names() %>%
  filter(!is.na(t1) & !is.na(t2)) %>%
  group_by(id) %>%
  summarise(Temp_Fuel_Bed = max(t1, na.rm = TRUE),
            Temp_10cm_Above = max(t2, na.rm = TRUE),
            .groups = 'drop') %>%
  # The summary() output showed these were characters. Explicitly convert to numeric.
  mutate(Temp_Fuel_Bed = as.numeric(Temp_Fuel_Bed),
         Temp_10cm_Above = as.numeric(Temp_10cm_Above))

# Merge all three cleaned dataframes into a single data frame with one row per ID.
data_final <- data_time_clean %>%
  inner_join(data_weight_clean, by = "id") %>%
  inner_join(data_temp_clean, by = "id")

# Inspect the final merged data frame to ensure it's correct
str(data_final)
summary(data_final)

# Now, we will define the list of metrics we want to plot.
# This allows us to loop through each one to create the individual plots.
metrics_to_plot <- c(
  "Fuel_Bed_Height", "Max_Flame_Height", "Flame_Duration",
  "Smoldering_Duration", "Mass_Loss", "Mass_Loss_Rate",
  "Temp_Fuel_Bed", "Temp_10cm_Above"
)

# Create a list to store all the generated plots
plot_list <- list()

# Define a consistent color palette for all plots
cbbPalette <- c("#BE0032", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

# Loop through each metric to generate a box plot
for (metric_name in metrics_to_plot) {
  # The aov() is a practical way to get the post-hoc tests for individual
  # metrics after a significant MANCOVA.
  # The formula uses the current metric name and controls for Fuel_Bed_Height.
  formula_aov <- as.formula(paste(metric_name, "~ Fuel_Bed_Height + species"))
  anova_model <- aov(formula_aov, data = data_final)
  
  # Perform post-hoc Tukey HSD test on the `species` factor
  tukey_results <- TukeyHSD(anova_model)
  
  # Get the compact letter display (CLD) for the `species` factor
  cld <- multcompLetters(extract_p(tukey_results$species))
  cld_df <- as.data.frame(cld$Letters)
  cld_df$species <- row.names(cld_df)
  colnames(cld_df) <- c("letters", "species")
  
  # Merge the letter data with the original data for plotting
  plot_data <- data_final %>%
    left_join(cld_df, by = "species") %>%
    group_by(species) %>%
    mutate(y_position = max(.data[[metric_name]], na.rm = TRUE) +
             (max(.data[[metric_name]], na.rm = TRUE) * 0.1)) # Adjust y position for labels
  
  # Create a custom title for each plot
  plot_title <- ""
  if (metric_name == "Fuel_Bed_Height") {
    plot_title <- "A) Fuel Bed Height (cm)"
  } else if (metric_name == "Max_Flame_Height") {
    plot_title <- "B) Max Flame Height (cm)"
  } else if (metric_name == "Flame_Duration") {
    plot_title <- "C) Flame Duration (s)"
  } else if (metric_name == "Smoldering_Duration") {
    plot_title <- "D) Smolder Duration (s)"
  } else if (metric_name == "Mass_Loss") {
    plot_title <- "E) Mass Loss (%)"
  } else if (metric_name == "Mass_Loss_Rate") {
    plot_title <- "F) Mass Loss Rate (g/s)"
  } else if (metric_name == "Temp_Fuel_Bed") {
    plot_title <- "G) Temp at Fuel Bed (C)"
  } else if (metric_name == "Temp_10cm_Above") {
    plot_title <- "H) Temp at 10cm Above (C)"
  }
  
  
  # Create the ggplot boxplot
  p <- ggplot(plot_data, aes(x = species, y = .data[[metric_name]], fill = species)) +
    geom_boxplot() +
    # Add the compact letters at the top of each box plot
    geom_text(data = distinct(plot_data, species, .keep_all = TRUE),
              aes(label = letters, y = y_position),
              size = 5, fontface = "bold", color = "black") +
    geom_point(shape = 16, size = 2, show.legend = FALSE) +
    labs(title = plot_title,
         x = "",  # Remove x-axis label for cleaner look
         y = "") + # Remove y-axis label
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      text = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
      axis.text.x = element_text(size = 12, face = "italic", color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12, face = "bold", color = "black"),
      legend.position = "none",
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") # Add margin
    ) +
    scale_fill_manual(values = cbbPalette) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  
  # Add the generated plot to our list
  plot_list[[metric_name]] <- p
}

# Combine all the plots into a single figure using ggarrange.
# The `labels` argument adds the A, B, C, ... labels.
combined_plot <- ggarrange(plotlist = plot_list,
                           ncol = 2, nrow = 3,
                           align = "hv",
                           common.legend = FALSE)


# Save the final combined figure to a file.
ggsave("Figures/Figure3_Combined_Boxplots.png",
       plot = combined_plot,
       width = 12, height = 16, units = "in")

