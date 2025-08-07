################################################################################
################################################################################
######################  R Script for Flammability Plots  #######################
######################        Updated for MANCOVA      #########################
######################     University of Florida      ##########################
######################          Gage LaPierre          #########################
######################              2023               ##########################
################################################################################
################################################################################

######################### Clears Environment & History #########################

rm(list=ls(all=TRUE))
cat("\014")

#########################     Installs Packages      ##########################
# This code checks if the necessary packages are installed. If not, it installs
# them automatically to ensure the script runs smoothly.
# The `janitor` package has been added to handle problematic column names.

list.of.packages <- c("tidyverse", "vegan", "multcomp", "multcompView",
                      "ggpubr", "cowplot", "janitor")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################      Loads Packages        ###########################

library(tidyverse)
library(vegan)
library(multcomp)
library(multcompView)
library(ggpubr)
library(cowplot)
library(janitor)

##########################        Read in Data        ###########################
# Read in the three CSV files from the current directory.
data_time_raw <- read.csv("Data/Flammability Project - Time.csv")
data_weight_raw <- read.csv("Data/Flammability Project - Weight.csv")
data_temp_raw <- read.csv("Data/Flammability Project - Temp.csv")

# Corrected data cleaning and renaming block
data_time_clean <- data_time_raw %>%
  janitor::clean_names() %>%
  # Now, we use rename() after clean_names().
  # The column name 'fb' becomes 'fb' after janitor::clean_names()
  rename(Fuel_Bed_Height = fb,
         Max_Flame_Height = max_height,
         Flame_Duration = flame_total,
         Smoldering_Duration = smld_total) %>%
  # And then we select the columns you need
  dplyr::select(id, species, ruderal, Fuel_Bed_Height, Max_Flame_Height,
                Flame_Duration, Smoldering_Duration)

# Corrected data cleaning and renaming block for weight data
data_weight_clean <- data_weight_raw %>%
  janitor::clean_names() %>%
  rename(Mass_Loss = mass_loss,
         Mass_Loss_Rate = mass_rate) %>%
  # Now select the columns with their new names
  dplyr::select(id, Mass_Loss, Mass_Loss_Rate)

data_temp_clean <- data_temp_raw %>%
  janitor::clean_names() %>%
  filter(!is.na(t1) & !is.na(t2)) %>%
  group_by(id) %>%
  summarise(Temp_Fuel_Bed = max(t1, na.rm = TRUE),
            Temp_10cm_Above = max(t2, na.rm = TRUE),
            .groups = 'drop')

data_final <- data_time_clean %>%
  inner_join(data_weight_clean, by = "id") %>%
  inner_join(data_temp_clean, by = "id")

# Add this line after reading and cleaning the data but before the factor() call
data_final$species <- str_replace_all(data_final$species, "Sprobulus juncus", "Sporobolus junceus")

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

# Updated list of metrics to plot
metrics_to_plot <- c(
  "Fuel_Bed_Height", "Max_Flame_Height", "Flame_Duration",
  "Smoldering_Duration", "Mass_Loss", "Mass_Loss_Rate"
)

# Create a list to store all the generated plots
plot_list <- list()

# Define a consistent color palette for all plots
cbbPalette <- c("#BE0032", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

# Loop through each metric to generate a box plot
for (metric_name in metrics_to_plot) {
  f_value <- NA
  p_value <- NA
  plot_title <- ""
  
  if (metric_name == "Fuel_Bed_Height") {
    anova_model <- aov(Fuel_Bed_Height ~ species, data = data_final)
    anova_summary <- summary(anova_model)
    f_value <- anova_summary[[1]]["species", "F value"]
    p_value <- anova_summary[[1]]["species", "Pr(>F)"]
    plot_title <- "A) Fuel Bed Height (cm)"
    stats_text <- paste0(" (F=", round(f_value, 2), ", p=", format.pval(p_value, digits = 2, eps = 0.001), ")")
    plot_title <- paste0(plot_title, stats_text)
    tukey_results <- TukeyHSD(anova_model, which = "species")
    cld <- multcompLetters(extract_p(tukey_results$species))
    cld_df <- as.data.frame(cld$Letters)
    cld_df$species <- row.names(cld_df)
    colnames(cld_df) <- c("letters", "species")
  } else {
    formula_aov <- as.formula(paste(metric_name, "~ Fuel_Bed_Height + species"))
    anova_model <- aov(formula_aov, data = data_final)
    anova_summary <- summary(anova_model)
    f_value <- anova_summary[[1]]["species", "F value"]
    p_value <- anova_summary[[1]]["species", "Pr(>F)"]
    
    plot_title <- case_when(
      metric_name == "Max_Flame_Height"     ~ "B) Max Flame Height (cm)",
      metric_name == "Flame_Duration"       ~ "C) Flame Duration (s)",
      metric_name == "Smoldering_Duration"  ~ "D) Smolder Duration (s)",
      metric_name == "Mass_Loss"            ~ "E) Mass Loss (%)",
      metric_name == "Mass_Loss_Rate"       ~ "F) Mass Loss Rate (g/s)"
    )
    
    stats_text <- paste0(" (F=", round(f_value, 2), ", p=", format.pval(p_value, digits = 2, eps = 0.001), ")")
    plot_title <- paste0(plot_title, stats_text)
    
    tukey_results <- TukeyHSD(anova_model, which = "species")
    if (is.null(tukey_results$species)) {
      cld_df <- data.frame(letters = rep("a", nlevels(data_final$species)),
                           species = levels(data_final$species))
    } else {
      cld <- multcompLetters(extract_p(tukey_results$species))
      cld_df <- as.data.frame(cld$Letters)
      cld_df$species <- row.names(cld_df)
      colnames(cld_df) <- c("letters", "species")
    }
  }
  
  # Merge the letter data with the main data and ensure correct factor order again
  plot_data <- data_final %>%
    left_join(cld_df, by = "species") %>%
    mutate(species = factor(species, levels = species_order)) %>%
    group_by(species) %>%
    mutate(y_position = max(.data[[metric_name]], na.rm = TRUE) +
             (max(.data[[metric_name]], na.rm = TRUE) * 0.1))
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = species, y = .data[[metric_name]], fill = species)) +
    geom_boxplot() +
    geom_text(data = distinct(plot_data, species, .keep_all = TRUE),
              aes(label = letters, y = y_position),
              size = 5, fontface = "bold", color = "black") +
    geom_point(shape = 16, size = 2, show.legend = FALSE) +
    labs(title = plot_title, x = "", y = "") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      text = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
      axis.text.x = if (metric_name %in% c("Mass_Loss", "Mass_Loss_Rate")) {
        element_text(size = 12, face = "italic", color = "black", angle = 45, hjust = 1)
      } else {
        element_blank()
      },
      axis.text.y = element_text(size = 12, face = "bold", color = "black"),
      legend.position = "none",
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    ) +
    scale_fill_manual(values = cbbPalette) +
    scale_x_discrete(
      limits = species_order,
      labels = function(x) str_wrap(x, width = 10)
    )
  
  plot_list[[metric_name]] <- p
}


# Combine all the plots into a single figure using ggarrange.
combined_plot <- ggarrange(plotlist = plot_list,
                           ncol = 2, nrow = 3,
                           common.legend = FALSE)

# Save the final combined figure to a file.
ggsave("Figures/Combined_Boxplots.png",
       plot = combined_plot,
       width = 12, height = 16, units = "in")

