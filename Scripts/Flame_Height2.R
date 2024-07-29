# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcompView)

# Load your data
GRASS <- read.csv("Data/Flammability Project - Time.csv")

# Convert columns to appropriate types
GRASS$Species <- factor(GRASS$Species)
GRASS$Max_Height <- as.numeric(GRASS$Max_Height)
GRASS$FB <- as.numeric(GRASS$FB)

# Fit the ANOVA model
anova <- aov(Max_Height ~ Species + FB, data = GRASS)

# Perform post-hoc tests on the Species factor
posthoc_results <- emmeans(anova, pairwise ~ Species)

# Generate compact letter displays for significance testing
cld_results <- multcomp::cld(posthoc_results$emmeans, Letters = letters, adjust = "tukey")

# Convert cld_results to a data frame
cld_df <- as.data.frame(cld_results)

# Create a boxplot with significance letters
ggplot(GRASS, aes(x = Species, y = Max_Height, fill = Species)) +
  geom_boxplot() +
  geom_text(data = cld_df, aes(x = Species, y = emmean, label = .group), vjust = -0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
