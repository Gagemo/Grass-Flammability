################################################################################
################################################################################
#########################   FNPS - Seed Mixture   ##############################
#########################       Obligates         ##############################
#########################  University of Florida  ##############################
#########################     Gage LaPierre       ##############################
#########################      2021 - 2023        ##############################
################################################################################
################################################################################

######################### Clears Environment & History  ########################
rm(list=ls(all=TRUE))
cat("\014") 

#########################     Installs Packages   ##############################
list.of.packages <- c("tidyverse", "vegan", "agricolae", "extrafont", 
                      "ggsignif", "multcompView", "ggpubr", "rstatix",
                      "vegan", "labdsv")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################     Loads Packages     ##############################
library(extrafont)
#font_import()
loadfonts(device = "win")
library(tidyverse)
library(vegan)
library(agricolae)
library(ggsignif)
library(multcompView)
library(ggpubr)
library(rstatix)
library(vegan)
library(labdsv)

####################### Read in 2021 - 2023 Data  ##############################
Data = read.csv("Data/FNPS - Seed Mixture Project - 2021-2023.csv")
Data$Coverage = as.numeric(Data$Coverage)

str(Data)
summary(Data)

# Reclasifys coverage data (CV) from 1-10 scale to percent scale #
Data <- mutate(Data, Coverage = case_when(
  grepl(10, Coverage) ~ 97.5,
  grepl(1, Coverage) ~ 0.1,
  grepl(2, Coverage) ~ 0.5,
  grepl(3, Coverage) ~ 1.5,
  grepl(4, Coverage) ~ 3.5,
  grepl(5, Coverage) ~ 7.5,
  grepl(6, Coverage) ~ 17.5,
  grepl(7, Coverage) ~ 37.5,
  grepl(8, Coverage) ~ 62.5,
  grepl(9, Coverage) ~ 85,
  grepl(0, Coverage) ~ 0,
))

Data$Treatment = factor(Data$Treatment, levels=c('BH', 'BM', 'LH', 'LM', 'W', "C"))

Pt = filter(Data, Species == "Pityopsis trayci")
summary(Pt)

# Creates data sets by year #
Pt_22 = filter(Pt, Year == 1)
Pt_23 = filter(Pt, Year == 2)

################################################################################
################ Test for Significance across years ############################
################################################################################

############################### 2022 Data ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = Pt_22)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
Pt_22$Treatment= as.factor(Pt_22$Treatment)
Pt_22 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_22 = Pt_22 %>% anova_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_22)

tukey_22 <- Pt_22 %>% 
  tukey_hsd(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_22

############################### 2023 Pt ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = Pt_23)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
Pt_23$Treatment= as.factor(Pt_23$Treatment)
Pt_23 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_23 = Pt_23 %>% anova_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_23)

tukey_23 <- Pt_23 %>% 
  tukey_hsd(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_23

################################################################################
################ Create Box Plot for Pityopsis Across Years ####################
################################################################################

## Lovegrass Coverage 2022 Box plot ##
PtBox22 = 
  ggplot(Pt_22, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  stat_pvalue_manual(tukey_22,size = 8, bracket.size = 1, hide.ns = T)+
  labs(subtitle = get_test_label(anova_22,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_22)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_text(size=20, face="italic", colour = "black"),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_text(size=20, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 20, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "P. trayci % Coverage", title = "2022")
PtBox22

ggsave("Figures/Pt_box22.png", 
       width = 12, height = 8)

## Lovegrass Coverage 2023 Boxplot ##
PtBox23 = 
  ggplot(Pt_23, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  stat_pvalue_manual(tukey_23, size = 8, bracket.size = 1, hide.ns = T)+
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_23,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_23)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_blank(),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(), 
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "P. trayci % Coverage", title = "2023")
PtBox23

ggsave("Figures/Ptbox23.png", 
       width = 12, height = 8)

################## Save Figures Above using ggarrange ##########################
ggarrange(PtBox22, PtBox23, ncol = 2, nrow = 1)
ggsave("Figures/22-23_PtBox.png", 
       width = 16, height = 14)

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=Pt_22)
tmp

write.csv.tabular(tmp, "Figures/Pt_22.csv")

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=Pt_23)
tmp

write.csv.tabular(tmp, "Figures/Pt_23.csv")

############################### Liatris ########################################

Lg = filter(Data, Species == "Liatris gracilis")
summary(Lg)

# Creates data sets by year #
Lg_22 = filter(Lg, Year == 1)
Lg_23 = filter(Lg, Year == 2)

################################################################################
################ Test for Significance across years ############################
################################################################################

############################### 2022 Data ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = Lg_22)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
Lg_22$Treatment= as.factor(Lg_22$Treatment)
Lg_22 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_22 = Lg_22 %>% anova_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_22)

tukey_22 <- Lg_22 %>% 
  tukey_hsd(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_22

############################### 2023 Lg ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = Lg_23)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
Lg_23$Treatment= as.factor(Lg_23$Treatment)
Lg_23 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_23 = Lg_23 %>% anova_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_23)

tukey_23 <- Lg_23 %>% 
  tukey_hsd(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_23

################################################################################
################ Create Box Plot for Pityopsis Across Years ####################
################################################################################

## Lovegrass Coverage 2022 Box plot ##
LgBox22 = 
  ggplot(Lg_22, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  stat_pvalue_manual(tukey_22,size = 8, bracket.size = 1, hide.ns = T)+
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_22,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_22)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_text(size=20, face="italic", colour = "black"),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_text(size=20, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 20, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "L. gracilis % Coverage", title = "2022")
LgBox22

ggsave("Figures/LG_box22.png", 
       width = 12, height = 8)

## Lovegrass Coverage 2023 Boxplot ##
LgBox23 = 
  ggplot(Lg_23, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  stat_pvalue_manual(tukey_23, size = 8, bracket.size = 1, hide.ns = T)+
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_23,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_23)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_blank(),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(), 
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "L. gracilis % Coverage", title = "2023")
LgBox23

ggsave("Figures/LG_box23.png", 
       width = 12, height = 8)

################## Save Figures Above using ggarrange ##########################
ggarrange(LgBox22, LgBox23, ncol = 2, nrow = 1)
ggsave("Figures/22-23_LGBox.png", 
       width = 12, height = 10)

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=Lg_22)
tmp

write.csv.tabular(tmp, "Figures/Lg_22.csv")

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=Lg_23)
tmp

write.csv.tabular(tmp, "Figures/Lg_23.csv")

######################## Dalea pinnata #########################################

Dp = filter(Data, Species == "Dalea pinnata")
summary(Dp)

# Creates data sets by year #
Dp_22 = filter(Dp, Year == 1)
Dp_23 = filter(Dp, Year == 2)

################################################################################
################ Test for Significance across years ############################
################################################################################

############################### 2022 Dp ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = Dp_22)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
Dp_22$Treatment= as.factor(Dp_22$Treatment)
Dp_22 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_22 = Dp_22 %>% kruskal_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_22)

tukey_22 <- Dp_22 %>% 
  dunn_test(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_22

############################### 2023 Dp ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = Dp_23)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
Dp_23$Treatment= as.factor(Dp_23$Treatment)
Dp_23 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_23 = Dp_23 %>% kruskal_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_23)

tukey_23 <- Dp_23 %>% 
  dunn_test(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_23

################################################################################
################ Create Box Plot for Pityopsis Across Years ####################
################################################################################

## Lovegrass Coverage 2022 Box plot ##
DpBox22 = 
  ggplot(Dp_22, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  stat_pvalue_manual(tukey_22,size = 8, bracket.size = 1, hide.ns = T)+
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_22,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_22)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_text(size=20, face="italic", colour = "black"),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_text(size=20, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 20, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "D. pinnata % Coverage", title = "2022")
DpBox22

ggsave("Figures/DP_box22.png", 
       width = 12, height = 8)

## DP Coverage 2023 Boxplot ##
DpBox23 = 
  ggplot(Dp_23, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  stat_pvalue_manual(tukey_23, size = 8, bracket.size = 1, hide.ns = T)+
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_23,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_23)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_blank(),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(), 
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "D. pinnata % Coverage", title = "2023")
DpBox23

ggsave("Figures/DPbox23.png", 
       width = 12, height = 8)

################## Save Figures Above using ggarrange ##########################
ggarrange(DpBox22, DpBox23, ncol = 2, nrow = 1)
ggsave("Figures/22-23_DPBox.png", 
       width = 12, height = 10)

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=Dp_22)
tmp

write.csv.tabular(tmp, "Figures/Dp_22.csv")

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=Dp_23)
tmp

write.csv.tabular(tmp, "Figures/Dp_23.csv")

###################### Sorghastrum #############################################

Ss = filter(Data, Species == "Sorghastrum secundum")
summary(Ss)

# Creates data sets by year #
Ss_22 = filter(Ss, Year == 1)
Ss_23 = filter(Ss, Year == 2)

################################################################################
################ Test for Significance across years ############################
################################################################################

############################### 2022 Ss ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = Ss_22)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
Ss_22$Treatment= as.factor(Ss_22$Treatment)
Ss_22 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_22 = Ss_22 %>% anova_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_22)

tukey_22 <- Ss_22 %>% 
  tukey_hsd(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_22

############################### 2023 Ss ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = Ss_23)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
Ss_23$Treatment= as.factor(Ss_23$Treatment)
Ss_23 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_23 = Ss_23 %>% anova_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_23)

tukey_23 <- Ss_23 %>% 
  tukey_hsd(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_23

################################################################################
################ Create Box Plot for Pityopsis Across Years ####################
################################################################################

## indiangrass Coverage 2022 Box plot ##
SsBox22 = 
  ggplot(Ss_22, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  stat_pvalue_manual(tukey_22,size = 8, bracket.size = 1, hide.ns = T)+
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_22,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_22)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_text(size=20, face="italic", colour = "black"),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_text(size=20, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 20, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "S. secundum % Coverage", title = "2022")
SsBox22

ggsave("Figures/Indiangrass_box22.png", 
       width = 12, height = 8)

## Lovegrass Coverage 2023 Boxplot ##
SsBox23 = 
  ggplot(Ss_23, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  stat_pvalue_manual(tukey_23, size = 8, bracket.size = 1, hide.ns = T)+
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_23,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_23)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_blank(),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(), 
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "S. secundum % Coverage", title = "2023")
SsBox23

ggsave("Figures/Indiangrass_box23.png", 
       width = 12, height = 8)

################## Save Figures Above using ggarrange ##########################
ggarrange(SsBox22, SsBox23, ncol = 2, nrow = 1)
ggsave("Figures/22-23_IndiangrassBox.png", 
       width = 14, height = 10)

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=Ss_22)
tmp

write.csv.tabular(tmp, "Figures/Ss_22.csv")

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=Ss_23)
tmp

write.csv.tabular(tmp, "Figures/Ss_23.csv")

################################ Splitbeard ####################################

At = filter(Data, Species == "Andropogon ternarius")
summary(At)

# Creates data sets by year #
At_22 = filter(At, Year == 1)
At_23 = filter(At, Year == 2)

################################################################################
################ Test for Significance across years ############################
################################################################################

############################### 2022 At ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = At_22)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
At_22$Treatment= as.factor(At_22$Treatment)
At_22 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_22 = At_22 %>% kruskal_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_22)

tukey_22 <- At_22 %>% 
  dunn_test(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_22

############################### 2023 At ######################################
# Check Assumptions #
model  <- lm(Coverage ~ Treatment, data = At_23)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)
# Compute Levene's Test
At_23$Treatment= as.factor(At_23$Treatment)
At_23 %>% levene_test(Coverage ~ Treatment)

# Test for Significance #
anova_23 = At_23 %>% kruskal_test(Coverage ~ Treatment) %>% 
  add_significance()
summary(anova_23)

tukey_23 <- At_23 %>% 
  dunn_test(Coverage ~ Treatment) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_23

################################################################################
################ Create Box Plot for A. ternarius Across Years ####################
################################################################################

## Lovegrass Coverage 2022 Box plot ##
AtBox22 = 
  ggplot(At_22, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  stat_pvalue_manual(tukey_22,size = 8, bracket.size = 1, hide.ns = T)+
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_22,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_22)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_text(size=20, face="italic", colour = "black"),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_text(size=20, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 20, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "A. ternarius % Coverage", title = "2022")
AtBox22

ggsave("Figures/Split_box22.png", 
       width = 12, height = 8)

## Lovegrass Coverage 2023 Boxplot ##
AtBox23 = 
  ggplot(At_23, aes(x = Treatment, y = Coverage), colour = Treatment) +
  geom_boxplot(aes(fill=Treatment), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Treatment), 
             position = position_jitterdodge(), size = 2, alpha = 0.5) +
  stat_pvalue_manual(tukey_23, size = 8, bracket.size = 1, hide.ns = T)+
  scale_color_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                     values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  scale_fill_manual(labels=c('BH', 'BM', 'LH', 'LM', 'W', "C"),
                    values=c("#663333", "#FF9966", "#006600", "#99FF99", "#CC0000", "#330099")) +
  ylim(0, 100) +
  labs(subtitle = get_test_label(anova_23,
                                 detailed = TRUE),
       caption = get_pwc_label(tukey_23)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=20),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_blank(),   
        axis.text.x=element_text(size=20, face = "bold", color = "black"),
        axis.text.y=element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(), 
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "A. ternarius % Coverage", title = "2023")
AtBox23

ggsave("Figures/Split_box23.png", 
       width = 12, height = 8)

################## Save Figures Above using ggarrange ##########################
ggarrange(AtBox22, AtBox23, ncol = 2, nrow = 1)
ggsave("Figures/22-23_SplitBox.png", 
       width = 12, height = 10)

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=At_22)
tmp

write.csv.tabular(tmp, "Figures/At_22.csv")

tmp <- tabular(Treatment ~ Coverage * (mean+sd+std.error), data=At_23)
tmp

write.csv.tabular(tmp, "Figures/At_23.csv")

################## Save All Figures Above using ggarrange ##########################
all = 
  ggarrange(PtBox22, PtBox23, DpBox22, DpBox23, 
            SsBox22, SsBox23, ncol = 2, nrow = 3)
ggsave("Figures/22-23_Obli_Forb_Grass.png", 
       width = 14, height = 18)
