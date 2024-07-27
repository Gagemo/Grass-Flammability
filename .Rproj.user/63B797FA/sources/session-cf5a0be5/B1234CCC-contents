################################################################################
################################################################################
#########################      GRIN - Fire        ##############################
#########################    Change in Cover      ##############################
#########################  University of Florida  ##############################
#########################     Gage LaPierre       ##############################
#########################      2021 - 2023        ##############################
################################################################################
################################################################################

######################### Clears Environment & History  ########################
rm(list=ls(all=TRUE))
cat("\014") 

#########################     Installs Packages   ##############################
list.of.packages <- c("tidyverse", "vegan", "agricolae", "extrafont", "plotrix", 
                      "ggsignif", "multcompView", "ggpubr", "rstatix", "labdsv",
                      "tables")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################     Loads Packages     ##############################
library(tidyverse)
library(vegan)
library(labdsv)
library(agricolae)
library(extrafont)
library(ggsignif)
library(multcompView)
library(ggpubr)
library(plotrix)
library(rstatix)
library(tables)

##########################     Read in  Data       #############################
GRIN = read.csv("Data/GRIN - 2020-2023.csv")

GRIN$Coverage = as.numeric(GRIN$Coverage)

# Reclasifys coverage data (CV) from 1-10 scale to percent scale #
GRIN <- mutate(GRIN, Coverage = case_when(
  grepl(10, Coverage) ~ 97.5,
  grepl(0, Coverage) ~ 0,
  grepl(1, Coverage) ~ 0.1,
  grepl(2, Coverage) ~ 0.5,
  grepl(3, Coverage) ~ 1.5,
  grepl(4, Coverage) ~ 3.5,
  grepl(5, Coverage) ~ 7.5,
  grepl(6, Coverage) ~ 17.5,
  grepl(7, Coverage) ~ 37.5,
  grepl(8, Coverage) ~ 62.5,
  grepl(9, Coverage) ~ 85
))

str(GRIN)
summary(GRIN)

# Selects just Seeding Treatment # 
GRIN = filter(GRIN, Treatment == 'S')

GRIN$YID <- paste(GRIN$Year,GRIN$ID)
GRIN$ID_ <- paste(GRIN$Fire, GRIN$ID)

# Orders years and treatments so that they display in same sequence in graphs #
GRIN$Year = factor(GRIN$Year, levels=c('2','3'))
GRIN$Fire = factor(GRIN$Fire, levels=c('C','Sp','W'))

#Renames values in fire treatments for heat map later #
GRIN$Fire <- recode(GRIN$Fire, 
                    C ="No Burn", W = "Winter", Sp = "Late-Spring")

#################### Species abundances ########################################
# Creates and joins  data year 22 & 23 to make long data format #
Two_Abundance <- GRIN[which(GRIN$Year == "2"),]
Three_Abundance <- GRIN[which(GRIN$Year == "3"),]

Abundance_w <- full_join(Two_Abundance, Three_Abundance, 
                         by = c('ID_', "Fire", 'Species'))
Abundance_w = arrange(Abundance_w, Fire)

# Turns NA values into zeros #
Abundance_w$Coverage.x <- ifelse(is.na(Abundance_w$Coverage.x), 0, 
                                 Abundance_w$Coverage.x)
Abundance_w$Coverage.y <- ifelse(is.na(Abundance_w$Coverage.y), 0, 
                                 Abundance_w$Coverage.y)

# Change abundance to reflect percentage change from (Year 1) to (Year 2)  #
Change_Abundance <- Abundance_w %>% 
  dplyr::select(ID_, Fire, Species, 
                Coverage.x, Coverage.y) %>%
  group_by(ID_, Fire, Species) %>% 
  mutate(Change_abundance = Coverage.y - Coverage.x)

##################################  COVER CAHNGES ##############################
ES = 
  Change_Abundance[which(Change_Abundance$Species == "Eragrostis spectabilis"),]
ES<-as.data.frame(ES)
ES$Fire<-factor(ES$Fire)

# Check Assumptions #
model  <- lm(Change_abundance ~ Fire, data = ES)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Test for Significance #
anova_ES = ES %>% anova_test(Change_abundance ~ Fire) %>% 
  add_significance()
anova_ES

lm(formula = Change_abundance ~ Fire, ES)
tukey_ES <- ES %>% 
  tukey_hsd(Change_abundance ~ Fire) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_ES

tmp <- tabular(Fire ~ Change_abundance* (mean+sd+std.error), data=ES)
tmp

write.csv.tabular(tmp, "Figures/Chapter 2 - Fire/ES_Change.csv")

love_change_Box = 
  ggplot(ES, aes(x = Fire, y = Change_abundance), colour = Fire) +
  geom_boxplot(aes(fill=Fire), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Fire), size = 3, 
             position = position_jitterdodge(), alpha = 0.7) +
  stat_pvalue_manual(tukey_ES,size = 8, bracket.size = 1, hide.ns = T)+
  labs(subtitle = get_test_label(anova_ES, detailed = TRUE),
       caption = get_pwc_label(tukey_ES)) +
  ylim(-70,50) +
  scale_fill_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                    values=c("#333333", "#FF9900", "#3366FF")) +
  scale_color_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                     values=c("#333333", "#FF9900", "#3366FF")) +
  scale_x_discrete(labels=c('No Burn', 'Late-Spring', 'Winter')) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=16),
        axis.title.x = element_text(size=15, face="bold", colour = "black"),    
        axis.title.y = element_text(size=15, face="bold", colour = "black"),   
        axis.text.x=element_text(size=15, face = "bold", color = "black"),
        axis.text.y=element_text(size=15, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "Change in Coverage", title = "Eragrostis spectabilis")
love_change_Box
ggsave("Figures/Chapter 2 - Fire/change_love.png", 
       width = 10, height = 7)

################################################################################
########################### Indiangrass ########################################
################################################################################
SS = 
  Change_Abundance[which(Change_Abundance$Species == "Sorghastrum secundum"),]
SS<-as.data.frame(SS)
SS$Fire<-factor(SS$Fire)

# Check Assumptions #
model  <- lm(Change_abundance ~ Fire, data = SS)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Test for Significance #
anova_SS = SS %>% anova_test(Change_abundance ~ Fire) %>% 
  add_significance()
summary(anova_SS)

tukey_SS <- SS %>% 
  tukey_hsd(Change_abundance ~ Fire) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_SS

tmp <- tabular(Fire ~ Change_abundance* (mean+sd+std.error), data=SS )
tmp

write.csv.tabular(tmp, "Figures/Chapter 2 - Fire/SS_Change.csv")

SS_change_Box = 
  ggplot(SS, aes(x = Fire, y = Change_abundance), colour = Fire) +
  geom_boxplot(aes(fill=Fire), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Fire), size = 3, 
             position = position_jitterdodge(), alpha = 0.7) +
  stat_pvalue_manual(tukey_SS,size = 8, bracket.size = 1, hide.ns = T)+
  labs(subtitle = get_test_label(anova_SS, detailed = TRUE),
       caption = get_pwc_label(tukey_SS)) +
  scale_fill_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                    values=c("#333333", "#FF9900", "#3366FF")) +
  scale_color_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                     values=c("#333333", "#FF9900", "#3366FF")) +
  scale_x_discrete(labels=c('No Burn', 'Late-Spring', 'Winter')) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=16),
        axis.title.x = element_text(size=15, face="bold", colour = "black"),    
        axis.title.y = element_text(size=15, face="bold", colour = "black"),   
        axis.text.x=element_text(size=15, face = "bold", color = "black"),
        axis.text.y=element_text(size=15, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "Change in Coverage", title = "Sorghastrum secundum")
SS_change_Box
ggsave("Figures/Chapter 2 - Fire/change_indian.png", 
       width = 10, height = 7)

################################################################################
########################### Pityopsis ##########################################
################################################################################
Pt = 
  Change_Abundance[which(Change_Abundance$Species == "Pityopsis trayci"),]
Pt<-as.data.frame(Pt)
Pt$Fire<-factor(Pt$Fire)

# Check Assumptions #
model  <- lm(Change_abundance ~ Fire, data = Pt)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Test for Significance #
anova_Pt = Pt %>% anova_test(Change_abundance ~ Fire) %>% 
  add_significance()
summary(anova_Pt)

tukey_Pt <- Pt %>% 
  tukey_hsd(Change_abundance ~ Fire) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_Pt

tmp <- tabular(Fire ~ Change_abundance* (mean+sd+std.error), data=Pt)
tmp

write.csv.tabular(tmp, "Figures/Chapter 2 - Fire/Pt_Change.csv")

Pt_change_Box = 
  ggplot(Pt, aes(x = Fire, y = Change_abundance), colour = Fire) +
  geom_boxplot(aes(fill=Fire), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Fire), size = 3, 
             position = position_jitterdodge(), alpha = 0.7) +
  stat_pvalue_manual(tukey_Pt,size = 8, bracket.size = 1, hide.ns = T)+
  labs(subtitle = get_test_label(anova_Pt, detailed = TRUE),
       caption = get_pwc_label(tukey_Pt)) +
  scale_fill_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                    values=c("#333333", "#FF9900", "#3366FF")) +
  scale_color_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                     values=c("#333333", "#FF9900", "#3366FF")) +
  scale_x_discrete(labels=c('No Burn', 'Late-Spring', 'Winter')) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=16),
        axis.title.x = element_text(size=15, face="bold", colour = "black"),    
        axis.title.y = element_text(size=15, face="bold", colour = "black"),   
        axis.text.x=element_text(size=15, face = "bold", color = "black"),
        axis.text.y=element_text(size=15, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "Change in Coverage", title = "Pityopsis trayci")
Pt_change_Box
ggsave("Figures/Chapter 2 - Fire/change_Pityopsis.png", 
       width = 10, height = 7)

################################################################################
############################### Rubus ##########################################
################################################################################
Rubus = 
  Change_Abundance[which(Change_Abundance$Species == "Rubus spp."),]
Rubus<-as.data.frame(Rubus)
Rubus$Fire<-factor(Rubus$Fire)

# Check Assumptions #
model  <- lm(Change_abundance ~ Fire, data = Rubus)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Test for Significance #
anova_Rubus = Rubus %>% kruskal_test(Change_abundance ~ Fire) %>% 
  add_significance()
summary(anova_Rubus)

tukey_Rubus <- Rubus %>% 
  dunn_test(Change_abundance ~ Fire) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_Rubus

tmp <- tabular(Fire ~ Change_abundance* (mean+sd+std.error), data=Rubus)
tmp

write.csv.tabular(tmp, "Figures/Chapter 2 - Fire/Rubus_Change.csv")

Rubus_change_Box = 
  ggplot(Rubus, aes(x = Fire, y = Change_abundance), colour = Fire) +
  geom_boxplot(aes(fill=Fire), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Fire), size = 3, 
             position = position_jitterdodge(), alpha = 0.7) +
  stat_pvalue_manual(tukey_Rubus,size = 8, bracket.size = 1, hide.ns = T)+
  labs(subtitle = get_test_label(anova_Rubus, detailed = TRUE),
       caption = get_pwc_label(tukey_Rubus)) +
  scale_fill_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                    values=c("#333333", "#FF9900", "#3366FF")) +
  scale_color_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                     values=c("#333333", "#FF9900", "#3366FF")) +
  scale_x_discrete(labels=c('No Burn', 'Late-Spring', 'Winter')) +
  ylim(0, 90) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=16),
        axis.title.x = element_text(size=15, face="bold", colour = "black"),    
        axis.title.y = element_text(size=15, face="bold", colour = "black"),   
        axis.text.x=element_text(size=15, face = "bold", color = "black"),
        axis.text.y=element_text(size=15, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "Change in Coverage", title = "Rubus spp.")
Rubus_change_Box
ggsave("Figures/Chapter 2 - Fire/change_Rubus.png", 
       width = 10, height = 7)

################################################################################
############################### Liatris ########################################
################################################################################
Liatris = 
  Change_Abundance[which(Change_Abundance$Species == "Liatris gracilis"),]
Liatris<-as.data.frame(Liatris)
Liatris$Fire<-factor(Liatris$Fire)

# Check Assumptions #
model  <- lm(Change_abundance ~ Fire, data = Liatris)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Test for Significance #
anova_Liatris = Liatris %>% kruskal_test(Change_abundance ~ Fire) %>% 
  add_significance()
summary(anova_Liatris)

tukey_Liatris <- Liatris %>% 
  dunn_test(Change_abundance ~ Fire) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_Liatris
summary(tukey_Liatris)

tmp <- tabular(Fire ~ Change_abundance* (mean+sd+std.error), data=Liatris)
tmp

write.csv.tabular(tmp, "Figures/Chapter 2 - Fire/LG_Change.csv")

Liatris_change_Box = 
  ggplot(Liatris, aes(x = Fire, y = Change_abundance), colour = Fire) +
  geom_boxplot(aes(fill=Fire), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Fire), size = 3, 
             position = position_jitterdodge(), alpha = 0.7) +
  stat_pvalue_manual(tukey_Liatris,size = 8, bracket.size = 1, hide.ns = T)+
  labs(subtitle = get_test_label(anova_Liatris, detailed = TRUE),
       caption = get_pwc_label(tukey_Liatris)) +
  ylim(0, 50) +
  scale_fill_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                    values=c("#333333", "#FF9900", "#3366FF")) +
  scale_color_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                     values=c("#333333", "#FF9900", "#3366FF")) +
  scale_x_discrete(labels=c('No Burn', 'Late-Spring', 'Winter')) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=16),
        axis.title.x = element_text(size=15, face="bold", colour = "black"),    
        axis.title.y = element_text(size=15, face="bold", colour = "black"),   
        axis.text.x=element_text(size=15, face = "bold", color = "black"),
        axis.text.y=element_text(size=15, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "Change in Coverage", title = "Liatris gracilis")
Liatris_change_Box
ggsave("Figures/Chapter 2 - Fire/change_Liatris.png", 
       width = 10, height = 7)

################################################################################
############################### Bahia   ########################################
################################################################################
PN = 
  Change_Abundance[which(Change_Abundance$Species == "Paspalum notatum"),]
PN<-as.data.frame(PN)
PN$Fire<-factor(PN$Fire)

# Check Assumptions #
model  <- lm(Change_abundance ~ Fire, data = PN)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
plot(model, 1)

# Test for Significance #
anova_PN = PN %>% kruskal_test(Change_abundance ~ Fire) %>% 
  add_significance()
summary(anova_PN)

tukey_PN <- PN %>% 
  dunn_test(Change_abundance ~ Fire) %>% 
  add_significance() %>% 
  add_xy_position()
tukey_PN
summary(tukey_PN)

tmp <- tabular(Fire ~ Change_abundance* (mean+sd+std.error), data=PN)
tmp

write.csv.tabular(tmp, "Figures/Chapter 2 - Fire/PN_Change.csv")

PN_change_Box = 
  ggplot(PN, aes(x = Fire, y = Change_abundance), colour = Fire) +
  geom_boxplot(aes(fill=Fire), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(fill=Fire), size = 3, 
             position = position_jitterdodge(), alpha = 0.7) +
  stat_pvalue_manual(tukey_PN,size = 8, bracket.size = 1, hide.ns = T)+
  labs(subtitle = get_test_label(anova_PN, detailed = TRUE),
       caption = get_pwc_label(tukey_PN)) +
  ylim(0, 50) +
  scale_fill_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                    values=c("#333333", "#FF9900", "#3366FF")) +
  scale_color_manual(labels=c('No Burn', 'Late-Spring', 'Winter'),
                     values=c("#333333", "#FF9900", "#3366FF")) +
  scale_x_discrete(labels=c('No Burn', 'Late-Spring', 'Winter')) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        text=element_text(size=16),
        axis.title.x = element_text(size=15, face="bold", colour = "black"),    
        axis.title.y = element_text(size=15, face="bold", colour = "black"),   
        axis.text.x=element_text(size=15, face = "bold", color = "black"),
        axis.text.y=element_text(size=15, face = "bold", color = "black"),
        strip.text.x = 
          element_text(size = 15, colour = "black", face = "bold"),
        legend.position = "none") +
  guides(fill = guide_legend(label.position = "bottom")) +
  labs(x = "", y = "Change in Coverage", title = "Paspalum notatum")
PN_change_Box
ggsave("Figures/Chapter 2 - Fire/change_PN.png", 
       width = 10, height = 7)

################## Save Figures Above using ggarrange ##########################
Change = 
  ggarrange(love_change_Box, SS_change_Box, 
            Pt_change_Box, Liatris_change_Box, ncol = 2, nrow = 2)
annotate_figure(Change, top = text_grob("", color = "black", 
                                                  face = "bold", size = 25))
ggsave("Figures/Chapter 2 - Fire/Change.png", 
       width = 12, height = 8)

