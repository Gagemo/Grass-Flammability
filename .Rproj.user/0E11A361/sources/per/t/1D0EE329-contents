################################################################################
################################################################################
#########################   data - Flammability    #############################
#########################      Pearson Core        #############################
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
                      "ggpubr", "rstatix", "multcompView", "emmeans", "Hmisc")
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
library(Hmisc)

##########################     Read in Data  ###################################

time = read.csv("Data/Flammability Project - Time.csv")
weight = read.csv("Data/Flammability Project - Weight.csv")

data <- cbind(time, weight)
data <- data[ , !duplicated(colnames(data))]
data <- dplyr::select(data, FB, Max_Height, Flame_Total, Smld_Total, Mass_Loss, Mass_Rate)
colnames(data) <- c("FB", "Max Flame Height", "Flame Duration", 
                     "Smoldering Duration", "Mass Loss", "Mass Loss Rate")

core = cor_mat(data)
core_p = cor_pmat(data)

write.csv(core, "Figures/CORE.csv", row.names = FALSE)
write.csv(core_p, "Figures/CORE_P.csv", row.names = FALSE)

res<-rcorr(as.matrix(data))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
core_flat = flattenCorrMatrix(res$r, res$P)
write.csv(core_flat, "Figures/CORE_FLAT.csv", row.names = FALSE)
