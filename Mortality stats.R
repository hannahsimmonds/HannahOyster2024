########Examine mortality between sites in field study###
# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(nlme)
library(lme4)
# Read in and prepare the data --------------------------------------------
#read in the mortality data
mortdat <- read.csv(here("C:/Users/Acidification/OneDrive - UW/Desktop/HannahOyster2024/MAY_2023_JULY_2024_morts.csv"))
bagmortdat <- read.csv(here("C:/Users/Acidification/OneDrive - UW/Desktop/HannahOyster2024/MAY_2023_JULY_2024_MORTSBYBAG.csv"))
#Ensure data structure is correct
mortdat <- mortdat %>% mutate_at(c("Site", "Colour", "Month"), as.factor)
bagmortdat <- bagmortdat %>% mutate_at(c("Site", "Bag_Col", "Month", "Bag_no", "Month_no"), as.factor)

##calculate survival proportion
widebagmort$May_2023_prop <- widebagmort$May_2023_survivors/50
widebagmort$June_2023_prop <- widebagmort$June_2023_survivors/widebagmort$May_2023_survivors
widebagmort$July_2023_prop <- widebagmort$July_2023_survivors/widebagmort$June_2023_survivors
widebagmort$August_2023_prop <- widebagmort$August_2023_survivors/widebagmort$July_2023_survivors
widebagmort$September_2023_prop <- widebagmort$September_2023_survivors/widebagmort$August_2023_survivors
widebagmort$October_2023_prop <- widebagmort$October_2023_survivors/widebagmort$September_2023_survivors
widebagmort$May_2024_prop <- widebagmort$May_2024_survivors/widebagmort$October_2023_survivors
widebagmort$June_2024_prop <- widebagmort$June_2024_survivors/widebagmort$May_2024_survivors
widebagmort$July_2024_prop <- widebagmort$July_2024_survivors/widebagmort$June_2024_survivors


# Statistical analysis ----------------------------------------------------
#Question - are there differences in mortality between months, sites, and ploidy
#We have multiple cages - each cage is a rep
#Inspect the data

hist(mortdat$Morts)
hist(bagmortdat$Mort_count)
##two way ANOVA
mort.aov_Site <- aov(Mort_count~Site, data = bagmortdat)
summary(mort.aov_Site)

mort.aov_Ploidy <- aov(Mort_count~Bag_Col, data = bagmortdat)
summary(mort.aov_Ploidy)
#Basic three way ANOVA
mort.aov1 <- aov(Mort_count~Site*Bag_Col*Month_year, bagmortdat)
summary(mort.aov1)
#No interactions are present so we can drop these 
mort.aov2 <- aov(Mort_count~Site+Bag_Col+Month_year, bagmortdat)
summary(mort.aov2)
TukeyHSD(mort.aov2)
###Linear mixed model
#step one no random factors
fit1 <- gls(Mort_count ~ 1+Site*Bag_Col*Month, method = "REML", data = bagmortdat, )
