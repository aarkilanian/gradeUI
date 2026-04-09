# Title: My R script for sapling lab
# Author: Tessa Morton
# Date: 26-04-2020

# Load libraries needed ---------------------------
library(dplyr)

# Set working directory ---------------------------
setwd("C:/Users/temor/Desktop/BIOL1002_RAssignment")

#11
# Load data ---------------------------
saplings <- read.csv(file = "SaplingStudy.csv")
View(saplings)
na.omit(45)
sap_clean <- na.omit(saplings)

#12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(mean_browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
#12b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(mean_browsing))
print(avg_browse_reg)
#The ecoregion with the highest average browsing is Northern_Peninsula_Forests,
#and the ecoregion with the lowest average browsing is StraitOfBelleIsleBarrens.

#13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(mean_height = mean(Height, na.rm = TRUE))
print(sap_reg_height)
#13b
sap_reg_height_low <- sap_clean %>%
  filter(Height < 20) %>%
  group_by(Ecoregion) %>%
  summarise(mean_height = mean(Height, na.rm = TRUE))
print(sap_reg_height_low)
#The ecoregions with saplings less than 20 cm are Central Forests, North Shore Forests, Northern Peninsula Forests and Western Forests.
#14a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
#14b
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(mean_browsing)) %>%
  print()
#The species with the highest browsing score is Black Ash
#and the species with the lowest is Black Spruce

#15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))

#16
barplot(
  fir_reg_browse$mean_BrowsingScore,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Mean Browsing Score",
  main = "Browsing Pressure on Balsam Fir by Ecoregion",
  col = "forestgreen",
  cex.names = 0.6
  )
#17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))
barplot(
  spruce_reg_browse$mean_BrowsingScore,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Mean Browsing Score",
  main = "Browsing Pressure on Black Spruce by Ecoregion",
  col = "darkgreen",
  cex.names = 0.6)
  # Black spruce generally experiences similar or slightly higher browsing pressure than balsam fir across most ecoregions, 
  # with especially high browsing in the Northern Peninsula Forests, while balsam fir shows more moderate variation overall.
#18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#no, only 5 ecoregions had the same number of tree saplings counted, those being Western Forests, Long Range Barrens, Eastern HyperOceanic Barrens, Central Forests and Avalon Forests. All of which having 5 tree saplings.
#19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#no, the same number of tree saplings were not counted for each species, only Willow amd Alder were the same at 8. 
#20a
# The SaplingStudy dataset does not appear to be evenly distributed, as some ecoregions and tree species 
# (such as balsam fir and northern forest regions) are more heavily represented than others, while some 
# ecoregions show little or no sampling.
#20b
# Recognizing sampling bias is important because uneven coverage can skew interpretations of herbivory/browsing pressure, 
# potentially misrepresenting true browsing intensity across the landscape.

