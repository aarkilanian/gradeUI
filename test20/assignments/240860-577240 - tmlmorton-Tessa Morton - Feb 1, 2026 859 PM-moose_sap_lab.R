# Title: My R script for Joint lab
# Author: Tessa Morton
# Date: 26-04-2020

# Load libraries needed ---------------------------
library(dplyr)

# Set working directory ---------------------------
setwd("C:/Users/temor/Desktop/BIOL1002_RAssignment")

#21
moose_2020b <- filter(moose_clean, Year == 2020)
moose2020c <- mutate(moose_2020b, MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose2020c, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(
    mean_browsing = mean(BrowsingScore, na.rm = TRUE),
    mean_MooseDensity = mean(MooseDensity, na.rm = TRUE),
    .groups = "drop")
#23a
# Yes, the data support the hypothesis: at low moose density, browsing is concentrated on a few preferred species, 
# while at higher densities browsing increases across most species, indicating a shift toward more generalist feeding.
#23b
# Moose appear to favour Willow and Alder most strongly, showing consistently high browsing scores, 
# while Black Ash and Black Spruce are browsed the least, especially at lower moose densities.
#23c
# Some sapling species are not shown because they were either absent from the dataset or had insufficient observations 
# to calculate meaningful average browsing values.
#24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#25
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose2020c, moose_coll2, by = "Ecoregion")
#26
plot(
  coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Vehicle Collisions",
  main = "Moose Density vs Vehicle Collisions"
)
#As the moose density increases as does the frequency of Moose vs Vehicle Collisions showing a positive correlation.
# There are two outliers which show a higher frequency of moose vs vehicle collision, possibly due to other factors.

#27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)


#28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population"
)
#29
#Collisions per Capita are higher in low-population regions and lower in high population regions.
#This is because rural areas are more moose and wildlife interections with the roads, due to less frequent traffic causing them to change that habit.