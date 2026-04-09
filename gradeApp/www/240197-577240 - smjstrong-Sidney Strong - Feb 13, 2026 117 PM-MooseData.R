# Title: My R Script 
# Author: Sidney Strong
# Date: 07-02-2026
# Set working directory
setwd("~/Bio1002_RAssignment")
# Install package
install.packages("dplyr")
# 1: Load library
library(dplyr)
# 2: Load Dataset
moosedata <- read.csv("MoosePopulation.csv")
# 3: Clean moose dataset
moose_clean <- na.omit(moosedata)
# 4: Simplified moose dataset
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# 5a: Oldest observation in data set for year
year_min <- min(moose_sel$Year, na.rm = TRUE)
# 5b: Highest estimated moose population
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
# 6: Moose density for each ecoregion
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
# 7: Plot data to show the changes in moose density over a year
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
# 8a: Observations from the western forests ecoregion
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# 8b: Line graph of moose density in western forest egoregion over a year
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose Density Over Time in Western Forests")
# 9a: Data set for moose from 2020
moose_2020 <- filter(moosedata2, Year == 2020 )
# 9b: Moose densities about 2.0 moose/km^2
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0 )
# 9c: Arrange by descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
# 10: Combining code with pipes
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
# 11a: Loading sapling data file
saplings <- read.csv("SaplingStudy.csv")
# 11b: Clean sapling set
sap_clean <- na.omit(saplings)
# 12a: Mean browsing score across Ecoregions
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# 12b: Highest and lowest moose browsing
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing)) %>%
  print()
# highest Average Browsing Score: Northern Peninsula Forests (4.57), Lowest Average Browsing Score: Strait Of Belle Isle Barrens(1)
# 13a: Average tree height
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
# 13b: Saplings less than 20cm
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions with severely browsed trees (AverageHeight < 20cm): Northern Penisula Forests (19.9), Western Forests (18.9)
# 14a: Average browsing of sapling species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# 14b: Highest and lowest species browsing 
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing)) %>%
  print()
# Highest average species browsing score: Black Ash (5), Lowest average species browsing score: Black Spruce (2.33)
# 15: Balsam fir browsing intensity by ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# 16: Barplot for balsam fir and ecoregions
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", ylab = "Average Browsing Intensity",
        main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        col = "forestgreen", cex.names = 0.6)
# 17a: Black spruce browsing intensity by ecoregions        
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# 17b: Barplot for Black Spruce and Egoregions
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", ylab = "Average Browsing Intensity",
        main = "Average Black Spruce Browsing Intensity by Ecoregion",
        col = "blue", cex.names = 0.6)
# 17c: Black Spruce Dataset shows lower browsing intensity compared to Balsam fir with ranges from (0.0-4.0) compared to (1.0-4.5)
# 17c: This indicates that moose prefer browsing the Balsam Fir species across most ecoregions 
# 18: Number of tree saplings counted in each ecoregion
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
# 19: Number of saplings counted for each species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# 20a: The Sapling study data set is not evenly distriubuted. Some ecoregions and species are over represented, while others have fewer saplings. 
# 20a: For sample, 5 saplings were observed in the Avalon and Central Forest Ecoregions compared to 1 in the Strait of Belle Isle Barrens. 
# 20a: Another example shows 11 Balsam Fir saplings observed, compared to 1 Black Ash. 
# 20b: It is important to recognize bias in ecological datasets to accurately represent true ecological patterns. 
# 20b: Uneven sampling can misrepresent the data, leading to incorrect assumptions about species and organism interactions such as browsing intensities shown here. 
# 21a: Creating moose data set
moose_2020b <- filter(moose_clean, Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
# 21b: Joining moose data with sapling data
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
# 22: Average browsing score and average moose density for species in ecoregion
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore),
            MeanDensity = mean(MooseDensity)) %>%
  print()
# 23: graphing the data
library(ggplot2)
ggplot(sum_spe_browse, aes(x = MeanDensity, y = MeanBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# 23a: The figure shows evidence supporting the researchers' hypothesis.
# 23a: Moose appear to prefer certain species at low density, and at higher density they browse a wide range of species, indicating that pressure and competition force moose to shift towards generalist browsing. 
# 23b: Moose appear to favor willow the most with consistently high browsing scores, and browse black spruce the least with the lowest browsing scores.
# 23c: Black Ash species is not shown on the figure because there were too few observations recorded to calculate a meaningful average compared to the other species.  
# 24: Running given data, creating moose collision dataset
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# 25a: Renaming column in dataset
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
# 25b: Joining the datasets
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
# 26a: Creating scatterplot of moose density by collisions in 2020
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (per km^2)", ylab = "Number of Moose Vehicle Collisions (2020)",
     main = "Moose Density vs Moose Vehicle Collisions",
     pch = 19, col = "purple")
# 26b: There is a general trend of higher moose density tends to be associated with higher numbers of moose vehicle collisions. 
# 26b: There is an outlying site around the moose density of 1 per km^2, where moose collisions are at an all time high (above 100.)
# 27: Creating data set of ecoregions and number of moose collisions per person
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
# 28: Creating a scatterplot for collisions per capita by human population
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population", ylab = "Moose Vehicle Collisions per Person",
     main = "Collisions per Capita vs Human Population",
     pch = 19, col = "red")
# 29: There is a general trend where regions with smaller human populations have higher collision reports.
# 29: This trend does make sense, it suggests that collisions are more frequent in less populated areas, corresponding with our knowledge of rural areas having higher a moose population and density. 
