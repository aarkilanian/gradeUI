# Title: R script BIOL 1002
# Author: Kayla Greening
# Date: 30-01-2026
# Installing and Loading Data
setwd("BIOL1002_Rassignment")
install.packages("dplyr")
# Question 1:
library(dplyr)
# Question 2:
moosedata <- read.csv("MoosePopulation(1)")
# Question 3:
moose_clean <- na.omit(moosedata)
# Question 4:
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# Question 5:
# a. 1904
year_min <- min(moose_sel$Year)
# b. 41250
moose_max <- max(moose_sel$Estimated_Moose_Pop)
# Question 6:
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
# Question 7:
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
# Question 8:
# a.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# b.
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland Western Forest Ecoregion")
# Question 9:
# a.
moose_2020 <- filter(moosedata2, Year == 2020)
# b.
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
# c.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
# Question 10:
moosefinal <- moosedata2 %>% filter(Year == 2020) %>% filter (MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()
# Part 2
# Question 11:
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
# Question 12:
# a.
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarise(mean(BrowsingScore)) %>% print()
# b.
avg_browse_reg <- arrange(sap_reg_browse, desc(`mean(BrowsingScore)`))
# Highest average browsing score: Northern_Peninsula_Forests 
# Lowest average browsing score: StraitOfBelleIsleBarrens
# Question 13:
# a.
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(mean(Height)) %>% print()
# b.
# Northern_Peninsula_Forests and Western_Forests have average heights less than 20cm
sap_reg_height_low <- filter(sap_reg_height, `mean(Height)` < 20)
# Question 14:
# a.
sap_spe_browse <- (sap_clean %>% group_by(Species) %>% summarize(mean(BrowsingScore))) %>% print()
# b.
avg_browse_spe <- arrange(sap_spe_browse, desc(`mean(BrowsingScore)`))
# Highest average browsing score: Black_Ash
# Lowest average browsing score: Black_Spruce
# Question 15:
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore))
# Question 16:
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Variation of Balsam Fir Browsing Intensities by Ecoregion", col = "forestgreen")
# Question 17:
# a.
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore))
# b.
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Variation of Black Spruce Browsing Intensities by Ecoregion", col = "pink")
# c.
# Black Spruce browsing is lower than Balsam Fir browsing in the Avalon Forest, Central Forest, Eastern Hyper Oceanic Barrens, Maritime Barrens, North Shore Forest and Northern Peninsula Forest
# Black Spruce browsing is higher than Balsam Fir browsing in the Long Range Barrens, there is only data for Black Spruce in the Western Forest and none for Balsam Fir in this region
# Question 18:
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()
# Question 19: 
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()
# Question 20:
# a.
# I think the Sapling Study data set is not evenly distributed, the North Shore Forest ecoregion and Balsam Fir species are overrepresented, while the Strait of Belle Isle Barrens ecoregion and Black Ash species is underrepresented.
# b.
# It is important to recognize bias in ecological data sets because it can misrepresent ecological patterns, leading conclusions to be based on inaccurate or misleading data.
# Question 21:
# a.
moose_2020b <- filter(moose_clean, Year == 2020)
moose_2020b <- mutate(moose_2020b, MooseDensity = Estimated_Moose_Pop / Area)
# b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
# Question 22:
sum_spe_browse <- moose_sap %>% group_by(Ecoregion,Species) %>% summarise((mean(BrowsingScore)),(mean(MooseDensity))) %>% print()
# Question 23:
# a.
# Yes the hypothesis is supported, at low density moose are able to pick and choose their favorite trees to eat and tend to eat less of others, this is observed by various average browsing densities for varying sapling species. At high densitys food becomes more scarce and therefore they have less option and eat as much as they can of whatever they can find, which is seen by similar and concentrated data points at high browsing intensitys for each species.
# b. 
# Willow and Alder saplings have higher average browsing scores and therefore are favored the most. Black spruce have the lowest average browsing scores and are favored the least.
# c.
# The sapling species not shown on the figure is the black ash. This is because it only occurs in the Western Forests, meaning there isn't enough data to observe a trend.
# Question 24:
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# Question 25:
# a.
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
# *I recognize in the instructions it says to use "rename_with" however that returned an error for me when using the provided template, and so I had to use "rename" which produced the wanted result.
# b.
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
# Question 26:
# a.
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Number of Moose-Vehicle Collisions (2020)", main = "Moose Density vs. Moose-Vehicle Collisions")
# b.
# The trend is that as the moose density increases so does the number of moose-vehicle collisions. There are two outliers, both at a moose density = 1.0, where the number of moose-vehicle collisions are 40 and 110. This is probably because the population size for both of those regions are higher than others.
# Question 27:
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
# Question 28:
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop, xlab = "Collisions per capita", ylab = "Human Population", main = "Collisions per Capita vs. Human Populations")
# Question 29:
# The trend is that as human populations decrease, the collisions per capita increase. This makes sense based on human and moose populations in Newfoundland because areas with smaller human populations would have higher collisions per person because each person is more likely to encounter a moose on the road, even if the total number of collisions is smaller.
