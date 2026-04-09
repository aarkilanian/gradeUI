
# Title: BIOL 1002 R Assignment
# Author: Donovan McKay
# Date: 2-8-2026
# ---------------------------------------------
# Question 1 # Load libraries needed
library(dplyr)
# ---------------------------------------------
# Question 2 # Load data 
moosedata <- read.csv("MoosePopulation.csv")
# ---------------------------------------------
# Question 3 
# a.
na.omit(moosedata)
# b.
moose_clean <- na.omit(moosedata)
# ---------------------------------------------
# Question 4 
# a.
select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# b.
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# ---------------------------------------------
# Question 5
# a.
year_min <- min(moose_sel$Year)
# b.
moose_max <- max(moose_sel$Estimated_Moose_Pop)
# ---------------------------------------------
# Question 6 
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
# ---------------------------------------------
# Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l", xlab = "Year", ylab = "Moose Density in Newfoundland Ecoregions Over Time")
# ---------------------------------------------
# Question 8
# a.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# b.
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moosemper sq km", main = "Moose Density Over Time in Western Forests")
# -------------------------------------------
# Question 9
# a.
moose_2020 <- filter(moosedata2, Year == 2020)
# b.
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
# c.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
# -------------------------------------------
# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
# -------------------------------------------
# Question 11
# a.
saplings <- read.csv("SaplingStudy.csv")
# b.
sap_clean <- na.omit(saplings)
# -------------------------------------------
# Question 12
# a.
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))
print(sap_reg_browse)
# b.
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
print(avg_browse_reg)
# Highest browsing: Northern Peninsula Forests
# Lowest browsing: Strait of Belle Isle Barrens
#-------------------------------------------
# Question 13
# a.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>% 
  summarize(AverageHeight = mean(Height))
print(sap_reg_height)
# b.
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)
print(sap_reg_height_low)
# Ecoregions with average height < 20 cm: Northern Peninsula Forests, Western Forests
# ------------------------------------------
# Question 14
# a.
sap_spe_browse <- sap_clean %>% 
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))
print(sap_spe_browse)
# b.
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
print(avg_browse_spe)
# Highest browsing species: Black Ash
# Lowest browsing species: Black Spruce
# ------------------------------------------
# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))
print(fir_reg_browse)
# ------------------------------------------
# Question 16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Balsam Fir Browsing Score", main = "Avergae Balsam Fir Browsing Intensity by Ecoregion", col = "forestgreen", cex.names = 0.6)
# ------------------------------------------
# Question 17
# a.
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))
print(spruce_reg_browse)
# b. 
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Black Spruce Browsing Score", main = "Average Black Spruce Browsing Intensity by Ecoregion", col = "darkolivegreen3", cex.names = 0.6)
# c. 
# Overall, the Balsam Fir has higher averages across ecoregions. 
# The biggest differences were in EasternHyperOceanicBarrens and Maritime Barrens, each with a score of 0.00 for Black Spruce, and 2.00 for Balsam Fir.
# ------------------------------------------
# Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# The number of tree saplings counted across each ecoregion were not the same, some had as hhigh as 8 and others only 1.
#-------------------------------------------
# Question 19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
# The same number of tree saplings were not counted for each species.
# ------------------------------------------
# Question 20
# a. 
# The North Shore Forests are overrepresented with more appearances in the dataset than other regions, while the Straight of Belle Isle Barrens are underepresented with only 1.
# Black Ash is very underepresented in this study with only one sapling.
# b.
# Recognizing bias in ecological datasets is important because over or underepresented species or regions in a study can lead people to believe browsing patterns are uneven when they may not be.
#-------------------------------------------
# Question 21
# a. 
moose_2020b <- moose_clean %>%
  filter(Year == 2020)%>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
# b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
print(moose_sap)
# ------------------------------------------
# Question 22 
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore), AverageMooseDensity = mean(MooseDensity)) %>%
  print()
# ------------------------------------------
# Question 23 
# a. 
# Moose show stronger preferences at low density and move to more generalist browsing at high density. The figure shows that as moose density increases, so does the browsing intensity. Moose consume more specific saplings when there are less moose around as they can choose what to consume.
# b. 
# Moose favour Balsam Fir the most as it is consistently consumed throughout densities. 
# Black Spruce appears to be the least browsed sapling.
# c.
# Black Ash is not shown on the figure beacuse there was only 1 sample of it in this study.
# ------------------------------------------
# Question 24 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# ------------------------------------------
# Question 25
# a. 
moose_coll2 <- moose_coll %>%
  rename_with(.cols = study_sites, .fn = ~ "Ecoregion")
# b. 
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
# ------------------------------------------
# Question 26
# a.
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Number of Moose-Vehicle Collisions in 2020", main = "Moose Density vs Vehicle Collisions", pch = 19, col = "darkred")
# b.
# Generally speaking, as moose density increases, moose-vehicle collisions increase along with it. Suggesting higher density means higher risk of collisions.
# ------------------------------------------
# Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop) %>% 
  print()
# ------------------------------------------
# Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Moose Collisions per Capita", main = "Collisions per Capita vs Human Population", pch = 19, col = "purple")
# ------------------------------------------
# Question 29
# The collisions per capita are higher in the regions with a lower human population which makes sense as there is a larger moose population in the more rural parts of Newfoundland.

