install.packages("tidyverse")
Moosedata <- read.csv("MoosePopulation.csv")
library(dplyr)
Moosedata <- na.omit(Moosedata)
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(Moosedata, Year)
#1904
max(Moosedata, Estimated_Moose_Pop)
#41250
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)
plot(Moosedata$Year, Moosedata$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland Western Forests over time",
     type = "l")
MooseData_2020 <- filter(Moosedata, Year == "2020")
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > "2.0")
arrange(MooseData_2020_b, desc(MooseDensity))
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Part 2
Saplings <- na.omit(Saplings)
SaplingsBrowing <- Saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean = mean(BrowsingScore)) %>%
  print()
#highest: Northern_Peninsula_Forests, lowest: StraitOfBelleIsleBarrens
SaplingsHeight <- Saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean = mean(Height)) %>%
  print()
#western and northern peninsula have heights less than 20cm
SaplingsScore <- Saplings %>%
  group_by(Species) %>%
  summarize(mean = mean(BrowsingScore)) %>%
  print()
#highest: Black Ash, lowest: Black Spruce
fir_reg_browse <- Saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Avg. Browsing Intensity", main = "Balsam Fir Browsing Intensity by Ecoregion", col = "forestgreen", cex.names = 0.6) 
spruce_reg_browse <- Saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Avg. Browsing Intensity", main = "Black Spruce Browsing Intensity by Ecoregion", col = "forestgreen", cex.names = 0.6) 
#Black Spruce has a browsing intensity of 0 in the EasternHyperOceanicBarrens and Maritime_Barrens while the fir browsing intesity is higher.
sap_reg_tally<- Saplings %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- Saplings %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#I think that the sapling study is pretty good with even distribution with a few exceptions. For example, the StraitOfBelleIsleBarrens is very under studied while the North_Shore_Forests are deeply studied.
#Recognizing bias in datasets is very important as bias can cause fluctuation in results and inaccuracies as some data may be over represented while some are under represented.
Moosedatafiltered <- filter(Moosedata, Year == 2020)
moose_2020b <- mutate(Moosedatafiltered, MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, Saplings, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean(BrowsingScore), mean(MooseDensity)) %>%
  print()
#Yes, there are no dots past 1 average moose density that are on the lower half of the graph, proving the hypothesis
#The moose favor the Willows the most and the Black Spruce the least.
#Black Ash is not shown on the graph as there was no browsing intensity on this specific species for moose.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>% 
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, MooseData_2020, by = 'Ecoregion', relationship = "many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Collisions in 2020", main = "Collisions in 2020 vs Moose Density")
#The general trend is as density increases, so does amount of collisions. There is an outlier with over 100 collisions with 1.0 density.
coll_merge_per_capita <- moose_coll2 %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$coll_per_capita, coll_merge$human_pop, xlab = "Collisions Per Capita", ylab = "Human Population", main = "Collisions Per Capita vs Human Population")
#There are less collisions in more populated areas compared to more rural places. This makes sense as rural areas of NL tend to be where the Moose population resides.