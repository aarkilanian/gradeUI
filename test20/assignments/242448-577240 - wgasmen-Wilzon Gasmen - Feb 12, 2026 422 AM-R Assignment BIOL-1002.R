# Title: My R script for Managing Biological Data
# Author: Wilzon Gasmen
# Date: 10-02-2026
install.packages("tidyverse")
#Question 1
library(dplyr)
setwd("/cloud/project/Biological Datas")
#Question 2
moosedata <- read.csv("MoosePopulation.csv")
#Question 3
moose_clean <- na.omit(moosedata)
#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# Question 5
#a
Year <- moose_sel$Year
year_min <- min(Year)
#b
Estimated_Moose_Pop<-moose_sel$Estimated_Moose_Pop
moose_max <- max(Estimated_Moose_Pop)
#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7
#a
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Question 8
#a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#b
plot(type = "l", moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in West Forests ecoregions over time")
#Question 9
#a
moose_2020 <- filter(moosedata2, Year == "2020")
#b 
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Question 11
#a
saplings <- read.csv("SaplingStudy.csv")
#b
sap_clean <- na.omit(saplings)
#Question 12
#a
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
#b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(`mean(BrowsingScore)`))
#)The Northern Peninsula had the highest AverageBrowsing score, while the Strait of Belle Island had the lowest AverageBrowsing score
#Question 13
#a
sap_reg_height <- sap_clean %>% 
  group_by(Ecoregion) %>%
  summarize(mean(Height)) %>%
  print()
#b
sap_reg_height_low <- sap_reg_height %>%
  filter(`mean(Height)`< 20) %>%
  print()
#)The Northern Peninsula Forests and Western Forest have avrg. heights less than 20cm.
#Question 14
#a
sap_spe_browse <- sap_clean %>% 
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
#b
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(`mean(BrowsingScore)`)) %>%
  print()
#)Black Ash has the highest AvgBrowsing score, while Blacg Spruce has the lowest Avg.BRpwsing score.
#Question 15
fir_reg_browse <- sap_clean %>% 
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) 
#Question 16
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "mean (Browsing Score)", main = "Balssam Fir's Browsing Intensity in Different Ecoregions", col = "forestgreen", cex.names = 0.65
#Question 17
#a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))
#b
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "mean (Browsing Score)", main = "Black Spruces' Browsing Intensity in Different Ecoregions", col = "pink", cex.names = 0.65)
#c)In regions, such as Maritime Barrens and Eastern Hyper Oceanic Barrens, black spruce aren't browsed by the moose population, or rather isn't able to grow on those specific regions. Balsam Fir however, is able to grow in all displayed regions and is browsed by moose for most times.
#Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#Question 19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#Question 20
#a)Looking at the data provided, the count for each saplings in each data are equal to each other. 
#b)It's important to recognize bias in ecological datasets to ensure that facts are presented and is not manipulated. 
#Question 21
#a
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity =  Estimated_Moose_Pop / Area)
#b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean(BrowsingScore), mean(MooseDensity)) 
#Question 23
#a)Yes, moose do show strong preferences at low density and shift to more generalist browsing at higher density, because Black Spruce got a higher browsing score as the moose density increase, adn same can be said for the rest of the sapling species (except for Black Ash). 
#b)Moose mostly favors Willow sapling and apparently despises Black Ash sapling due to its absent color on the graph provided. 
#c)Black Ash is not shown in the figure because the moose population does not care for it.
#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Question 25
#a
moose_coll2 <- moose_coll %>%
  rename(Ecoregion=study_sites)
#b 
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
#Question 26 
#a
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose per sq km", 
     ylab = "Collisions in 2020", 
     main = "Moose Density in Relation to Moose-Vehicle Collisions ")
#b)Collisions increased as moose density increased, but between 0.5 and 0.10 moose density collision unexpectedly increased. 
#Question 27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population", 
     ylab = "Collisions per Capita", 
     main = "Moose-Vehicle Collision per Capita ")
#Question 29
#)Collision decreased as human population increased, this makes sense because as a place gets more populated, rules and laws are regulated to control such accidents from not happening as often.