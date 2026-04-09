install.packages("dplyr")
library("dplyr")
moosedate <- read.csv("MoosePopulation.csv")
View("MoosePopulation.csv")
moose_clean <- na.omit(moosedate)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min("1904")
moose_max <- max("41250")
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_2020 <- filter(moosedata2, moosedata2$Year == "2020")
moose_2020_high <- filter(moosedata2, moosedata2$MooseDensity >= 2.)
moose_2020_high_byD <- arrange(moosedata2, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_reg_browse)
avg_browse_reg <- arrange(sap_reg_browse,desc(AverageBrowsing))
#Highest: Northern_Peinsula_Forest Lowest: StraitOffBelleIsleBArrens
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height))
print(sap_reg_height)
#Northern_peninsula_Forest and Western_forest
sap_reg_height_low <- sap_clean %>% filter(Height<=20)
print(sap_reg_height_low)
sap_spe_browse <-sap_clean %>%
  group_by(Species)%>%
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_spe_browse)
avg_browse_spe<- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
#Highest Browsing Score: Block_Ash Lowest Browsing Score: Black_Spruce
fir_reg_browse <- sap_clean %>%
  filter(Species=="Balsam_Fir") 
group_by(Ecoregion) %>%
  summarize(AverageBrowsing= mean(BrowsingScore))
barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "BrowsingScore", main = "Average Browsing Intesity of Balsam Fir", col = "grey", cex.names=0.6) 
spurce_reg_browse <- sap_clean %>%
  filter(Species=="Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing= mean(BrowsingScore)) %>%
  print()
#They are very similar as they both stay with in a 0.5-4.5 region
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_reg_tally %>%
  group_by(Ecoregion) %>%
  tally(n) %>% 
  print()
#For the most part the Ecoregions are evenly represented, with areound 5 each. However StaitOfBelleIsleBarrens and Maritime_Barrens are underrepresented. North_Shore_Forest and Northern_Penisula_Forests are slighty overrepresented.
#Bias in ecological datasets can lead to unacurate data and incorrect assumptions about the ecosystem. Elmiating bias is impportant for accurate results.        
moose_2020B <- moose_clean %>% 
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020B, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sap_spe_browse <- sap_clean %>%
  group_by(Species,Ecoregion) %>%
  summarize(AdverageBrowsing= mean(BrowsingScore))
print(sum_spe_browse)
#Yes, moose trend to show stong preferences for certain types of browse when their population density is low. 
#Moose favor the willow sapling species. They browse black_spruce the least.
#Black_Ash becasue there are no Moose near it so there is no data.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll <- moose_coll %/%
  + rename_with(.fn + moose_coll2 <- moose_coll)
coll_merge <- left_join(moose_coll, moose_2020, by = 'Ecoregion', relationship = "many-to-many")
plot(moose_2020, collisions2020, 
     xlab = "density", 
     ylab = "number_collisons", 
     main = "Moose density compared to collisions")
#The number of collisions trends to increase when the moose density is higher however there are a few that are out of place
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita, human_pop, 
     xlab = "collisions", 
     ylab = "population", 
     main = "population compared to collisions")
#Yes the trends in the graph makes sense, with a greater population there is a higher chance of collsions.The is shown on the graph.  

  

  
  
