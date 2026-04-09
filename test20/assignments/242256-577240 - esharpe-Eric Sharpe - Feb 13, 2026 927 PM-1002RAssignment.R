install.packages("dplyr")
    ###PART 1###
  ###question 1###
library(dplyr)
  ###question 2###
moosedata <- read.csv("MoosePopulation.csv")
  ###question 3###
moose_clean <- na.omit(moosedata)
  ###question 4###
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
  ###question 5 (a)###
year_min <- min(moose_sel$Year)
  ###question 5 (b)###
moose_max <- max(moose_sel$Estimated_Moose_Pop)
  ###question 6###
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
  ###question 7###
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "# of Moose per sq km", 
     main = "Newfoundland Moose Density Over Time")
  ###question 8 (a)###
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
  ###question 8 (b)###
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "# of Moose per sq km", 
     main = "Newfoundland Moose Density Over Time in Western Forests")
  ###question 9 (a)###
moose_2020 <- filter(moosedata2, moosedata2$Year == 2020)
  ###question 9 (b)###
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
  ###question 9 (c)###
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
  ###question 10###
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
    ###PART 2###
  ###question 11 (a)###
saplings <- read.csv(file = "SaplingStudy.csv")
  ###question 11 (b)###
sap_clean <- na.omit(saplings)
  ###question 12 (a)###
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(BrowsingScore = mean(BrowsingScore)) %>%
  print()
  ###question 12 (b)###
avg_browse_reg <- arrange(sap_reg_browse, desc(BrowsingScore)) #Highest: Northern Peninsula Forests (4.571429), Lowest: Strait of Belle Isle Barrens (1.000000)
  ###question 13 (a)###
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(Height = mean(Height)) %>%
  print()
  ###question 13 (b)###
sap_reg_height_low <- filter(sap_reg_height, Height < 20) %>%
  print() #Height < 20cm: Northern Peninsula Forests (19.9), Western Forests (18.9)
  ###question 14 (a)###
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(BrowsingScore = mean(BrowsingScore)) %>%
  print()
  ###question 14 (b)###
avg_browse_spe <- arrange(sap_spe_browse, desc(BrowsingScore)) #Highest Browsing Score: Black Ash (5.000000), Lowest: Black Spruce (2.333333)
  ###question 15###
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(BrowsingScore = mean(BrowsingScore)) %>%
  print()
  ###question 16###
barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Browsing Intensity", main = "Moose Browsing Intensity of Balsam Fir Trees Based on Newfoundland Ecoregion", col = "forestgreen", cex.names = 0.6)
  ###question 17 (a)###
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(BrowsingScore = mean(BrowsingScore)) %>%
  print()
  ###question 17 (b)###
barplot(spruce_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Browsing Intensity", main = "Moose Browsing Intensity of Black Spruce Trees Based on Newfoundland Ecoregion", col = "forestgreen", cex.names = 0.6)
  ###question 17 (c)###
#black spruce trees tend to have a lower average browsing intensity than balsam firs.The biggest change in browsing intensity is in the "maritime barrens" ecoregion.
  ###question 18###
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
  ###question 19###
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
  ###question 20 (a)###
#the SaplingStudy data is not evenly distributed. This is shown by the disparity between the 11 samples of balsam fir trees recorded, and the single black ash.
  ###question 20 (b)###
#It is important to recognize bias in ecological datasets, because when there is a single point of data for one set, and many for another, they can not be fairly compared. The point of said dataset is to compare these samples, so sample bias skews the scientific environment.
    ###PART 3###
  ###question 21 (a)###
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) 
  ###question 21 (b)###
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
  ###question 22###
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(MooseDensity = mean(MooseDensity), BrowsingScore = mean(BrowsingScore)) %>%
  print()
  ###question 23 (a)###
#There is no evidence to support the researchers' hypothesis. From the graph, it can be shown that moose browse far more in higher densities and more generally in lower densities--the opposite of what the researchers claim.
  ###question 23 (b)###
#The moose prefer to browse willows the most, while they browse black spruce the least.
  ###question 23 (c)###
#Black ash is not shown, as there is only one datapoint for it, nullifying the validity of its inclusion as a variable.
  ###question 24###
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
  ###question 25 (a)###
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
  ###question 25 (b)###
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', relationship = "many-to-many")
  ###question 26 (a)###
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density", 
     ylab = "Collisions in 2020", 
     main = "Newfoundland Moose Density Compared to Collisions in 2020")
  ###question 26 (b)###
#Generally, lower moose density leads to lower collisions, however at 1.0 moose density, there is an outlier value of over 100 collisions. 
  ###question 27###
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
  ###question 28###
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita", 
     main = "Moose Collisions Per Capita Compared to Human Population")
  ###question 29###
#As shown by the graph, areas with a lower human population have a much higher rate of collisions per capita than more densely populated areas. This makes sense when taking into consideration the fact that many areas with lower populations have more wilderness (and thus more moose to collided with) than larger areas which are more urbanized (have less wilderness, and thus less moose to collide with).