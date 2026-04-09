#Name: Nayon Park
#Biol R-Studio Assignment 2
#Date: February 6 2025

#Question 1
install.packages("dplyr")
library(dplyr)

#Question 2
moosedata <- read.csv("MoosePopulation.csv")

#Question 3
moose_clean <- na.omit(moosedata)

#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5
year_min <- min(moose_sel$Year)
#1904
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#41250

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, type= "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Question 8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type= "l", 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests over time")

#Question 9
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Question 11
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

#Question 12
sap_reg_browse <- sap_clean %>% 
 group_by(Ecoregion)%>% 
  summarize(AverageBrowsing = mean(BrowsingScore))%>% 
  print()

#Question 13
sap_reg_height <- sap_clean %>% 
  group_by(Ecoregion)%>% 
  summarize(AverageHeight = mean(Height)) %>% 
  print()
sap_reg_height_low <- sap_reg_height %>% 
 filter(AverageHeight < 20) %>% 
 print()

#Question 14
sap_spe_browse <- sap_clean %>% 
  group_by(Species) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore)) %>% 
  print()
avg_browse_spe <- sap_spe_browse %>%
 arrange(desc(AverageBrowsing)) %>%
 print()
#species with highest browsing score is Black_Ash
#species with lowest browsing score is Black_Spruce

#Question 15
fir_reg_browse <- filter(sap_clean, Species == "Balsam_Fir")%>% 
  group_by(Ecoregion)%>% 
  summarize(AverageBrowsing = mean(BrowsingScore))%>% 
  print()

#Question 16
barplot(fir_reg_browse$AverageBrowsing,
 names.arg = fir_reg_browse$Ecoregion,
 xlab = "Ecoregion", 
 ylab = "Average Browsing Intensity", 
 main = "Average Balsam Fir Browsing by each NL Ecoregions",
 col = "forestgreen",
 cex.names = 0.33
)

#Question 17
spruce_reg_browse <- filter(sap_clean, Species == "Black_Spruce")%>% 
  group_by(Ecoregion)%>% 
  summarize(AverageBrowsing = mean(BrowsingScore))%>% 
  print()
barplot(spruce_reg_browse$AverageBrowsing,
 names.arg = spruce_reg_browse$Ecoregion,
 xlab = "Ecoregion", 
 ylab = "Average Browsing Intensity", 
 main = "Average Black Spruce Browsing by each NL Ecoregions",
 col = "forestgreen",
 cex.names = 0.28
)
#Across ecoregions, Average Browsing is higher for Balsam Fir than Black Spruce
#suggesting that Moose prefers to browse Balsam Fir over Black Spruce.

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
#SaplingStudy was not evenly distributed because StraitOfBelleIsleBarrens and Black_Ash
#was underrepresented compared to the rest of the ecoregions and species.
#Bias should be recognized to judge the accuracy of datasets.

#Question 21
moose_2020b <- moose_clean %>%
  filter(Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion',
                       relationship = "many-to-many")

#Question 22
sum_spe_browse <- moose_sap %>% 
  group_by(Species, Ecoregion)%>% 
  summarize(AverageBrowsing = mean(BrowsingScore), 
            AverageMooseDensity = mean(MooseDensity))%>% 
  print()

#Question 23
#There is some evidence as they have stronger browsing preferences at lower moose
#density but even distribution of browsing at higher moose density.
#Moose favours Alder the most and Black Ash the least.
#Some sampling species were filtered out earlier because of low sample sizes or
#insufficient data.

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests",
                 "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25
moose_coll2 <- moose_coll %>% 
  rename(Ecoregion = study_sites)
coll_merge <- moose_2020 %>% 
  left_join(moose_coll2, by = "Ecoregion") %>% 
  print()

#Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose per sq km", 
     ylab = "Moose collisions", 
     main = "Moose Density Relation to Moose Collisions")
#As moose density increases, the moose collisions increase,
#the outlier is over 100 moose collisions for 1.0 moose/km squared but the rest
#generally follows the pattern. 

#Question 27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Humans",
     ylab = "Collisions per capita",
     main = "Moose Collisions Per Capita Versus Humans")

#Question 29
#The graph shows more collisions for lower density places which makes sense in
#why NL has so many moose collisions, because we are a small city.