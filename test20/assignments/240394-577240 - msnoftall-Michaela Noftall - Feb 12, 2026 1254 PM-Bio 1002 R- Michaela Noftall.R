install.packages("dplyr")
library(dplyr)
moose<-read.csv("MoosePopulation (1).csv")
sapling<-read.csv("SaplingStudy.csv")
colnames(moose)
colnames(sapling)
str(moose)
str(sapling)
#question 1
library(dplyr)
#question 2
library(dplyr)
moosedata<-read.csv("MoosePopulation (1).csv")
#question 3
library(dplyr)
View(moosedata)
moose_clean<-na.omit(moosedata)
#question 4
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5
#a)
year_min<-min(moose_sel$Year)
year_min
#b)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
moose_max
#question 6
library(dplyr)
moosedata<-read.csv("MoosePopulation (1).csv")
moose_clean<- na.omit(moosedata)
moose_sel<- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moosedata2<- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop/Area)
head(moosedata2)
#question 7
library(dplyr)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type= "l",
     xlab= "Year",
     ylab= "Moose per sq km",
     main= "Moose density in Newfoundland")
#question 8
moose_west<-filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type= "l",
     xlab= "Year",
     ylab= "Moose per sq km",
     main= "Moose density in western_forests")
#question 9
#a)
moose_2020<-filter(moosedata2, Year==2020)
#b)
moose_2020_high<-filter(moose_2020, MooseDensity>2.0)
#c)
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
#question 10
  moosefinal<-moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#question 11
library(dplyr)
saplings<-read.csv("SaplingStudy.csv")
sap_clean<-na.omit(saplings)
#question 12
#a)
sap_reg_browse<-sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#b)
avg_browse_reg<-sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#question 13
#a)
sap_reg_height<-sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
#b
sap_reg_height_low<-sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
#question 14
#a)
sap_spe_browse<-sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#b)
avg_browse_spe<-sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#question 15
fir_reg_browse<-sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
#question 17
#a)
spruce_reg_browse<-sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#b)
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
#c)
#Black Spruce shows lower browsing intensity than balsam Fir in most ecoregions
#indicating moose prefer browsing balsam fir
#question 18
sap_reg_tally<-sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#question 19
sap_spe_tally<-sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#question 20
#a)
#the dataset is not evenly distributed; some ecoregions and species have 
#more saplings counted than others indicating sampling bias
#b)
#sampling bias can misrepresent browsing patterns and lead to incorrect
#conclusions about moose preferences across regions and species
#question 21
#a)
library(dplyr)
moose<-read.csv("MoosePopulation (1).csv")
moose_clean<-na.omit(moose)
moose_2020b<-moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop/Area)
moose_sap<-left_join(moose_2020b, sap_clean, by = "Ecoregion")
#b)
moose_sap<-left_join(moose_2020b, sap_clean, by = "Ecoregion")
#question 22
sum_spe_browse<-moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
#question 23
#a) Yes. at low moose density, browsing is concentrated on preferred species
#but at higher densities browsing becomes more spread across species
#b) moose favour willow and alder. they browse black spruce the least
#c) ash is not shown because there weren't enough observations
#question 24
study_sites<-c("Avalon_Forests", "Eastern_HyperOceanic", "Central_Newfoundland", "Western_Newfoundland", "Northern_Peninsula")
collisions2020<-c(120, 95, 60, 80, 40)
human_pop<-c(270000, 80000, 30000, 50000, 15000)
moose_coll<-data.frame(study_sites, collisions2020, human_pop)
#question 25
moose_coll2<-moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge<-left_join(moose_2020b, moose_coll2, by = "Ecoregion")
#question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020)
#as moose density increases collisions also increase, 2 regions are mild outliers
#question 27
coll_merge_per_capita<-coll_merge %>%
  mutate(coll_per_capita = collisions2020/human_pop)
#question 28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita)
#question 29
#regions with smaller human populations often have higher collisions per person.
#because moose habitats overlaps more with roads in less populated areas

