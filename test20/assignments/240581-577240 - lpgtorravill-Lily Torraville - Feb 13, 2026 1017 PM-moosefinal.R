install.packages(c('dplyr', 'readr', 'tidyr'))
library(dplyr)
View(MoosePopulation.csv)
moosedata <- read.csv("MoosePopulation.csv")
date1 <- read.csv(file = "MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_clean$Year)
year_min<-min(moose_clean$Year)
max(moose_clean$Estimated_Moose_Pop)
moose_max<-max(moose_clean$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "year",
     ylab = "moose per sq km",
     main = "moose density in newfoundlands western forests ecoregions over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity == "<2.0")
arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
  filter (Year == 2020) %>%
  filter (MooseDensity > 2.0) %>%
  arrange (desc (MooseDensity)) %>%
  print()

saplings <- SaplingStudy
sap_clean<-na.omit (saplings) 
sap_reg_browse<-sap_clean%>%
  group_by (Ecoregion)%>%
  summarise (AverageBrowsing=mean(BrowsingScore))%>%
  print()
avg_browse_reg<-arrange(sap_reg_browse, AverageBrowsing)
view(avg_browse_reg)
   #the lowsest average browsing score is StraitOfBelleIsLeBarrens and the highest score is Northern_Peninsula_Forests

sap_reg_height<-sap_clean%>%
  group_by (Ecoregion)%>%
  summarize (AverageHeight=mean(Height))%>%
  print ()
sap_reg_height_low<-sap_reg_height%>%
  filter(AverageHeight<20)%>%
  print()
#the ones that have less then 20cm are Northen_Peninsula_Forests and Western_Forests.

sap_spe_browse<-sap_clean%>%
  group_by (Species)%%
  summarise(AverageBrowsingSpe=mean (BrowsingScore))%>%
  print()
avg_browse_spe<-arrange(sap_spe_browse, AverageBrowsingSpe)
view(avg_browse_spe)
#the lowset browsing score is Black_Spruce and the highest is Black_ash

fir_reg_browse<-sap_clean%>%
  Filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(AverageBalsmBrowsing=mean(BrowsingScore))%>%
  print ()
barplot(fir_reg_browse$AverageBalsmBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "ecoregions", ylab = "browsing score")

spruce_reg_browse<-sap_clean%>%
  filter (Species == "Black_Spruce")%>% 
  group_by (Ecoregion)%>% 
  summarise(Averagesprusebrowsing=mean(BrowsingScore))%>%
  print ()
barplot(spruce_reg_browse$Averagesprusebrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "ecoregions", ylab = "browsing score")

sap_spe_tally<-sap_clean%>%
  group_by(Species)%>%
  tally()%>%
  print()

#no, the sapling study is not very evenly distributed, an example for my reasoning because we are giving 5 data sets from centre yet there is 9 data sets from northern.
#it is very Important to recognize bias while analyzing data due to the bais making the data unreliable and not accurate.

moose_2020b<-moose_clean%>%
  filter(Year == 2020)%>%
  mutate(MooseDensity=Estimated_Moose_Pop)%>%
  print ()
moose_sap <- left_join (moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

sum_spe_browse<-moose_sap%>%
  group_by(Species)%>%
  group_by(Ecoregion)%>%
  summarise(averagemoosebrosing=mean(MooseDensity))%>%
  
  #a) there's evidence that do support the rearchers on how the moose strong preferences at low density and shift to more generalist browsing at higher density. with the lower  density showing more interest in willow and alder, while the higher density leaning towards White birch and Balsam fir. 
  #b) the sapling species that the moose favor would be the willow. the species of sapling that the moose do not favor would be the Black Spruce.
  #c) the sapling species that wasnt displayed in the graph  was the Blackash. the resaoning for this is because there was too little to no braising. 
  
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)   
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests", "Western_Forests")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
  rename_with(~ "Ecoregion", .cols = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
length(coll_merge$collisions2020)

plot(coll_merge$collisions2020, coll_merge$Estimated_Moose__Pop,
     xlab = "Number of Moose-Vehicle Collisions", 
     ylab = "Moose Density",
     main = "Moose Density vs. Moose-Vehicle Collisions")
#a trend that i noticed while working on this is that the higher the population, the more collison has occured.

coll_merge_per_capita<-coll_merge%>%
  mutate(coll_per_capita=collisions2020/human_pop)%>%
  print()

plot(coll_merge_per_capita$coll_per_capita,coll_merge_per_capita$human_pop,
     xlab = "# of collions per capita", 
     ylab = "human population",
     main = "number of colliosions per capita vs. human population")
