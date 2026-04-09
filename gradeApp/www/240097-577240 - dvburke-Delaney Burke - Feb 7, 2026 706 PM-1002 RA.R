#My R script for Biology 1002 R Assignment
#Delaney Burke-202510778
#February 7th, 2026

setwd("~/BIOL1002_Rassignment")

#Part I:  Moose Populations in Newfoundland

#Question1
install.packages("dplyr")
library("dplyr")

#Question2
Moosedata <- read.csv("MoosePopulation.csv")

#Question3
View("Moosedata")
moose_clean<-na.omit(Moosedata)

#Question4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question5a
year_min<-min(moose_sel$Year)
#Question5b
moose_max<-max(moose_sel$Estimated_Moose_Pop)

#Question6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Question8b
plot(moose_west$Year, moose_west$MooseDensity, pch=17, type="l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests Ecoregion over time")

#Question9a
moose_2020 <- filter(moosedata2, Year == "2020")
#Question9b
moose_2020_high<-filter(moosedata2, MooseDensity == "")
#Question9c
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))

#Question10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Part II

#Question 11a
saplings<-read.csv("SaplingStudy.csv")
#Question11b
sap_clean<-na.omit(saplings)

#Question12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing=mean(BrowsingScore)) %>%
  print()
#Question12b
avg_browse_reg<-arrange(sap_reg_browse, desc(AverageBrowsing))
  #The ecoregion with the highest average browsing score is Northern_Peninsula_Forests with a score of 4.571429. The ecoregion with the lowest average browsing score is StraitOfBelleIsleBarrens with a score of 1.000000.

#Question13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight=mean(Height)) %>%
  print()
#Question13b
  #The ecoregions Northern_Peninsula_Forests and Western_Forests have average heights less than 20cm.
sap_reg_height_low<-filter(sap_reg_height,AverageHeight<20)

#Question14a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsingBySpecies=mean(BrowsingScore)) %>%
  print()
#Question14b
avg_browse_spe<-arrange(sap_spe_browse, desc(AverageBrowsingBySpecies))
  #Black_Ash has the hightest browsing score of 5.000000 and Black_Spruce has the lowest browsing score of 2.333333.

#Question15
fir_reg_browse <- sap_clean %>%
  filter(Species=="Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(MeanBrowseScore = mean(BrowsingScore)) %>%
  print()

#Question16
barplot(fir_reg_browse$MeanBrowseScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "How Average Browsing Intensity of Balsam Fir Varies by Ecoregions", col = "pink", cex.names = 0.6)

#Question17a
spruce_reg_browse <- sap_clean %>%
  filter(Species=="Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(MeanMooseBrowseScore = mean(BrowsingScore)) %>%
  print()
#Question17b
barplot(spruce_reg_browse$MeanMooseBrowseScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "How Average Browsing Intensity of Black Spruce Varies by Ecoregions", col = "yellow", cex.names = 0.6)
#Question17c
  #Black Spruce browsing intensity in lower than Balsam Fir browsing intensity in avalon forests. Other regions are not easily comparable because their data is not shown on the graph. 

#Question18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
  #No, majority did have 5 samplings but others had a different amount of samplings.

#Question19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Question20a
  #I think the SaplingStudy data set is not evenly distributed. The north shore forests have 7 observations and the northern peninsula forests have 8 observations, meaning they are over represented. The maritime barrens have 3 observations and the strait of belle isle barrens have 1 observation, meaning they are underrepresented. For the plants, Balsam Fir is over represented while Black Ash in underrepresented.  
#Question20b
  #It is important to recognize bias is ecological datasets because uneven distribution can show patterns that are not true because of this uneven distribution. Ensuring there is no bias helps make clear, accurate conclusions of the data collected. 

#Part III

#Question21a
moose2020<-filter(moose_clean, Year==2020)
moose_2020b<-mutate(moose2020, MooseDensity = Area/Year)
#Question21b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Question22
sum_spe_browse<- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AverageBrowsingScore=mean(BrowsingScore), AverageMooseDensity=mean(MooseDensity)) %>% 
  print()

#Question23a
  #Yes, at low moose densities moose feed on more palatable plants, and higher moose densities they are less selective due to compitition. So, browsing score increases at higher densities and decreases at lower densities. 
#Question23b
  #Moose favour willow the most, I know this because this is the most common plant at higher broswing scores. Black spruce is browsed the least, I know this beacuse it is most common across low broswing score. 
#Question23c
  #Black ash is not shown because it only occured once in the dataset, so it was irrelevent. 

#Question24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question25a
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
#Question25b
coll_merg<-left_join(moose_coll2, moose_2020, by = 'Ecoregion', relationship = "many-to-many")

#Question26a
plot(coll_merg$MooseDensity, coll_merg$collisions2020,
     xlab = "Moose Density", 
     ylab = "Number of Collisions", 
     main = "Moose density Related to Number of Moose-Vehicle Collisions")
#Question26b
  #I see a trend of as moose density increases the number of collisions  tends to increase. An outliar is at approximately 1.0 moose density with 100 collisions.

#Question27
coll_merge_per_capita<-mutate(coll_merg, coll_per_capita = collisions2020 / human_pop)
  #Northern Peninsula Forests have the highest number of moose collisions per person, the value being 0.0050000000.

#Question28
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop,
     xlab = "Moose collisions per person", 
     ylab = "Human Population", 
     main = "Moose Collision per Person vs Human Population") 

#Question29
  #As human population decreases, the number of moose collisions per person decreases. This trend does not make sense because in smaller towns where there are smaller populations, moose are more likely to be there making it more likely for a moose accident. But in a larger place like St. Johns, moose aren't likely to be seen in the city where the high population is, so there should be less frequent accidents, not more frequent. 