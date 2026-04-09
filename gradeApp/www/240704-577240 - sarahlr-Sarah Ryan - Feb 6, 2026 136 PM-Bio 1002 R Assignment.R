#Name: Sarah Ryan
#Student Number: 202511573
#Bio 1002 Assignment
#Due February 13th 2026, Completed February 6th 2026
#Part 1 Moose Populations in Newfoundland
#Question 1
install.packages("dplyr")
library(dplyr)
#Question 2
library(readr)
MoosePopulation<- read_csv("Bio 1002 Data/MoosePopulation.csv")
#Question 3
MoosePopulation<-na.omit(MoosePopulation)
#Question 4
MoosePopulation<-select(MoosePopulation, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5
#5.a)
oldest_year<-min(MoosePopulation$Year)
print(oldest_year)
#Answer 5.a) Oldest Year is 1904
#5.b)
highest_EMP<-max(MoosePopulation$Estimated_Moose_Pop)
print(highest_EMP)
#Answer 5.b) Highest Estimated Moose Population is 41250, This is for Central_Forests Ecoregion not for all of Newfoundland
#Question 6
MoosePopulation<-mutate(MoosePopulation, MooseDensity=Estimated_Moose_Pop/Area)
#Question 7
plot(MoosePopulation$Year, MoosePopulation$MooseDensity, xlab="Year", ylab="Moose Per sq km", main="Moose Density in Newfoundland Ecoregions Over Time")
#Question 8
#8.a)
MoosePopulationWest<-filter(MoosePopulation, Ecoregion=="Western_Forests")
#8.b)
plot(MoosePopulationWest$Year, MoosePopulationWest$MooseDensity, xlab="Year", ylab="Moose Per sq km", main="Moose Density in Newfoundland Western Region Ecoregions Over Time", type="l")
#Question 9
#9.a)
MoosePopulation_2020<-filter(MoosePopulation, Year=="2020")
#9.b)
MoosePopulation_2020_b<-filter(MoosePopulation, MooseDensity>2.0)
print(MoosePopulation_2020_b)
#9.c)
MoosePopulation_2020_b_SD<-arrange(MoosePopulation_2020_b, desc(MooseDensity))
print(MoosePopulation_2020_b_SD)
#Question 10
MoosePopulation_FinalData<-MoosePopulation%>%
  filter(Year==2020)%>%
  filter(MooseDensity>2.0)%>%
  arrange(desc(MooseDensity))%>%
  print(.)
#End of Part 1
#Part 2 Tree Sapling Study
#Question 11
#11.a)
SaplingStudy<-read_csv("Bio 1002 Data/SaplingStudy.csv")
#11.b)
sap_clean<-na.omit(SaplingStudy)
#Question 12
#12.a)
sap_reg_browse<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean_browse=mean(BrowsingScore))%>%
  print(.)
#12.b)
avg_browse_reg<-sap_reg_browse%>%
  arrange(desc(mean_browse))%>%
  print(.)
#Answer 12.b)The Northern_Peninsula_Forests had the highest average browsing score (4.57) and the StraitOfBelleIsleBarrens had the lowest (1)
#Question 13
#13.a)
sap_reg_height<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean_height=mean(Height))%>%
  print(.)
#13.b)
sap_reg_height_low<-sap_reg_height%>%
  filter(mean_height<20)%>%
  print(.)
#Answer 13.b)The Ecoregions with an average height less than 20cm are the Northern_Peninsula_Forests(19.9) and Western_Forests(18.9)
#Question 14
#14.a)
sap_spe_browse<-sap_clean%>%
  group_by(Species)%>%
  summarise(mean_browse=mean(BrowsingScore))%>%
  print(.)
#14.b)
avg_browse_spe<-sap_spe_browse%>%
  arrange(desc(mean_browse))%>%
  print(.)
#Answer 14.b)The species with the highest browsing score is Black_Ash (5), the species with the lowest is Black_Spruce (2.33)
#Question 15
fir_reg_browse<-sap_clean%>%
  filter(Species=="Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(mean_browse=mean(BrowsingScore))%>%
  print(.)
#Question 16
barplot(fir_reg_browse$mean_browse, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Effect of Ecoregion on the Average Browsing Score of the Balsam Fir Species", col = "darkgreen", cex.names = 0.6)
#Question 17
#17.a)
spruce_reg_browse<-sap_clean%>%
  filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(mean_browse=mean(BrowsingScore))%>%
  print(.)
#17.b)
barplot(spruce_reg_browse$mean_browse, names.arg = spruce_reg_browse$Ecoregion,xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Effect of Ecoregion on the Average Browsing Score of the Black Spruce Species", col = "lightgreen", cex.names = 0.6)
#17.c)
#The Black Spruce Average Browsing Scores are lower than those of the Blasam Fir species. The EasternHyperOceanicBarrens and Maritime_Barrens browsing scores are 0 for the Black Spruce however, they are 2 for the Balsam Fir along with the other scores for Balsam fir also having scores greater than 4 (North_Shore_Forests and Northern_Peninsula_Forests) which are only 4 for the Black Spruce.
#Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print(.)
#No the same number of tree saplings were not counted in each Ecoregion, they range from 1-8.
#Question 19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print(.)
#No the same number of tree saplings were not counted for each species, they range from 1-11.
#Question 20
#20.a)
#I think that the SaplingStudy Dataset is not evenly distributed. As shown in the previous two questions the Ecoregion North_Shore_Forests had a tally of 8 while the StraitOfBelleIsleBarrens only had 1. There is a similar situation with the species where the Balsam_Fir is the most represnted with 11 while the Black_Ash only has 1 and therefore, is underrepresented compared to the others.
#20.b)
#It is important to recognize bias in ecological datasets so that we are still open minded to the possibility of other data being possible. For example in an underrespresented dataset if it had been affected by another condition or is a data outlier, it may not be a true representation of what another sample would be. 
#End of Part 2
#Start of Part 3
#Question 21
#21.a)
MoosePopulation_2020<-filter(MoosePopulation, Year=="2020")
moose_2020b<-mutate(MoosePopulation_2020, MooseDensity)
#21.b)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#Question 22
sum_spe_browse<-moose_sap%>%
  group_by(Species, Ecoregion)%>%
  summarise(mean_browse=mean(BrowsingScore),mean_density=mean(MooseDensity))%>%
  print(.)
#Question 23
#23. Extra Graph Given In Instructions
#23.a)
#There is evidence that supports the researchers hypothesis, At higher moose densities there also seemed to be a higher browsing score indicating that they may have been less selective as predicted in the hypothesis. Moose do seem to be more selective at lower densities and shift to more general at higher densities, as the moose density increased so did the average browsing score as a whole and the data is shown to be more uniformly distributed.
#23.b)
#Moose favour the willow species the most. They browse Black_Spruce the least.
#23.c)
#Black_ash is not shown in the figure because when tallied in question 19 Black_Ash was only shown to have 1 of its species while the others all had 7 or more, however it did have the highest average broswing score of 5. Therefore, since it did not have as much data as the other species, it is not shown in the graph since it may be a data outlier.
#6.0.1 Moose-vehicle Collisions
#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Question 25
#25.a)
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
#25.b)
coll_merge<-left_join(moose_coll2, MoosePopulation_2020, by = "Ecoregion", relationship = "many-to-many")
coll_merge<-left_join(moose_coll2, moose_2020b, by = "Ecoregion", relationship = "many-to-many")
#Question 26
#26.a)
plot(coll_merge$MooseDensity,coll_merge$collisions2020, xlab="Moose Density", ylab="Collisions 2020", main="Moose Density in Relation to the Amount of Moose-Vehicle Collisions in 2020")
#26.b)
#The trend being been is that when the Amount of Moose (MooseDensity) increased so did the amount of Moose-Vehicle Collisions in 2020. There was a data outlier at density of just less than 1(Average) where there was over 100 collisions.
#Question 27
coll_merge_per_capita<-mutate(coll_merge,coll_per_capita=collisions2020/human_pop)
#Question 28
plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita, ylab="Collisions in 2020 Per Person", xlab="Human Population", main="The Effect of the Human Population on the Amount of Moose-Vehicle Collisions per person in 2020")
#Question 29
#The amount of Moose-Vehicle Collisions per person decreased as the human population grew. Since in 2020 there was covid, not a lot of people were driving, therefore, even though the human population grew, there were more people staying inside in Newfoundland which contributed towards the decrease in Moose-Vehicle Collisions per person in 2020.