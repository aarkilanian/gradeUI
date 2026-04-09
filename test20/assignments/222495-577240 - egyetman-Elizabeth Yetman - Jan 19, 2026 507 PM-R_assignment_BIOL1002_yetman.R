getwd()
library(dplyr)
Moosedata<-read.csv("MoosePopulation.csv")
View(Moosedata)
na.omit(Moosedata)
Moosedata<-na.omit(Moosedata)
Moosedata<-select(Moosedata,Ecoregion,Year,Area,Estimated_Moose_Pop)
min(Moosedata$Year)
# Question 5: The oldest observation in the dataset is from Western forests in 1904, when the estimated moose population was 4.
max(Moosedata$Estimated_Moose_Pop)
#Question 5, b): This number was for the Central Forests ecoregion of Newfoundland, not all of Newfoundland.
MooseDensity<-mutate(Moosedata,MooseDensity=Estimated_Moose_Pop/Area)
Moosedata<-MooseDensity
plot(Moosedata$Year,Moosedata$MooseDensity,type="l", xlab="Year",ylab="Moose per sq km", main="Moose density in Newfoundland Ecoregions over Time")
MooseDataWest<-filter(Moosedata, Ecoregion=="Western_Forests")
plot(MooseDataWest$Year,MooseDataWest$MooseDensity, type="l",xlab="Year",ylab="Moose per sq km", main="Moose Density in Newfoundland Western Forest Ecoregion Over Time")
MooseData_2020<-filter(Moosedata, Year==2020)
MooseData_2020_b<-filter(MooseData_2020,MooseDensity>2)
arrange(MooseData_2020_b, desc(MooseDensity))
MooseData_Final<-Moosedata%>%
  filter(Year==2020)%>%
  filter(MooseDensity>2.0)%>%
  arrange(desc(MooseDensity))%>%
  print()
Saplings<-read.csv("SaplingStudy.csv")
View(Saplings)
Saplings<-Moose_Browsing_By_Ecoregion<-Saplings%>%
  na.omit(Saplings)
Moose_Browsing_By_Ecoregion<-Saplings%>%
  group_by(Ecoregion)%>%
  summarize(meanBrowsingScore =mean(BrowsingScore))
print(Moose_Browsing_By_Ecoregion)
# Question 12, b): The Northern Peninsula Forests ecoregion has the most moose browsing. The Strait of Belle Isle Barrens ecoregion has the least moose browsing.
Mean_Height_by_Ecoregion<-Saplings%>%
  group_by(Ecoregion)%>%
  summarize(mean_Height = mean(Height))
print(Mean_Height_by_Ecoregion)
SeverelyBrowsedEcoregions<-Mean_Height_by_Ecoregion%>%
  filter(mean_Height<20)
print(SeverelyBrowsedEcoregions)
#Question 13, b): The Northern Peninsula Forests ecoregion and the Western Forests ecoregion are severely browsed, because their average heights are less than 20cm. 
Mean_Browsing_by_Species<-Saplings%>%
  group_by(Species)%>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
print(Mean_Browsing_by_Species)
#Question 14, b): The Black Ash has the highest browsing score, and the Black Spruce has the lowest browsing score.
BalsamFir<-Saplings%>%
  filter(Species=="Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
print(BalsamFir)
barplot(BalsamFir$mean_BrowsingScore,names.arg=BalsamFir$Ecoregion, xlab="Ecoregion", ylab="Average Balsam Fir Browsing Score", main="Average Moose Browsing Intensity on Balsam Fir by Ecoregion", col="forestgreen",cex.names=0.6)
BlackSpruce<-Saplings%>%
  filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarize(mean_BrowsingScore=mean(BrowsingScore))
print(BlackSpruce)
barplot(BlackSpruce$mean_BrowsingScore,names.arg=BlackSpruce$Ecoregion,xlab="Ecoregion", ylab="Average Black Spruce Browsing Score",main="Average Moose Browsing Intensity on Black Spruce by Ecoregion", col="limegreen",cex.names=0.6)
#Question 17, c): Of the Balsam Fir and the Black Spruce, the mean browsing score across ecosystems is higher in the Balsam Fir. This means that the moose prefer browsing Balsam Fir over Black Spruce saplings in this study.
EcoregionTally<-Saplings%>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()
SpeciesTally<-Saplings%>%
  group_by(Species)%>%
  tally()%>%
  print()
#Question 19: No, the name number of tree saplings were not counted in each region.
#Question 20, a): No, I do not think the SaplingStudy dataset is evenly distributed, because the number of saplings from each species in the sample varied from n=1 to n=11. The Balsam Fir saplings are overrepresented (n=11) and the Black Ash saplings are underrepresented with only one sapling of this species included in the sample. All other sapling species had a number of saplings where n=8±1.
#Question 20, b): It is important to recognize bias in ecological datasets because if samples and tests are not being performed equally across all groups, then results will not be accurate and will likely be scewed in favour of one variable. The results of the study may lead to misleading conclusions that do not accurately represent the environment studied. 
View(MooseData_2020)
MooseSaplingData<-left_join(MooseData_2020,Saplings, by='Ecoregion',relationship="many-to-many")
BrowsingBySpeciesDensity<-MooseSaplingData%>%
  group_by(Species,Ecoregion)%>%
  summarize(mean_BrowsingScore=mean(BrowsingScore),mean_MooseDensity=mean(MooseDensity))
print(BrowsingBySpeciesDensity)
#Question 23, a): Yes, there is evidence that supports the researchers' hypothesis because moose show strong preferences at low density and shift to more generalist at higher density. At lower density, the browsing score is higher for the Willow and Alder trees and lower for all other species, meaning that the moose have a preference for the two trees with the highest browsing scores. At moose density the browsing scores remain relatively high (between 3-5) for all species of saplings, meaning all species are being eaten by the moose and the moose cannot show an evident preference because there are simply more moose eating them.
#Question 23, b) The moose favour the Willow saplings the most, and the Black Spruce the least. This is evident in the graph because the Black Spruce have the lowest browsing score, relative to the other species, among all levels of moose density. The Willow saplings have the highest browsing score across all levels of moose density, meaning that they are the most browsed by moose overall. 
#Question 23, c): The figure does not show the Black Ash species of sapling because this species was only observed in one ecoregion in 2020. There is not enough data to apply this observation to the whole sample of moose. The browsing intensity cannot be accurately generalized or interpreted across different moose density levels for this species.
Collisions_2020<-c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation<-c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
StudySites<-c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
MooseCollisions<-data.frame(StudySites,HumanPopulation,Collisions_2020)
#Question 25, a) #The code from question 21 does not work because the datasets MooseData and MooseCollisions do not have a common collumn name (variable) to left join them ("Ecoregions"=/="StudySites"). 
MooseCollisions<-MooseCollisions%>%
  rename(Ecoregion=StudySites)
View(MooseCollisions)
MooseCollisions2020<-left_join(MooseData_2020,MooseCollisions, by='Ecoregion', relationship="many-to-many")
plot(MooseCollisions2020$MooseDensity,MooseCollisions2020$Collisions_2020,xlab="Moose Density per sq km", ylab="Number of Moose-Vehicle Collisions (2020)",main="Average Moose Density on Moose-Vehicle Collisions in 2020")
# Question 26, b): There is an upward trend evident on the graph. As Moose Density increases, the number of Moose-Vehicle collisions increases. There is an outlier where Moose Density equals 0.9619048 moose per square kilometer, where the number of moose-vehicle collisions is extremely higher than the rest of the data points (110 moose-vehicle collisions compared to 40-48 collisions for similar densities). This value is an outlier because it is the only data point out of the total 9 data points that is outside of the approximate linear trend. 
MooseCollisions2020<-MooseCollisions2020%>%
  mutate(CollisionsPerCapita=Collisions_2020/HumanPopulation)
plot(MooseCollisions2020$HumanPopulation,MooseCollisions2020$CollisionsPerCapita,xlab="Human Population", ylab="Number of Moose-Vehicle Collisions Per Capita (2020)",main="Number of Moose-Vehicle Collisions vs. Human Population in 2020")
#Question 29: Yes, this data makes sense. Where the population was smaller, there were more moose-vehicle collisions per capita. In Newfoundland, there are smaller populations in more rural, remote communities. These areas have more forest areas and more habitats for moose, and less cars on the roads means moose may be more likely to cross the road regularly. This increases the likelihood of a moose-vehicle collision. 