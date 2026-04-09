Brianna Walsh

#Question 1
install.packages("dplyr")
library(dplyr)

#Question 2
moosedata<-read.csv("MoosePopulation.csv")

#Question 3
moose_clean<-na.omit(moosedata)

#Question 4
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop) 

#Question 5
min(moose_sel$Year) #1904
max(moose_sel$Estimated_Moose_Pop) #41250

#Question 6
moosedata2<-mutate(moose_sel, MooseDensity = Estimated_Moose_Pop/Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab="year",
     ylab="Moose per sq km",
     main="Moose density in Newfoundland ecoregions over time",
     type="l")

#Question 8
MoosedataWest<-filter(moosedata2, Ecoregion=="Western_Forests")
plot(MoosedataWest$Year,MoosedataWest$MooseDensity,
     xlab="year",
     ylab="Moose in sq km",
     main="Moose density in Western Forest over time",
     type="l")

#Question 9 
moose_2020<-filter(moosedata2, Year==2020)
moose_2020_high<-filter(moose_2020,MooseDensity>=2.0)
moose_2020_high_byD<-arrange(moose_2020_high,
                             desc(MooseDensity))

#Question 10
moosefinal<-moosedata2 %>%
  filter(Year==2020) %>%
  filter(MooseDensity>2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#PART 2 Sapling Study

#Question 11
saplings<-read.csv(file="SaplingStudy.csv")
sap_clean<-na.omit(saplings)

#Question 12 
sap_reg_browse<- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing=mean(BrowsingScore, na.rm=TRUE)) %>%
  print() 

#Northern Peninsula and North Shore had the highest browsing scores
#Straight of Bell Isle and maritime barrens had the lowest browsing scores
 
   #Question 13
TreeHeight_byEcoregion<-sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Height=mean(Height,na.rm=TRUE)) %>%
  print ()
SeverlyBrowsed<-TreeHeight_byEcoregion %>%
  filter(Mean_Height<20) %>%
  print()

#Northern Peninsula and Western forests had average heights less than 20cm

#Question 14
Browsing_bySpecies<-sap_clean %>%
  group_by(Species) %>%
  summarize(Mean_Browsing=mean(BrowsingScore,na.rm=TRUE)) %>%
  print()

#Black ash has the highest browsing, and black spruce has the lowest.

#Question 15
BalsamFir <-sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing=mean(BrowsingScore,na.rm=TRUE)) %>%
  print()

#Question 16
barplot(BalsamFir$Mean_Browsing,
        names.arg=BalsamFir$Ecoregion,
        xlab="Ecoregion",
        ylab="Average Moose Browsing Score",
        main="Average Balsam Fir Browsing Intensity by Ecoregion",
        col="darkblue",
        cex.names=0.6)

#Question 17
BlackSpruce<-sap_clean %>%
  filter(Species=="Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing=mean(BrowsingScore, na.rm=TRUE)) %>%
  print()
barplot(BlackSpruce$Mean_Browsing,
        names.arg=BlackSpruce$Ecoregion,
        xlab="Ecoregion",
        ylab="Average Moose Browsing Score",
        main="Average Black Spruce Browsing Intensity by Ecoregion",
        col="cyan",
        cex.names=0.5)

#The Black Spruce has a significantly lower average than Balsam Fir, suggesting that Balsam Fir is more loved by moose.

#Question 18
sap_reg_tally<-sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

#Question 19
sap_spe_tally<-sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#Question 20

#No, the saplings are not evenly distributed, the number of saplings range from a min of 1, to a max of 11 
#indicating that some species are over presented while others are underrepresented
#Unequal sample sizes can bias the analysis and affect the accuracy of the comparisons between tree types

#PART 3 Joining Data Sets

#Question 21
moose_2020<-filter(moose_clean, Year==2020)
moose_sap<-left_join(moose_2020, sap_clean, by='Ecoregion', relationship="many-to-many")

#Question 22
sum_spe_browse<-moose_sap %>%
  group_by(Species,Ecoregion) %>%
  summarize(Avg_Browsing=mean(BrowsingScore, na.rm=TRUE),
            Avg_mooseDensity=mean(Estimated_Moose_Pop,na.rm = TRUE)) %>%
  print()

#Question 23

#Yes, the moose show strong preferences at both low and high densities. Moose are less selective in their browsing
#Moose like the willow the most because its the highest all throughout the Avg moose densities
#The black spruce is the least liked for most average moose densities
#Black ash is not on the graph possibly meaning that there is not enough data for moose browsing in that area

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens",
            "Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25
moose_coll<-moose_coll %>%
  rename_with(~"Ecoregion",study_sites)
coll_merge<-left_join(moose_coll, moose_2020, by = "Ecoregion", relationship = "many-to-many")

#Question 26
plot(coll_merge$Estimated_Moose_Pop, coll_merge$collisions2020,
     xlab="Moose Density (moose per sq km)",
     ylab="Number of Moose-Vehicle Collisions (2020)",
     main="Relationship Between Moose Density and Vehicle Collisions",
     pch=19,
     col="green")

#Directly proportional because the higher the moose density, the more collisions there are.
#At around 5000 moose density, there was more collisions.

#Question 27
coll_merge<-coll_merge %>%
  mutate(coll_per_capita=collisions2020/human_pop)
coll_merge %>%
  arrange(desc(coll_per_capita)) %>%
  print()

#Question 28
plot(coll_merge$human_pop,
     coll_merge$coll_per_capita,
     xlab="Human Population",
     ylab="Moose Collisions Per Capita (2020)",
     main="Moose-Vehicle Collisions Per Capita vs Human Population",
     pch=19,
     col="blue")

#Question 29
#The graph shows a low human population equals more collisions, which accounts for eachother
#Newfoundland because there is no more moose collisions in higher populated areas, but more in rural areas









