#Title: Bio 1002 R Assignment
#Author: Jack Wellon
#Date: 15/01/2026

#IMPORTANT NOTE: I apologize as all the code for this R assignment was done before the naming change where you have to save the results under specific dataset names.

#Set Working Directory
setwd("C:/Users/jackw/OneDrive/Documents/Bio 1002 R")

#Install Necessary Packages & Load Libraries
install.packages("dplyr")
library(dplyr)

#Loading Moose Data Via Internet
Moose_Data<-read.csv((file="https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv"))

#Omit Missing Values
MooseData<- na.omit(Moose_Data)

#Simplify Dataset
MooseData <- select(MooseData,
                    Ecoregion,
                    Year,
                    Area,
                    Estimated_Moose_Pop)

#Find Oldest Observation
min(MooseData$Year,
    na.rm = FALSE)
#1904 was the oldest observation

#Find Highest Estimated Moose Population
max(MooseData$Estimated_Moose_Pop,
    na.rm = TRUE)
#Highest Estimated population was: 41250 for just one Ecoregion

#Standardize Data
MooseData <-mutate(MooseData,
                   MooseDensity = Estimated_Moose_Pop / Area)

#Plotting Data
plot(MooseData$Year, MooseData$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose Per Square Kilometer",
     type = "l",
     lwd = "2",
     col = "orange",
     main = "Moose Density in Newfoundland Ecoregions Over Time")

#Plotting Western Forest Region Data 
MooseDataWest <- filter(MooseData, Ecoregion == "Western_Forests")
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose Per Square Kilometer",
     type = "l",
     lwd = "2",
     col = "red",
     main = "Moose Density in Newfoundland Western Forest Region Over Time")

#Filter Moose Data for 2020

MooseData2020 <- filter(MooseData, Year == 2020)

MooseData2020b <- filter(MooseData2020, MooseDensity >= 2.0)

#Arrange Data in Descending Order
arrange(MooseData2020b, desc(MooseDensity))

#Connect Data With Pipes
MooseData_final <- MooseData %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Loading Sapling Data Via the Internet
Sapling_Data <-read.csv(file="https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
SaplingData <- na.omit(Sapling_Data)

#Create Dataset for Moose Browsing Pressure Across Different Ecoregions
MooseBrowsing <- SaplingData %>%
  group_by(Ecoregion)%>%
  summarize( mean(BrowsingScore)) %>%
  print()
#Which ecoregion has the most moose browsing. Which ecoregion has the lowest?
#The Northern_Peninsula_Forests have the most moose browsing and the StraitOfBelleIsleBarrens has the least

#Create Dataset For Varying Tree Height (and set average height to later filter)
TreeHeight <- SaplingData %>%
  group_by(Ecoregion) %>%
  summarize( AverageTreeHeight = mean(Height))%>%
  print()

#Filter to Find Average Heights
AverageTreeHeight <- TreeHeight %>%
  filter(AverageTreeHeight < 20) %>%
  print()
 
#Western_Forests and Northern_Peninsula_Forests have average heights less than 20

#Create Average Browsing Score Across Saplings Dataset
SaplingAverage <- SaplingData %>%
  group_by(Species)%>%
  summarize( mean(BrowsingScore)) %>%
  print()
#Black_Ash has the highest browsing score and Black_Spruce has the lowest

#Create Balsam Fir Browsing Dataset
BalsamFirData <- SaplingData %>%
  filter(Species == "Balsam_Fir" )%>%
  group_by(Ecoregion) %>%
  summarize( BrowsingAverage = mean(BrowsingScore)) %>%
  print()

#Create Barplot of Balsam Fir Dataset
barplot(BalsamFirData$BrowsingAverage,
        names.arg = BalsamFirData$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Browsing Intensity on Balsam Fir per Ecoregion",
        col = "pink",
        cex.names = .4,
        )
#Made the width of names a bit smaller than .6 so all labels could appear

#Create Black Spruce Browsing Dataset
BlackSpruceData <- SaplingData %>%
  filter(Species == "Black_Spruce" )%>%
  group_by(Ecoregion) %>%
  summarize( BrowsingAverage = mean(BrowsingScore)) %>%
  print()

#Create Barplot of Black Spruce Dataset
barplot(BlackSpruceData$BrowsingAverage,
        names.arg = BlackSpruceData$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Browsing Intensity on Black Spruce per Ecoregion",
        col = "orange",
        cex.names = .32,
        )
#Overall, the Balsam Fir tree generally experiences the most browsing from moose across every ecoregion (Balsam Fir has browsing in every region while Black Spruce does not).
#Both regions have the highest browsing in the northern regions.

#Determine how many trees were counted in each ecoregion.
EcoregionTally<- SaplingData %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#The same number of tree saplings were not counted in each ecoregion

#Were the same number of tree saplings counted for each species? 
SpeciesTally<- SaplingData %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#No, the number varies between species.

#Overall, the SaplingStudy dataset is not evenly distributed it underepresents the Black_Ash tree species and overepresents the Balsalm_Fir species.
#It is important to recognize bias in ecological datasets so you dont misrepresent and give inaccurate scientific data. In this case with the trees, because of the sampling bias some tree species are overepresented leading to the ecological patterns being wrong/inaccurate. 

#Create Dataset for the Moose in 2020
MooseData_2020 <- MooseData %>%
  filter (Year == 2020)%>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area )

#Group Moose and Sapling Datasets Together by Ecoregion
MooseSaplingData <- left_join(MooseData_2020,
                              SaplingData,
                              by = 'Ecoregion',
                              relationship = "many-to-many")

#Calculate  Average Browsing Score and  Moose Density
BrowsingBySpeciesDensity <- MooseSaplingData %>%
  group_by(Species, Ecoregion)%>%
  summarize((mean(BrowsingScore)), mean(MooseDensity)) %>%
  print(n= 38)

#Exploring the Browsing Intensity Across Moose Density by Species:
#A.Is there evidence that supports the researchers’ hypothesis? Do moose show strong preferences at low density and shift to more generalist browsing at higher density?
#A-Yes, the evidence somewhat supports the researchers hypothesis. As some specific tree types are visited less frequently (and some very highly) with less moose density (showing preference) and they're all browsed around the same frequency at high moose density (shifting to more generalist)
#B.Which sapling specie(s) do moose favour the most? Which do they browse the least?
#B- The moose favour the Willow sapling the most and they browse the Black Spruce the least. This is reflected in low density (more preference) where they browse the Willow the most and the Black Spruce the least.
#C.Which sapling species is not shown on the figure and why?
#C The Black Ash sapling species is not shown in the figure likely due to none  being recorded in the SaplingData dataset.
  
#Create Moose Collisions Dataset
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
StudySites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)

#Join Moose Collisions Dataset with the moose data from 2020
MooseCollisions2020<-left_join(MooseData2020, MooseCollisions, by = 'Ecoregion', relationship = "many-to-many")
#Does not work because the "MooseData2020" dataset uses Ecoregions for their column while the "MooseCollisions" dataset uses Studysites for their column.

#Fixing MooseCollisions Dataset To Match Column Names
MooseCollisions <- MooseCollisions %>%
  rename_with(~"Ecoregion", StudySites)
#Changed the "=" from given template as it was causing problems.

#Join Moose and Fixed Collision Datasets
MooseCollisions2020<-left_join(MooseData2020,
                               MooseCollisions,
                               by = 'Ecoregion',
                               relationship = "many-to-many")
#Plot MooseCollisions2020 Dataset
plot(
  MooseCollisions2020$MooseDensity,
  MooseCollisions2020$Collisions_2020
)
#The general trend/relation is that as the moose density increases, collisions increase.
#In the 1.0 value of moose density there is a big outlier which has two data points with one being extremely higher than the other points.

#Create New Collisions Per Ecoregion Column
MooseCollisions2020 <- mutate(MooseCollisions2020,
                              CollisionsPerCapita = Collisions_2020/HumanPopulation)
#Which ecoregions have the highest number of moose collisions per person? 
#Northern_Peninsula_Forests has the highest, then the Long_Range_Barrens and the North_Shore_Forests.

#Plot the CollisionsPerCapita vs Human Population

plot(MooseCollisions2020$HumanPopulation,
     MooseCollisions2020$CollisionsPerCapita
     )
#The general trend I see is that collisions with moose per capita decrease with increasing population. This makes sense for Newfoundland as most moose live far away from populated areas so more human population = less moose = less collisions.