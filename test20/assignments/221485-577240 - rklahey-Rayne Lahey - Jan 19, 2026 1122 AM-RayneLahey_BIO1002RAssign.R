#Title: R Assignment 1002
#Author: Rayne Lahey 
#Date: 2026-01-10

#Part I:
#Q1:
install.packages("dplyr")
library("dplyr")
#Q2:
MooseData <- read.csv("C:/Users/Rayne/OneDrive/Desktop/1002RASS/MooseData.csv")
View(MooseData)
#Q3:
MooseData <- na.omit(MooseData)
#Q4:
MooseData <- select(MooseData, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Q5:
#a: 
Min_Year <- min(MooseData$Year)
#1904
#b:
Max_Year <- max(MooseData$Year)
#2020
#Q6:
MooseData <- mutate(MooseData, MooseDensity = Estimated_Moose_Pop / Area)
#Q7:
plot(MooseData$Year, MooseData$MooseDensity, type = "l", xlab = "Year", ylab = "Moose Per sq Km", main = "Moose Density in Newfoundland Ecoregions Between 1904 and 2020")
#Q8:
MooseDataWest <- filter(MooseData, Ecoregion == "Western_Forests")
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, type = "l", xlab = "Year", ylab = "Moose Per sq Km", main = "Moose Density in Newfoundland Western Forest Ecoregions Between 1904 and 2020")
#Q9: 
MooseData_2020 <- filter(MooseData, Year == "2020")
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)
MooseData_2020_b <- arrange(MooseData_2020_b, desc(MooseDensity))
#Q10:
MooseData_Final <- MooseData %>% filter(Year == 2020) %>% filter(MooseDensity >2.0) %>% arrange(desc(MooseDensity)) %>% print()

#Part II:
#Q11:
SaplingData <- read.csv("C:/Users/Rayne/OneDrive/Desktop/1002RASS/SaplingData.csv")
SaplingData <- na.omit(SaplingData)
#Q12: 
#a:
SaplingData_Pressure <- SaplingData %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore)) %>% print()
#b:Northern Peninsula Forests
#Q13: 
SaplingData_TreeHeight <- SaplingData %>% group_by(Ecoregion) %>% summarize(mean(Height)) %>% print()
SaplingData_TreeHeight <- filter(SaplingData, Height < 20.0)
SaplingData_TreeHeight <- print(SaplingData_TreeHeight)
#Q14:
SaplingData_Browsing <- SaplingData %>% group_by(Species) %>% summarize(mean(BrowsingScore)) %>% print()
#b: Black Ash 
#Q15:
SaplingData_BalsamFir <- SaplingData %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore)) %>% print()
#Q16:
barplot(height = SaplingData_BalsamFir$`mean(BrowsingScore)`, names.arg = SaplingData_BalsamFir$Ecoregion,  xlab = "Ecoregion", ylab = "Mean Browsing Intensity", main = "Browsing Intensity of Balsam Firs by Ecoregion", col = "forestgreen", cex.names = 0.6)
#Q17:
Black_SpruceData <- SaplingData %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore)) %>% print()
barplot(height = Black_SpruceData$`mean(BrowsingScore)`, names.arg = Black_SpruceData$Ecoregion,  xlab = "Ecoregion", ylab = "Mean Browsing Intensity", main = "Browsing Intensity of Black Spruce Trees by Ecoregion", col = "purple", cex.names = 0.6)
#c: 
#The Balsam Fir and Black Spruce browsing intensity is very comparable in both the North Shore forests, and the Norther Peninsula forests; the Balsam Fir averages a browsing intensity of 4.3 for these regions, while the Black Spruce averages an intensity of 4 for these regions.  
#There is data stating a mean browsing intensity of 3.5 for Black Spruce in the Western forests, however there is no Balsam Fir data for this region, which indicates that there is likely no cases of Balsam Fir trees in the Western forest region. 
#There is a mean browsing intensity of 0 for the Black Spruce in both the Eastern hyper ocean barrens, as well as the Maritime barrens, but this is not the case for the Balsam Fir, which demonstrates a mean moose browsing intensity of 2 for both the Eastern hyper ocean barrens and the Maritime barrens. 
#The Black Spruce demonstrates an average mean browsing intensity of 0.6 for the three barren ecoregions, while the Balsam Fir has an average mean browsing intensity of 1.7 for the three barren ecoregions. 
#The mean browsing intensity is also comparable for both species of tree in the Central forest ecoregion; the Black Spruce displays a mean intensity of 3, while the Balsam Fir demonstrates a mean intensity of 3.5. 
#Q18:
EcoregionTally <- SaplingData %>% group_by(Ecoregion) %>% tally() %>% print()
#No the same number of saplings were not counted in each ecoregion. 
#Q19:
SpeciesTally <- SaplingData %>% group_by(Species) %>% tally() %>% print()
#No the same number of saplings were not counted for each species.
#Q20: 
#a: 
#The Black Ash tree is very underrepresented, as only 1 tree of this species was counted across all of the data. 
#Conversely, the Balsam Fir is over represented in comparison to the other species, as 11 trees of this species were counted across all of the data.
#This inequality of data is also represented in the amount of saplings counted per ecoregion; the number of saplings counted was 5 for the Avalon forests, the Central forests, the Eastern hyper ocean barrens, the Western Forests and the Long range barrens, however, only one sapling was counted in the Straight of Belle Isle barrens ecoregion, while the North Shore Forests and the Northern Peninsula forests had 8 and 7 saplings counted respectively. 
#I do not think the sapling study data set is evenly distributed. 
#b: 
#It is important to recognize under and over representation in ecological data, because if the data is not evenly distributed, it is difficult to obtain a clear picture of the actual situation. 
#For example, in this data set the representation of mean browsing intensity is not entirely accurate, because the sample size for each ecoregion is both very small, and also not equal for each ecoregion or species.
#A single tree cannot accurately represent the mean browsing intensity for a given species in a given ecoregion. 

#Part III: 
#Q21: 
MooseSaplingData <- left_join(MooseData_2020, SaplingData, by = 'Ecoregion', relationship = "many-to-many")
#Q22:
BrowsingBySpeciesDensity <- MooseSaplingData %>% group_by(Ecoregion, Species) %>%  summarize(meanBrowsingScore = mean(BrowsingScore, na.rm = TRUE), meanMooseDensity  = mean(MooseDensity, na.rm = TRUE), .groups = "drop") %>% print(BrowsingBySpeciesDensity)
#Q23: 
#a: 
#No, I do not think that there is evidence to support the hypothesis. It seems to me that the moose show a distinct preference for willow trees across the board, with an even stronger preference at higher density. 
#b: 
#The moose favour willow trees consistently, and they seem to dislike black spruce trees. 
#c: 
#Black ash is not shown in the figure, likely because only one tree of that species was counted, and it was only observed in one single ecoregion. 
#Q24:
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
StudySites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraightOfBelleIsleBarrens")
MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)
#Q25:
#a: Does not work, because the column "Ecoregion" in the MooseData_2020 dataset, is called "StudySites" in the MooseCollisions dataset.
#b: 
MooseCollisions <- MooseCollisions %>% rename(Ecoregion = StudySites)
#c:
MooseCollisions_2020 <- left_join(MooseData_2020, MooseCollisions, by = 'Ecoregion', relationship = "many-to-many")
#Q26:
#a: 
plot(MooseCollisions_2020$Collisions_2020, MooseCollisions_2020$MooseDensity, xlab = "Number of Collisions in 2020", ylab = "Density of Moose", main = "Moose Density in Relation to Moose Collisions in 2020")
#b: 
#Fairly consistently higher moose density is positively correlated to more collisions. However, there is an outlier; at the moose density of 0.5, there is more then 100 collisions, despite higher moose density areas maxing at just over 60 collisions.
#Q27: 
MooseCollisions_2020 <- mutate(MooseCollisions_2020, CollisionsPerCapita = Collisions_2020 / HumanPopulation)
#Q28:
plot(MooseCollisions_2020$CollisionsPerCapita, MooseCollisions_2020$HumanPopulation, xlab = "Collisions Per Capita", ylab = "Human Population", main = "Collisions Per Capita VS Human Population")
#Q29:
#In general, the human population remains under 50000, despite the collisions per capita ranging from less than 0.001, all the way to 0.005. 
#There is an outlier, where the human population is over 250000 and the collisions per capita is less than 0.001, this is likely the St John's region, as the human population is quite large in this area.
#Because St John's is also such an urbanized areas, especially in comparison to rural Newfoundland, it makes sense that the amount of moose collisions would be lower. 
#Also, given the high human population, there would need to be a very large amount of collisions for the collisions per capita to increase even to the 0.002 area. 
