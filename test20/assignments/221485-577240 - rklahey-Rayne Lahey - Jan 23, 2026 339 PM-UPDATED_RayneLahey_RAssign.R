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
Moose_Clean <- na.omit(MooseData)
#Q4:
Moose_Sel <- select(Moose_Clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Q5:
#a: 
Year_Min <- min(Moose_Sel$Year)
#1904
#b:
Moose_Max <- max(Moose_Sel$Year)
#2020
#Q6:
MooseData2 <- mutate(Moose_Sel, MooseDensity = Estimated_Moose_Pop / Area)
#Q7:
plot(MooseData2$Year, MooseData2$MooseDensity, type = "l", xlab = "Year", ylab = "Moose Per sq Km", main = "Moose Density in Newfoundland Ecoregions Between 1904 and 2020")
#Q8:
Moose_West <- filter(MooseData2, Ecoregion == "Western_Forests")
plot(Moose_West$Year, Moose_West$MooseDensity, type = "l", xlab = "Year", ylab = "Moose Per sq Km", main = "Moose Density in Newfoundland Western Forest Ecoregions Between 1904 and 2020")
#Q9: 
Moose_2020 <- filter(MooseData2, Year == "2020")
Moose_2020_High <- filter(Moose_2020, MooseDensity > 2.0)
Moose_2020_High_byD <- arrange(Moose_2020_High, desc(MooseDensity))
#Q10:
MooseFinal <- MooseData2 %>% filter(Year == 2020) %>% filter(MooseDensity >2.0) %>% arrange(desc(MooseDensity)) %>% print()

#Part II:
#Q11:
Saplings <- read.csv("C:/Users/Rayne/OneDrive/Desktop/1002RASS/SaplingData.csv")
Sap_Clean <- na.omit(Saplings)
#Q12: 
#a:
Sap_Reg_Browse <- Sap_Clean %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore)) %>% print()
#b:
Avg_Browse_reg <- arrange(Sap_Reg_Browse, desc(mean(BrowsingScore)))
#Northern Peninsula Forests is the highest, the lowest is the Straight of Belle Isle Barrens.
#Q13: 
#a:
Sap_Reg_Height <- Sap_Clean %>% group_by(Ecoregion) %>% summarize(mean(Height)) %>% print()
#b:
#The Western, Central, North Shore, and Northern Peninsula forests all had heights lower than 20cm. 
Sap_Reg_Height_Low <- filter(Sap_Reg_Height, Height < 20.0)
Sap_Reg_Height_Low <- print(Sap_Reg_Height_Low)
#Q14:
#a:
Sap_Spe_Browse <- Sap_Clean %>% group_by(Species) %>% summarize(mean(BrowsingScore)) %>% print()
#b: 
Avg_Browse_Spe <- arrange(Sap_Spe_Browse, desc(mean(BrowsingScore)))
#Black Ash is the highest, the lowest is Black Spruce. 
#Q15:
Fir_Reg_Browse <- Sap_Clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore)) %>% print()
#Q16:
barplot(height = Fir_Reg_Browse$`mean(BrowsingScore)`, names.arg = Fir_Reg_Browse$Ecoregion,  xlab = "Ecoregion", ylab = "Mean Browsing Intensity", main = "Browsing Intensity of Balsam Firs by Ecoregion", col = "forestgreen", cex.names = 0.6)
#Q17:
Spruce_Reg_Browse <- Sap_Clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore)) %>% print()
barplot(height = Spruce_Reg_Browse$`mean(BrowsingScore)`, names.arg = Spruce_Reg_Browse$Ecoregion,  xlab = "Ecoregion", ylab = "Mean Browsing Intensity", main = "Browsing Intensity of Black Spruce Trees by Ecoregion", col = "purple", cex.names = 0.6)
#c: 
#The Balsam Fir and Black Spruce browsing intensity is very comparable in both the North Shore forests, and the Norther Peninsula forests; the Balsam Fir averages a browsing intensity of 4.3 for these regions, while the Black Spruce averages an intensity of 4 for these regions.  
#There is data stating a mean browsing intensity of 3.5 for Black Spruce in the Western forests, however there is no Balsam Fir data for this region, which indicates that there is likely no cases of Balsam Fir trees in the Western forest region. 
#There is a mean browsing intensity of 0 for the Black Spruce in both the Eastern hyper ocean barrens, as well as the Maritime barrens, but this is not the case for the Balsam Fir, which demonstrates a mean moose browsing intensity of 2 for both the Eastern hyper ocean barrens and the Maritime barrens. 
#The Black Spruce demonstrates an average mean browsing intensity of 0.6 for the three barren ecoregions, while the Balsam Fir has an average mean browsing intensity of 1.7 for the three barren ecoregions. 
#The mean browsing intensity is also comparable for both species of tree in the Central forest ecoregion; the Black Spruce displays a mean intensity of 3, while the Balsam Fir demonstrates a mean intensity of 3.5. 
#Q18:
Sap_Reg_Tally <- Sap_Clean %>% group_by(Ecoregion) %>% tally() %>% print()
#No the same number of saplings were not counted in each ecoregion. 
#Q19:
Sap_Spe_Tally <- Sap_Clean %>% group_by(Species) %>% tally() %>% print()
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
#a: 
Moose_2020b <- filter(Moose_Clean, Year == "2020")
Moose_2020b <- mutate(Moose_2020b, MooseDensity = Estimated_Moose_Pop / Area)
#b:
Moose_Sap <- left_join(Moose_2020b, Sap_Clean, by = 'Ecoregion', relationship = "many-to-many")
#Q22:
Summary_Spe_Browse <- Moose_Sap %>% group_by(Ecoregion, Species) %>%  summarize(meanBrowsingScore = mean(BrowsingScore, na.rm = TRUE), meanMooseDensity  = mean(MooseDensity, na.rm = TRUE), .groups = "drop") %>% print(Summary_Spe_Browse)
#Q23: 
#a: 
#No, I do not think that there is evidence to support the hypothesis. It seems to me that the moose show a distinct preference for willow trees across the board, with an even stronger preference at higher density. 
#b: 
#The moose favour willow trees consistently, and they seem to dislike black spruce trees. 
#c: 
#Black ash is not shown in the figure, likely because only one tree of that species was counted, and it was only observed in one single ecoregion. 
#Q24:
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
Human_Pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
Study_Sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraightOfBelleIsleBarrens")
Moose_Collisions <- data.frame(Study_Sites, Human_Pop, Collisions_2020)
#Q25:
#a:
Moose_Collisions2 <- Moose_Collisions %>% rename(Ecoregion = StudySites)
#b:
Collisions_Merge <- left_join(Moose_2020, Moose_Collisions2, by = 'Ecoregion', relationship = "many-to-many")
#Q26:
#a: 
plot(Collisions_Merge$Collisions_2020, Collisions_Merge$MooseDensity, xlab = "Number of Collisions in 2020", ylab = "Density of Moose", main = "Moose Density in Relation to Moose Collisions in 2020")
#b: 
#Fairly consistently higher moose density is positively correlated to more collisions. However, there is an outlier; at the moose density of 0.5, there is more then 100 collisions, despite higher moose density areas maxing at just over 60 collisions.
#Q27: 
Collisions_Merge_Per_Capita <- mutate(Collisions_Merge, Collisions_Per_Capita = Collisions_2020 / HumanPopulation)
#Q28:
plot(Collisions_Merge_Per_Capita$Collisions_Per_Capita, Collisions_Merge_Per_Capita$HumanPopulation, xlab = "Collisions Per Capita", ylab = "Human Population", main = "Collisions Per Capita VS Human Population")
#Q29:
#In general, the human population remains under 50000, despite the collisions per capita ranging from less than 0.001, all the way to 0.005. 
#There is an outlier, where the human population is over 250000 and the collisions per capita is less than 0.001, this is likely the St John's region, as the human population is quite large in this area.
#Because St John's is also such an urbanized areas, especially in comparison to rural Newfoundland, it makes sense that the amount of moose collisions would be lower. 
#Also, given the high human population, there would need to be a very large amount of collisions for the collisions per capita to increase even to the 0.002 area. 
