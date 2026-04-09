#Question 1
install.packages("dplyr")
library(dplyr)
#Question 2
Moosedata <- read.csv("~/Downloads/MoosePopulation.csv")
View(Moosedata)
#Question 3
Moosedata <- na.omit(Moosedata)
#Question 4
Moosedata <- select (Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(Moosedata)
#Question 5
min(Moosedata$Year) #1904
max(Moosedata$Estimated_Moose_Pop) #41250
#Question 6
Moosedata <- mutate (Moosedata, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7
plot(Moosedata$Year, Moosedata$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Question 8
MoosedataWest <- filter(Moosedata, Ecoregion == "Western_Forests")
View(MoosedataWest)
plot(MoosedataWest$Year, MoosedataWest$MooseDensity,
    xlab = "Year",
    ylab = "Moose per sq km",
    main = "Moose density in Western region over time",
    type = "l")
#Question 9
Moosedata_2020 <- filter(Moosedata, Year == 2020)
View(Moosedata_2020)
Moosedata_2020_A <- filter(Moosedata_2020, MooseDensity >=2.0)
Moosedata <- arrange(Moosedata, desc(MooseDensity))
#Question 10
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Question 11
Saplings <- read.csv("~/Downloads/SaplingStudy.csv")
View(Saplings)
Saplings <- na.omit(Saplings)
#Question 12
MooseBrowsing_byEcoregion <- Saplings %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
#Northern_Peninsula_Forests = most moose browsing, StraitOfBelleIsleBarrens = lowest moose browsing 
#Question 13
TreeHeight_byEcoregion <- Saplings %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE)) %>%
  print()
SeverelyBrowsed <- TreeHeight_byEcoregion %>%
  filter(Mean_Height < 20) %>%
  print()
#Northern_Peninsula_Forests and Western_Forests have average heights less than 20cm
#Question 14
Browsing_bySpecies <- Saplings %>%
  group_by(Species) %>%
  summarize(Mean_Browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
#The species with the highest mean browsing is black_Ash (5), the lowest mean browsing is Black_Spruce(2.33)
#Question 15
BalsamFir <- Saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
#Question 16
barplot(BalsamFir$Mean_Browsing,
        names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Moose Browsing Score",
        main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        col = "green",
        cex.names = 0.6)  
#Question 17
BlackSpruce <- Saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
barplot(BlackSpruce$Mean_Browsing,
        names.arg = BlackSpruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Moose Browsing Score",
        main = "Average Black Spruce Browsing Intensity by Ecoregion",
        col = "red",
        cex.names = 0.6)  # Shrinks x-axis labels for readability
#The Black_Spruce browsing is typically much lower than that of Balsam_Fir. This suggests the Balsam_Fir is generally favoured by the moose more. 

#Question 18
EcoregionTally <- Saplings %>%   
  group_by(Ecoregion) %>%        
  tally() %>%                    
  print() 

#Question 19
sap_spe_tally <- Saplings %>%
  group_by(Species) %>%
  tally() %>%
  print() 
  
  
#Question20 - No, the saplings are not evenly distributed as the highest value of saplings is 8, whilst the lowest is 1. suggestions some are overpresented and some are underpresented.
#It's important to recognize these biases as it can alter the understandment of the data. As you're comparing data, but some trees have way less samples than others, so you're not getting an acurate reading. 
 
#Question 21
Moosedata_2020 <- filter(Moosedata, Year == 2020)
View(Moosedata_2020)
MoosedataSaplings <- left_join(Moosedata, Saplings, by = 'Ecoregion', relationship = "many-to-many")

#question 22 
BrowsingBySpeciesDensity <- MoosedataSaplings %>%
  group_by(Species, Ecoregion) %>% 
  summarize(Avg_Browsing = mean(BrowsingScore, na.rm = TRUE),   
    Avg_MooseDensity = mean(MooseDensity, na.rm = TRUE)) %>%
  print()  

#Question23 
install.packages("ggplot2")
library(ggplot2)
ggplot(BrowsingBySpeciesDensity, aes(x = Avg_MooseDensity, y = Avg_Browsing, color= Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#Yes, at low density moose are more selective browsing, and at higher desnity moose are less selective in their browsing.
#Moose prefer Willow the most, and their least prefered is black spruce
#Black ash was not shown as there was little to know data for moose browsing in that particular area

#Question 24
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
StudySites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)

#Question 25
MooseCollisions <- MooseCollisions %>%
  rename_with(~"Ecoregion", StudySites)
MooseCollisions_2020 <- left_join(MooseCollisions, Moosedata_2020, by = "Ecoregion")
View(MooseCollisions_2020)

#Question 26
plot(MooseCollisions_2020$MooseDensity,
MooseCollisions_2020$Collisions_2020,
xlab = "Moose Density (moose per sq km)",
ylab = "Number of Moose-Vehicle Collisions (2020)",
main = "Relationship Between Moose Density and Vehicle Collisions",
pch = 19,
col = "orange")
#The trend is generally proportionsal (as one increases the other does). Except the number of collisions at 1.0 moose density is unusually high

#Question 27
MooseCollisions_2020 <- MooseCollisions_2020 %>%
mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)
MooseCollisions_2020 %>%
  arrange(desc(CollisionsPerCapita)) %>%
  print()

#Questions28
plot(MooseCollisions_2020$HumanPopulation,
     MooseCollisions_2020$CollisionsPerCapita,
     xlab = "Human Population",
     ylab = "Moose Collisions Per Capita (2020)",
     main = "Moose-Vehicle Collisions Per Capita vs Human Population",
     pch = 19,
     col = "Blue")
#Question29
#The noticeable trend is the lower the population, the higher the collisions. Which makes perfect sense for Newfoundland as moose typically do not hang around the highly populated city areas. Typically they are seen far more often out around the bay in smaller communities.

