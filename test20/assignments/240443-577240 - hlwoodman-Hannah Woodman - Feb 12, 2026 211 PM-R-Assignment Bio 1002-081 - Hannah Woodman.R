#Question 1
install.packages("dplyr")
library(dplyr)
#Question 2
Moosedata <- read.csv("MoosePopulation.csv")
head(moosedata)
#Question 3
View(moosedata)
#Question 4
install.packages("dplyr") 
library(dplyr)
moosedata <- select(moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5a
oldest_year <- min(moosedata$Year)
oldest_year #the oldest observation in the data set is 1904
#Question 5b
max_population <- max(moosedata$Estimated_Moose_Pop) 
max_population #The highest moose population recorded is 41250
moosedata[moosedata$Estimated_Moose_Pop == max_population,] #Ecoregion for the maximum population is Central_Forests
#Question 6
moosedata <- mutate(moosedata, MooseDensity = Estimated_Moose_Pop / Area)
head(moosedata)
#Question 7
plot(moosedata$Year, moosedata$MooseDensity, )
     plot(moosedata$Year, moosedata$MooseDensity, 
          xlab = "year", 
          ylab = "Moose per sq km", 
          main = "Moose density in Newfoundland ecoregions over time")  
#Question 8a
MooseDataWest <- filter(moosedata, Ecoregion == "Western_Forests")     
head(MooseDataWest)
#Question 8b
plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
     type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests over time",
     col = "darkblue",
     lwd = 2) 
#Question 9a
mooseData_2020 <- filter(moosedata, Year == 2020)
#Question 9b
mooseData_2020_b <- filter(mooseData_2020, MooseDensity > 2.0)
#Question 9c
mooseData_2020_b <- arrange(mooseData_2020_b, desc(MooseDensity))
#Question 10
mooseData_final <- moosedata %>%
  filter(Year == 2020) %>%           
  filter(MooseDensity > 2.0) %>%   
  arrange(desc(MooseDensity)) %>% 
  print()        
#Question 11a
saplings <- read.csv("SaplingStudy.csv")
saplings <- read.csv(file.choose(saplings))
#Question 11b
saplings <- na.omit(saplings)
head(saplings)
#Question 12a
grouped_data <- Saplings %>%
  group_by(Ecoregion)
mean_browsing <- grouped_data %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))
print(mean_browsing)
#Most moose browsing: Northern_Peninsula_Forests (mean_BrowsingScore = 4.57)
#Least moose browsing: StraitOfBelleIsleBarrens (mean_BrowsingScore = 1)
mean_height <- saplings %>%
  #Question 13a
  group_by(Ecoregion) %>%                           
  summarize(mean_TreeHeight = mean(Height, na.rm = TRUE)) %>%  
  print() 
#Ecoregions with average heights less than 20 cm is considered severely browsed by moose.
severely_browsed <- mean_height %>%  
  #Question 13b
  filter(mean_TreeHeight < 20) %>% 
  print()
#Severely browsed ecoregions: Northern_Peninsula_Forests, Western_Forests.  
#Question 14a
mean_browsing_species <- saplings %>%
  group_by(Species) %>%                                     
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE)) %>%
  print() 
#Question 14b
#Species with the highest browsing score: Black_Ash (mean_BrowsingScore = 5)
#Species with the lowest browsing score: Black_Spruce (mean_BrowsingScore = 2.33)
#Question 15
BalsamFir <- saplings %>%
  filter(Species == "Balsam_Fir") %>%                   
  group_by(Ecoregion) %>%                                
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE)) %>%
  #Ecoregion with highest Balsam Fir browsing = North_Shore_Forests (mean_BrowsingScore = 4.5)
  #Ecoregion with lowest Balsam Fir browsing = Long_Range_Barrens (mean_BrowsingScore = 1) 
  #Question 16
  barplot(BalsamFir$mean_BrowsingScore,                 
          names.arg = BalsamFir$Ecoregion,             
          xlab = "Ecoregion",                          
          ylab = "Average Balsam Fir Browsing Score",  
          main = "Average Balsam Fir Browsing by Ecoregion",  
          col = "forestgreen",                         
          cex.names = 0.6)              
#Each bar represents an ecoregion, with the height showing the average browsing intensity for Balsam Fir.
#Question 17a
BlackSpruce <- sapling %>%
  filter(Species == "Black_Spruce") %>%                       
  group_by(Ecoregion) %>%                                     
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE)) %>%
  print() 
#Question 17b
barplot(BlackSpruce$mean_BrowsingScore,                       
        names.arg = BlackSpruce$Ecoregion,                    
        xlab = "Ecoregion",                                   
        ylab = "Average Black Spruce Browsing Score",          
        main = "Average Black Spruce Browsing by Ecoregion", 
        col = "darkgreen",                                    
        cex.names = 0.6)
#Question 17c
# Black Spruce browsing is generally lower than Balsam Fir across most ecoregions.
# Balsam Fir is more heavily browsed, particularly in Northern_Peninsula_Forests and Western_Forests.
#Question 18
EcoregionTally <- sapling %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#Question 19
SpeciesTally <- sapling %>%
  group_by(Species) %>%
  tally() %>%          
  print()
#Question 20a
# The SaplingStudy dataset is not evenly distributed.
# Some ecoregions, like North_Shore_Forests and Northern_Peninsula_Forests, are overrepresented,
# while Maritime_Barrens and StraitOfBelleIsleBarrens are underrepresented.
# Similarly, Balsam_Fir and Black_Spruce have more saplings counted, while Black_Ash is underrepresented.
#Question 20b
# Recognizing bias is important because uneven sampling can affect our interpretation of moose browsing patterns.
# Overrepresented species or ecoregions may make browsing appear more intense or widespread than it really is,
# while underrepresented areas or species might be overlooked.
#Question 21a
mooseData_2020 <-moosedata %>%
  filter(Year == 2020) %>%                                    
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
#Question 21b
MooseSaplingData <- left_join(mooseData_2020, sapling, by = "Ecoregion") %>%
  print()
# This joins the moose density data with sapling data for each ecoregion.
# The resulting dataset contains moose density and all sapling observations for 2020.
#Question 22
BrowsingBySpeciesDensity <- MooseSaplingData %>%
  group_by(Species, Ecoregion) %>%                                  
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE),
    mean_MooseDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print() 
# Observation: In ecoregions with higher moose density, Alder and BalsamFir tend to have higher browsing scores.
# White_Birch has lower browsing in ecoregions with low moose density.
#Question 23a
# Observation: The figure supports the hypothesis. Moose show strong preferences for certain species at low densities,
# such as Willow and Alder, and shift to more generalist browsing as moose density increases.
#Question 23b
# Observation: Moose favour Willow and Alder saplings the most, showing higher browsing scores.
# They browse Black_Spruce and White_Birch the least, with much lower browsing intensity.
#Question 23c
# Observation: Black_Ash is not shown on the figure because it was severely underrepresented in the dataset,
# with only one sapling counted, so its average browsing score could not be calculated.
#Question 24
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
StudySites <- c("North_Shore_Forests",
                "Northern_Peninsula_Forests",
                "Long_Range_Barrens",
                "Central_Forests",
                "Western_Forests",
                "EasternHyperOceanicBarrens",
                "Maritime_Barrens",
                "Avalon_Forests",
                "StraitOfBelleIsleBarrens")
MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)
print(MooseCollisions)
#Question 25a
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
moosedata_2020 <- filter(moosedata, Year == 2020)
#Question 25b
colnames(moosedata_2020)
colnames(MooseCollisions)
MooseCollisions <- rename(MooseCollisions, Ecoregion = StudySites)
#Question 25c
Moose2020_Joined <- left_join(moosedata_2020, MooseCollisions, by = "Ecoregion")
head(Moose2020_Joined)
#Question 26a
plot(Moose2020_Joined$MooseDensity, 
     Moose2020_Joined$Collisions_2020,
     xlab = "Moose-Density (moose per sq km)",
     ylab = "Moose-Vehicle Collisions in 2020",
     main = "Relationship between Moose-Density and Vehicle Collisions",
     pch = 19,       # solid points
     col = "darkred")
#Question 26b
# The scatterplot shows a positive trend: ecoregions with higher moose density 
# tend to have more moose-vehicle collisions. The Avalon_Forests region appears 
# as an outlier, with a very high number of collisions relative to its moose density.
#Question 27
Moose2020_Joined <- Moose2020_Joined %>%
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)
Moose2020_Joined %>%
  arrange(desc(CollisionsPerCapita)) %>%
  select(Ecoregion, CollisionsPerCapita) %>%
  print()
#Question 28
plot(Moose2020_Joined$HumanPopulation,
     Moose2020_Joined$CollisionsPerCapita,
     xlab = "Human Population",
     ylab = "Collisions per Person (CollisionsPerCapita)",
     main = "Relationship between Human Population and Collisions per Capita",
     pch = 19,         # solid points
     col = "purple")
#Question 29
# The scatterplot shows that ecoregions with smaller human populations tend to have higher collisions per person,
# while highly populated areas have lower collisions per capita. 
# This makes sense because in sparsely populated regions, even a moderate number of moose-vehicle collisions 
# results in a high per-person rate, whereas in densely populated areas, the same number of collisions 
# is spread over more people.