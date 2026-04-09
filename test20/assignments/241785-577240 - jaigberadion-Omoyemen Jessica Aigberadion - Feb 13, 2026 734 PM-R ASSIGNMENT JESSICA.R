# Author: Aigberadion Omoyemen Jessica
# Date: 13-02-2026
# Student ID: 202215497
# Course: Biology 1002
# Instructors: Dr Yolandaa Wiersma,Dr Tom chapman


# load libraries needed --------------------------
library(dplyr)

# Set working directory -------------------------- 
setwd("~/OMOYEMEN ASSIGNMENT 1/")

# Loading Relevant data -------------------------
Moosedata <- read.csv("MoosePopulation.csv")
Saplingdata <- read.csv("SaplingStudy.csv")


# PART 1-----------------------------------------


# question 3
Moosedata <- na.omit(Moosedata)

# question 4
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)

# question 5a
min(Moosedata$Year) # 1904

# question 5b
max(Moosedata$Estimated_Moose_Pop) # 41250, this is for the Central_Forests ecoregion

# question 6
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)

# question 7
plot(Moosedata$Year, Moosedata$MooseDensity,
     type = "l",
     xlab =
       "year"
     ,
     ylab =
       "Moose per sq km"
     ,
     main =
       "Moose density in Newfoundland ecoregions over time")

# question 8a
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")

# question 8b
plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
     type = "l",
     xlab =
       "year"
     ,
     ylab =
       "Moose per sq km"
     ,
     main =
       "Moose density in the Western_Forest ecoregion over time")

# question 9a
MooseData_2020 <- filter(Moosedata, Year == "2020")

# question 9b
MooseData_2020_b <- filter(Moosedata, MooseDensity > "2.0")

# question 9c
arrange(MooseData_2020_b, desc(MooseDensity))

# question 10
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


# PART 2 ----------------------------------------


# question 11b
Saplingdata <- na.omit(Saplingdata)

# question 12a
SaplingData_AVG_Browsing <- Saplingdata %>%
  group_by(Ecoregion) %>%
  summarize(AVG_moose_browsing = mean(BrowsingScore)) %>%
  print() # The Ecoregion with the highest average moose browsing is Northern_Peninsula_Forests with 4.57 
          # and the least is StraitOfBelleIsleBarrens with 1 

# question 13a
SaplingData_AVG_TreeHeight <- Saplingdata %>%
  group_by(Ecoregion) %>%
  summarize(AVG_TreeHeight = mean(Height)) %>%
  print() # The Ecoregion with the highest average Tree height is Avalon_Forests with 32.4 
          # and the least is Western_Forests with 18.9 
# question 13b
# By observing the data the Ecoregion which are severely browsed are:
# Northern_Peninsula_Forests and Western_Forests

# question 14a

SaplingData_AVG_Browsing_by_SaplingSpecies <- Saplingdata %>%
  group_by(Species) %>%
  summarize(AVG_moose_browsing_by_SaplingSpecies = mean(BrowsingScore)) %>%
  print() # The Species with the highest average moose browsing is Black_Ash with 5 
          # and the least is Black_Spruce with 2.33 

# question 15
SaplingData_AVG_Browsing_Balsam_Fir <- Saplingdata %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AVG_moose_browsing_Balsam_Fir = mean(BrowsingScore)) %>%
  print()

# question 16
barplot(SaplingData_AVG_Browsing_Balsam_Fir$AVG_moose_browsing_Balsam_Fir,
        names.arg = SaplingData_AVG_Browsing_Balsam_Fir$Ecoregion, 
        xlab = "Ecoregion",
        ylab = "AVG moose browsing", 
        main = "AVG moose browsing on balsam fir by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

# question 17a
SaplingData_AVG_Browsing_Black_Spruce <- Saplingdata %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AVG_moose_browsing_Black_Spruce = mean(BrowsingScore)) %>%
  print()

# question 17b
barplot(SaplingData_AVG_Browsing_Black_Spruce$AVG_moose_browsing_Black_Spruce,
        names.arg = SaplingData_AVG_Browsing_Black_Spruce$Ecoregion, 
        xlab = "Ecoregion",
        ylab = "AVG moose browsing", 
        main = "AVG moose browsing on black spruce by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

# question 17c
# Black spruce browsing is consistently lower than balsam fir browsing across all ecoregions. 
# The highest browsing intensity for both species occurs in Northern_Peninsula_Forests and North_Shore_Forests.

# question 18
EcoregionTally<- Saplingdata %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print() # No the number of saplings counted per region are not the same

# question 19
EcoregionTally<- Saplingdata %>%
  group_by(Species) %>%
  tally() %>%
  print() # No the number of saplings counted per species are not the same

# question 20a
# the saplingstudy dataset is not evenly distributed, certain ecoregions e.g North_shore_forests are over represented compared to others
# while some ecoregions and tree species have fewer observations suggesting potential sampling imbalance

# question 20b
# it is important to recongnize bias in ecological dataset because uneven sampling can distort patterns and lead to incorrect conclusions about moose browsing preferences or habitat use


# PART 3 -----------------------------------------------------

# question 21a
MooseData_2020 <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area ) %>%
  print()
  
# question 21b
  MooseSaplingData <- left_join(Moosedata, Saplingdata, by = 'Ecoregion', relationship = "many-to-many")
 
# question 22
  BrowsingBySpeciesDensity <- MooseSaplingData %>%
    group_by(Species, Ecoregion) %>%
    summarize(
      averagebrowsing = mean(BrowsingScore),
      averagemoosedensity = mean(MooseDensity)
    ) %>%
    print()
  
  # question 23a
  # yes.Moose are selective at low density but browse more species at a high density
  
  #  question 23b
  # moose favor willow most and browse Black spruce the least
  
  # question 23c
  # the missing species likely had no browsing data, so it does not appear
  
  # question 24
  Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
  HumanPopulation <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
  StudySites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
  
  MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)
  
  # question 25a
  # we cannot join because the column names do not match,i.e StudySites and Ecoregion
  
  # question 25b
  MooseCollisions <- MooseCollisions %>% 
    rename(Ecoregion = StudySites)
   
  # question 25c
  MooseCollisionData <- left_join(Moosedata, MooseCollisions, by = 'Ecoregion', relationship = "many-to-many")
  
  # question 26a
  plot(MooseCollisionData$MooseDensity, MooseCollisionData$Collisions_2020,
       xlab =
         "Moose Density"
       ,
       ylab =
         "Collisions"
       ,
       main =
         "Moose density vs Moose Collisions")
  
  # question 26b
  # There is no overall strong correlation between Moose Density and Collision 
  # Some outliners exist at the top of the plot
  
  # question 27
  MooseCollisionData <- mutate(MooseCollisionData, CollisionsPerCapita = Collisions_2020 / HumanPopulation)
  
  # question 28
  plot(MooseCollisionData$HumanPopulation, MooseCollisionData$CollisionsPerCapita,
       xlab =
         "Human Population"
       ,
       ylab =
         "CollisionsPerCapita"
       ,
       main =
         "CollisionsPerCapita vs HumanPopulation")
  
  # question 29
  # The collisionPerCapita decreases as human population increases.
  # This makes sense because moose are less likely to be around heavily populated areas  