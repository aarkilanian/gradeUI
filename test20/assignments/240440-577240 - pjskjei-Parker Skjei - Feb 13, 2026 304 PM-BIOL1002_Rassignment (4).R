install.packages("dplyr")
library("dplyr") 
Moosedata <- read.csv("MoosePopulation.csv") 
library(dplyr)
moose_clean <- moose_raw %>% na.omit()
moose_sel <- moose_clean %>%
  select(Ecoregion, Year, Area, Estimated_Moose_Pop)
str(moose_sel)
library(dplyr)
library(tidyr)
library(dplyr)
library(tidyr)
moose_sel <- moose_raw %>%
  drop_na() %>%
  select(Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel
library(dplyr)
library(tidyr)
install.packages("tidyr")
library(tidyr)
moose_sel <- moose_raw %>%
  drop_na() %>%
  select(Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel
dim(moose_sel)
min(moose_sel$Year)
max(moose_sel$Estimated_Moose_Pop)
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area) 
Moosedata <- moose_sel
Moosedata
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)
head(Moosedata)
plot(Moosedata$Year, Moosedata$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")
head(MooseDataWest)
plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
     type = "l",
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")
MooseData_2020 <- filter(Moosedata, Year == 2020)
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)
MooseData_2020_c <- arrange(MooseData_2020_b, desc(MooseDensity))
MooseData_2020_c
MooseData_2020_c <- arrange(MooseData_2020_b, desc(MooseDensity))
MooseData_2020_c
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
Saplings <- read.csv("SaplingStudy.csv")
Saplings <- na.omit(Saplings)
Saplings <- read.csv("SaplingStudy.csv")
Saplings <- na.omit(Saplings)
Saplings
Saplings <- read.csv("SaplingStudy.csv")
Saplings <- na.omit(Saplings)
Saplings
SaplingBrowse_byEco <- Saplings %>%
  group_by(Ecoregion) %>%
  summarize(meanBrowsing = mean(BrowsingScore)) %>%
  print()
SaplingHeight_byEco <- Saplings %>%
  group_by(Ecoregion) %>%
  summarize(meanHeight = mean(Height)) %>%
  print()
SaplingBrowse_bySpecies <- Saplings %>%
  group_by(Species) %>%
  summarize(meanBrowsing = mean(BrowsingScore)) %>%
  print()
BalsamFir <- Saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(meanBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(BalsamFir$meanBrowsing,
        names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Average Balsam Fir browsing intensity by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
BlackSpruce <- Saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(meanBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(BlackSpruce$meanBrowsing,
        names.arg = BlackSpruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Average Black Spruce browsing intensity by ecoregion",
        col = "steelblue",
        cex.names = 0.6)
EcoregionTally <- Saplings %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
SpeciesTally <- Saplings %>%
  group_by(Species) %>%
  tally() %>%
  print()
MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)
colnames(MooseCollisions)[colnames(MooseCollisions) == "StudySites"] <- "Ecoregion"
MooseCollisions <- MooseCollisions %>%
  rename(Ecoregion = StudySites)
MooseData_2020 <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
MooseSaplingData <- left_join(MooseData_2020, Saplings, by = "Ecoregion")
BrowsingBySpeciesDensity <- MooseSaplingData %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
StudySites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                "Long_Range_Barrens","Central_Forests","Western_Forests",
                "EasternHyperOceanicBarrens","Maritime_Barrens",
                "Avalon_Forests","StraitOfBelleIsleBarrens")

MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)

StudySites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                "Long_Range_Barrens","Central_Forests","Western_Forests",
                "EasternHyperOceanicBarrens","Maritime_Barrens",
                "Avalon_Forests","StraitOfBelleIsleBarrens")
MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
MooseCollisions
MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)
MooseCollisions
MooseCollisions
left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
MooseCollisions <- MooseCollisions %>%
  rename(Ecoregion = StudySites)

MooseCollisionData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
MooseCollisionData
MooseCollisions <- MooseCollisions %>%
  rename(Ecoregion = StudySites)

MooseCollisionData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
MooseCollisions <- MooseCollisions %>% rename(Ecoregion = StudySites)
MooseCollisionData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
MooseCollisions <- MooseCollisions %>%
  rename(Ecoregion = StudySites)

MooseCollisionData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
MooseCollisionData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
plot(MooseCollisionData$MooseDensity,
     MooseCollisionData$Collisions_2020,
     xlab = "Moose Density",
     ylab = "Number of Moose–Vehicle Collisions",
     main = "Moose Density vs Moose–Vehicle Collisions (2020)")
MooseCollisionData <- MooseCollisionData %>%
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)
MooseCollisionData
MooseCollisionData <- MooseCollisionData %>%
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)
plot(MooseCollisionData$HumanPopulation,
     MooseCollisionData$CollisionsPerCapita,
     xlab = "Human Population",
     ylab = "Moose Collisions Per Capita",
     main = "Moose Collisions Per Capita vs Human Population (2020)")
moose_raw <- read.csv("MoosePopulation.csv")