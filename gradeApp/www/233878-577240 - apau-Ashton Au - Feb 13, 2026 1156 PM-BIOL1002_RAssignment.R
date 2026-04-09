library(dplyr)

#Part 1 
Moosedata <- read.csv("MoosePopulation.csv")

View(Moosedata)

Moosedata <- na.omit(Moosedata)

Moosedata <- select(Moosedata,
                    Ecoregion,
                    Year,
                    Area,
                    Estimated_Moose_Pop)

head(Moosedata)

min(Moosedata$Year)
max(Moosedata$Estimated_Moose_Pop)
# Oldest year = 1980
# Highest population = 41250

Moosedata <- mutate(Moosedata,
                    MooseDensity = Estimated_Moose_Pop / Area)

head(Moosedata)

plot(Moosedata$Year,
     Moosedata$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

MooseDataWest <- filter(Moosedata,
                        Ecoregion == "Western_Forests")

MooseDataWest

plot(MooseDataWest$Year,
     MooseDataWest$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")

MooseData_2020 <- filter(Moosedata, Year == 2020)

MooseData_2020_b <- filter(MooseData_2020,
                           MooseDensity > 2.0)

MooseData_2020_b <- arrange(MooseData_2020_b,
                            desc(MooseDensity))

MooseData_2020_b

MooseData_final <-
  Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Part 2

SaplingData <- read.csv("SaplingStudy.csv")
SaplingData <- na.omit(SaplingData)

BrowsingByRegion <-
  SaplingData %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

# Highest browsing = Northern_Peninsula_Forests
# Lowest browsing = StraitOfBelleIsleBarrens

HeightByRegion <-
  SaplingData %>%
  group_by(Ecoregion) %>%
  summarize(MeanHeight = mean(Height)) %>%
  print()

SeverelyBrowsed <-
  HeightByRegion %>%
  filter(MeanHeight < 20) %>%
  print()

# Ecoregions with height < 20 cm = Northern_Peninsula_Forests, Western_Forests

BrowsingBySpecies <-
  SaplingData %>%
  group_by(Species) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

# Highest browsing species = Black_Ash
# Lowest browsing species = Black_Spruce

BalsamFir <-
  SaplingData %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore))

barplot(BalsamFir$MeanBrowsing,
        names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

BlackSpruce <-
  SaplingData %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore))

barplot(BlackSpruce$MeanBrowsing,
        names.arg = BlackSpruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)

# Balsam Fir shows higher browsing intensity than Black Spruce in most ecoregions.


EcoregionTally <-
  SaplingData %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

SpeciesTally <-
  SaplingData %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Sample sizes are uneven across ecoregions and species, which may bias results and reduce reliability of comparisons.

#Part 3

MooseData_2020 <-
  Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

MooseSaplingData <-
  left_join(MooseData_2020,
            SaplingData,
            by = "Ecoregion",
            relationship = "many-to-many")

BrowsingBySpeciesDensity <-
  MooseSaplingData %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()

Collisions_2020 <- c(56,60,14,36,48,10,40,110,6)

HumanPopulation <- c(18000,12000,4000,75100,24000,
                     3500,32000,270000,2300)

StudySites <- c("North_Shore_Forests",
                "Northern_Peninsula_Forests",
                "Long_Range_Barrens",
                "Central_Forests",
                "Western_Forests",
                "EasternHyperOceanicBarrens",
                "Maritime_Barrens",
                "Avalon_Forests",
                "StraitOfBelleIsleBarrens")

MooseCollisions <-
  data.frame(StudySites,
             HumanPopulation,
             Collisions_2020)

MooseCollisions <- rename_with(MooseCollisions,
                               ~ "Ecoregion",
                               StudySites)

CollisionDensity <-
  left_join(MooseCollisions,
            MooseData_2020,
            by = "Ecoregion")

plot(CollisionDensity$MooseDensity,
     CollisionDensity$Collisions_2020)

# Collisions increase as moose density increases, indicating a positive relationship between moose abundance and vehicle collisions.

CollisionDensity <-
  mutate(CollisionDensity,
         CollisionsPerCapita =
           Collisions_2020 / HumanPopulation)

plot(CollisionDensity$HumanPopulation,
     CollisionDensity$CollisionsPerCapita)

# Collisions per capita decrease as human population increases, indicating that people in sparsely populated regions experience a higher risk of moose–vehicle collisions.

