# ============================================================
# BIOL 1002 - R Assignment
# Ahmad Almogharbel
# ============================================================

setwd("/Users/ahmadalmogharbel/BIOL1002_R_Assignment_Ahmad_Almogharbel")


library(dplyr)
library(magrittr)

# ----------------------------
# Part I: Moose Populations (Q1–Q10)
# ----------------------------

Moosedata <- read.csv("MoosePopulation.CSV") %>% na.omit()
Moosedata <- Moosedata %>% select(Ecoregion, Year, Area, Estimated_Moose_Pop)

min(Moosedata$Year)
max(Moosedata$Estimated_Moose_Pop)
Moosedata %>% filter(Estimated_Moose_Pop == max(Estimated_Moose_Pop))

Moosedata <- Moosedata %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)

plot(Moosedata$Year, Moosedata$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

MooseDataWest <- Moosedata %>% filter(Ecoregion == "Western_Forests")

plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")

MooseData_final <- Moosedata %>%
  filter(Year == 2020, MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# ----------------------------
# Part II: Tree Sapling Study (Q11–Q20)
# ----------------------------

Saplings <- read.csv("SaplingStudy.csv") %>% na.omit()

Browsing_by_Ecoregion <- Saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_BrowsingScore)) %>%
  print()

Height_by_Ecoregion <- Saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_Height_cm = mean(Height, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_Height_cm) %>%
  print()

SeverelyBrowsed_Ecoregions <- Height_by_Ecoregion %>%
  filter(mean_Height_cm < 20) %>%
  print()

Browsing_by_Species <- Saplings %>%
  group_by(Species) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_BrowsingScore)) %>%
  print()

BalsamFir <- Saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_BrowsingScore)) %>%
  print()

barplot(BalsamFir$mean_BrowsingScore,
        names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Mean Browsing Score",
        main = "Average Balsam Fir browsing intensity by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

BlackSpruce <- Saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_BrowsingScore)) %>%
  print()

barplot(BlackSpruce$mean_BrowsingScore,
        names.arg = BlackSpruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Mean Browsing Score",
        main = "Average Black Spruce browsing intensity by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

EcoregionTally <- Saplings %>%
  group_by(Ecoregion) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print()

SpeciesTally <- Saplings %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print()

# Q20a:
# The SaplingStudy dataset is not evenly distributed. North_Shore_Forests and Northern_Peninsula_Forests
# are overrepresented, while StraitOfBelleIsleBarrens and Maritime_Barrens are underrepresented.
# By species, Balsam_Fir and Black_Spruce are overrepresented, whereas Black_Ash is strongly underrepresented.
# Q20b:
# Uneven sampling can bias conclusions about browsing patterns.

# ----------------------------
# Part III: Joining Datasets (Q21–Q29)
# ----------------------------

MooseData_2020 <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

MooseSaplingData <- left_join(MooseData_2020, Saplings, by = "Ecoregion")

BrowsingBySpeciesDensity <- MooseSaplingData %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity  = mean(MooseDensity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Q23a:
# Browsing is more variable among species at low moose density and more similar at higher density.
# Q23b:
# Willow and Black_Ash have the highest browsing scores; Black_Spruce has the lowest.
# Q23c:
# Black_Ash has only one observation, so it appears as a single point.

Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
StudySites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens",
                "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)

MooseCollisions <- MooseCollisions %>% rename(Ecoregion = StudySites)
MooseCollisionsData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")

plot(MooseCollisionsData$MooseDensity, MooseCollisionsData$Collisions_2020,
     xlab = "Moose Density",
     ylab = "Moose–Vehicle Collisions (2020)",
     main = "Moose Density vs Vehicle Collisions")

# Q26:
# Collisions generally increase with moose density; Avalon_Forests is high due to population/traffic.

MooseCollisionsData <- MooseCollisionsData %>%
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)

plot(MooseCollisionsData$HumanPopulation, MooseCollisionsData$CollisionsPerCapita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population")

# Q29:
# Smaller human populations tend to have higher collisions per capita.

