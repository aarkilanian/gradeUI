getwd()
list.files()
############################################################
# BIOL1002 R Assignment
# Logan Slaney
############################################################

rm(list = ls())

library(dplyr)
library(ggplot2)

############################################################
# PART I — MOOSE POPULATION
############################################################

MooseData <- read.csv("MoosePopulation.csv")

MooseData <- na.omit(MooseData)

MooseData <- MooseData %>%
  select(Ecoregion, Year, Area, Estimated_Moose_Pop)

min(MooseData$Year)
max(MooseData$Estimated_Moose_Pop)

MooseData <- MooseData %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Q7 Plot (all ecoregions)
plot(
  MooseData$Year, MooseData$MooseDensity,
  xlab = "Year",
  ylab = "Moose Density (moose/km²)",
  main = "Moose Density in Newfoundland Ecoregions Over Time",
  pch = 19,
  col = "royalblue"
)

# Q8 Western Forests only
MooseWest <- MooseData %>%
  filter(Ecoregion == "Western_Forests")

plot(
  MooseWest$Year, MooseWest$MooseDensity,
  type = "l",
  xlab = "Year",
  ylab = "Moose Density (moose/km²)",
  main = "Moose Density in Western Forests Over Time",
  lwd = 2,
  col = "darkgreen"
)

# Q9 + Q10 2020 high density
MooseHigh2020 <- MooseData %>%
  filter(Year == 2020) %>%
  filter(MooseDensity >= 2.0) %>%
  arrange(desc(MooseDensity))

MooseHigh2020

############################################################
# PART II — SAPLING STUDY
############################################################

saplings <- read.csv("SaplingStudy.csv")
saplings <- na.omit(saplings)

# Q12 Mean browsing by ecoregion
BrowsingEco <- saplings %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore))

BrowsingEco

# Interpretation:
# Northern Peninsula and North Shore show the highest browsing,
# while Maritime Barrens and Strait of Belle Isle show the lowest.

# Q13 Mean height by ecoregion
HeightEco <- saplings %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Height = mean(Height))

HeightEco

HeightEco %>% filter(Mean_Height < 20)

# Interpretation:
# Lower average heights likely indicate heavier browsing pressure.

# Q14 Browsing by species
BrowsingSpecies <- saplings %>%
  group_by(Species) %>%
  summarize(Mean_Browsing = mean(BrowsingScore))

BrowsingSpecies

# Interpretation:
# Black Ash appears most browsed, while Black Spruce is least preferred.

# Q15 Balsam Fir by ecoregion
BalsamFir <- saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore))

barplot(
  BalsamFir$Mean_Browsing,
  names.arg = BalsamFir$Ecoregion,
  col = "steelblue",
  las = 2,
  main = "Balsam Fir Browsing by Ecoregion",
  ylab = "Average Browsing Score"
)

# Q17 Black Spruce
BlackSpruce <- saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore))

barplot(
  BlackSpruce$Mean_Browsing,
  names.arg = BlackSpruce$Ecoregion,
  col = "mediumpurple3",
  las = 2,
  main = "Black Spruce Browsing by Ecoregion",
  ylab = "Average Browsing Score"
)

# Interpretation:
# Balsam Fir consistently shows higher browsing than Black Spruce,
# suggesting moose prefer fir over spruce.

# Q18 + Q19 Tallies
saplings %>% group_by(Ecoregion) %>% tally()
saplings %>% group_by(Species) %>% tally()

# Interpretation:
# Saplings are not evenly distributed; uneven sample sizes may affect comparisons.

############################################################
# PART III — JOIN + COLLISIONS
############################################################

Moose2020 <- MooseData %>%
  filter(Year == 2020)

MooseSaplings <- left_join(saplings, Moose2020, by = "Ecoregion")

BrowsingDensity <- MooseSaplings %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    Avg_Browsing = mean(BrowsingScore),
    Avg_Density = mean(MooseDensity),
    .groups = "drop"
  )

ggplot(BrowsingDensity,
       aes(x = Avg_Density, y = Avg_Browsing, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Browsing Intensity vs Moose Density (2020)",
    x = "Moose Density (moose/km²)",
    y = "Average Browsing Score"
  )

# Interpretation:
# At lower moose densities, species preference is clearer.
# At higher densities, browsing appears more generalized.

# Q24 Collisions data
collisions2020 <- c(56,60,14,36,48,10,40,110,6)
human_pop <- c(18000,12000,4000,75100,24000,3500,32000,270000,2300)
study_sites <- c(
  "North_Shore_Forests","Northern_Peninsula_Forests",
  "Long_Range_Barrens","Central_Forests",
  "Western_Forests","EasternHyperOceanicBarrens",
  "Maritime_Barrens","Avalon_Forests",
  "StraightOfBelleIsleBarrens"
)

MooseCollisions <- data.frame(
  Ecoregion = study_sites,
  human_pop,
  collisions2020
)

MooseCollisions <- left_join(MooseCollisions, Moose2020, by = "Ecoregion")

# Q26 Density vs collisions
plot(
  MooseCollisions$MooseDensity,
  MooseCollisions$collisions2020,
  pch = 19,
  col = "navy",
  xlab = "Moose Density (moose/km²)",
  ylab = "Collisions (2020)",
  main = "Moose Density vs Vehicle Collisions"
)

# Interpretation:
# Higher moose density is associated with increased vehicle collisions.

# Q27 Collisions per capita
MooseCollisions <- MooseCollisions %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

MooseCollisions %>%
  arrange(desc(coll_per_capita))

# Q28 Plot per capita
plot(
  MooseCollisions$human_pop,
  MooseCollisions$coll_per_capita,
  pch = 19,
  col = "seagreen4",
  xlab = "Human Population",
  ylab = "Collisions Per Capita",
  main = "Collisions Per Capita vs Human Population"
)

# Final Interpretation:
# Rural regions with smaller populations show higher per-capita collision rates,
# likely due to greater overlap between roads and moose habitat.
############################################################

