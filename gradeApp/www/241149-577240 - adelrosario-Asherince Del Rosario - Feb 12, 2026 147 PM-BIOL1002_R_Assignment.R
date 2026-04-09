

#AsherinceDelRosario_BIOl1002_RAssignment
###############################################

# Q1 Install/load dplyr
# install.packages("dplyr")  # run once only
library(dplyr)

################################################
# Part I: Moose Populations in Newfoundland
################################################

# Q2 Load MoosePopulation.csv
Moosedata <- read.csv("MoosePopulation.csv")

# Q3 Remove rows with missing values
Moosedata <- na.omit(Moosedata)

# Q4 Keep only columns of interest
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Q5a Oldest Year
min(Moosedata$Year)
# Answer (comment): Oldest Year = 1904

# Q5b Maximum Estimated_Moose_Pop, and where it occurs
max(Moosedata$Estimated_Moose_Pop)
Moosedata %>% filter(Estimated_Moose_Pop == max(Estimated_Moose_Pop))
# Answer (comment): Max Estimated_Moose_Pop = 41250 (Central_Forests, Year 2020).
# This value is for an ecoregion row (not total NL).

# Q6 Create MooseDensity (moose per sq km)
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)

# Q7 LINE graph: MooseDensity vs Year (all ecoregions together)
plot(Moosedata$Year, Moosedata$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

# Q8a Filter Western_Forests
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")

# Q8b LINE graph for Western_Forests
plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density over time in Western_Forests")

# Q9a Make dataset for 2020
MooseData_2020 <- filter(Moosedata, Year == 2020)

# Q9b Filter density > 2.0
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)

# Q9c Arrange from highest to lowest
MooseData_2020_c <- arrange(MooseData_2020_b, desc(MooseDensity))
MooseData_2020_c
# Answer (comment): Northern_Peninsula_Forests, North_Shore_Forests, Western_Forests, Central_Forests

# Q10 Do Q9 using pipes only (and print)
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

################################################
# Part II: Tree Sapling Study
################################################

# Q11a Load SaplingStudy.csv
SaplingData <- read.csv("SaplingStudy.csv")

# Q11b Remove NAs
SaplingData <- na.omit(SaplingData)

# (Helpful safety) trim whitespace in Species strings
SaplingData$Species <- trimws(SaplingData$Species)

# Q12a Mean BrowsingScore by Ecoregion
BrowseByEco <- SaplingData %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore), .groups = "drop") %>%
  arrange(desc(MeanBrowsing)) %>%
  print()

# Q12b (comment)
# Answer (comment): Highest mean browsing = Northern_Peninsula_Forests
#                  Lowest mean browsing = StraitOfBelleIsleBarrens

# Q13a Mean Height by Ecoregion
HeightByEco <- SaplingData %>%
  group_by(Ecoregion) %>%
  summarize(MeanHeight = mean(Height), .groups = "drop") %>%
  arrange(MeanHeight) %>%
  print()

# Q13b Which ecoregions have mean height < 20 cm?
HeightByEco %>% filter(MeanHeight < 20) %>% print()
# Answer (comment): Western_Forests and Northern_Peninsula_Forests

# Q14a Mean BrowsingScore by Species
BrowseBySpecies <- SaplingData %>%
  group_by(Species) %>%
  summarize(MeanBrowsing = mean(BrowsingScore), .groups = "drop") %>%
  arrange(desc(MeanBrowsing)) %>%
  print()

# Q14b (comment)
# Answer (comment): Highest mean browsing = Black_Ash
#                  Lowest mean browsing  = Black_Spruce

# Q15 Mean browsing for Balsam_Fir by Ecoregion
BalsamFir <- SaplingData %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore), .groups = "drop") %>%
  arrange(desc(MeanBrowsing))

BalsamFir

# Q16 Bar plot for BalsamFir
barplot(BalsamFir$MeanBrowsing,
        names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Balsam Fir browsing score",
        main = "Average browsing intensity on Balsam Fir by ecoregion",
        cex.names = 0.6)

# Q17 Repeat for Black_Spruce + bar plot
BlackSpruce <- SaplingData %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore), .groups = "drop") %>%
  arrange(desc(MeanBrowsing))

BlackSpruce

barplot(BlackSpruce$MeanBrowsing,
        names.arg = BlackSpruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Black Spruce browsing score",
        main = "Average browsing intensity on Black Spruce by ecoregion",
        cex.names = 0.6)

# Q17c (comment)
# Answer (comment): Black Spruce tends to have lower browsing (sometimes near zero) in several ecoregions,
# while Balsam Fir generally stays higher across many regions; both are high in North_Shore_Forests and Northern_Peninsula_Forests.

# Q18 Tally number of trees sampled per Ecoregion
EcoregionTally <- SaplingData %>%
  group_by(Ecoregion) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print()

# Q19 Tally number of trees sampled per Species
SpeciesTally <- SaplingData %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print()

# Q20a (comment)
# Answer (comment): Sampling is not evenly distributed across ecoregions/species
# (e.g., StraitOfBelleIsleBarrens has few samples; Black_Ash appears only once).

# Q20b (comment)
# Answer (comment): This bias matters because means can be unreliable for under-sampled groups and may misrepresent real patterns.

################################################
# Part III: Creating and Joining Datasets
################################################

# Q21a Filter moose data to 2020 + MooseDensity
MooseData_2020 <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Q21b Left-join moose density (2020) into sapling dataset
# Many-to-many is expected because each ecoregion has many saplings
MooseSaplingData <- left_join(MooseData_2020, SaplingData,
                              by = "Ecoregion",
                              relationship = "many-to-many")

# Q22 Avg browsing + avg density for each species within each ecoregion
BrowsingBySpeciesDensity <- MooseSaplingData %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity  = mean(MooseDensity),
    .groups = "drop"
  ) %>%
  print()

# Q23 (comment answers)
# 23(1) Evidence for hypothesis?
# Answer (comment): Browsing does not become uniform at high moose density; species still differ, so evidence is limited.
# 23(2) Most/least favoured?
# Answer (comment): Willow and Alder are generally among the most browsed; Black_Spruce tends to be among the least browsed.
# 23(3) Which species is not shown in the plot and why?
# Answer (comment): None are missing; all species appear. Black_Ash has very few observations so it appears as only one point.

# Q24 Create MooseCollisions data.frame
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
StudySites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens",
                "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)

# Q25(1) Try join and explain why it doesn't work (comment)
# Answer (comment): It doesn’t work because the key column names differ:
# MooseCollisions has StudySites, but MooseData_2020 has Ecoregion.

# Q25(2) Use rename_with() to rename StudySites -> Ecoregion
MooseCollisions_fixed <- MooseCollisions %>%
  rename_with(~"Ecoregion", .cols = "StudySites")

# Q25(3) Join into a new dataset
CollisionsWithDensity <- left_join(MooseCollisions_fixed, MooseData_2020, by = "Ecoregion")

# Q26a Scatterplot MooseDensity vs Collisions_2020
plot(CollisionsWithDensity$MooseDensity, CollisionsWithDensity$Collisions_2020,
     xlab = "Moose density (moose/km^2)",
     ylab = "Moose-vehicle collisions (2020)",
     main = "Moose density vs collisions (2020)")

# Q26b (comment)
# Answer (comment): Generally collisions increase with moose density, but there are outliers.
# Avalon_Forests has high collisions despite moderate density, likely due to high traffic/human activity.

# Q27 Create collisions per capita
CollisionsWithDensity <- CollisionsWithDensity %>%
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)

# (Optional check) Which regions have highest per-capita collisions?
CollisionsWithDensity %>%
  arrange(desc(CollisionsPerCapita)) %>%
  select(Ecoregion, CollisionsPerCapita, Collisions_2020, HumanPopulation, MooseDensity) %>%
  print()

# Q28 Scatterplot CollisionsPerCapita vs HumanPopulation
plot(CollisionsWithDensity$HumanPopulation, CollisionsWithDensity$CollisionsPerCapita,
     xlab = "Human population",
     ylab = "Collisions per person (2020)",
     main = "Collisions per capita vs human population (2020)")

# Q29 (comment)
# Answer (comment): Collisions per capita tends to be higher in smaller populations and lower in larger populations.
# This makes sense because per-capita rates drop when population is large, even if total collisions are high.

