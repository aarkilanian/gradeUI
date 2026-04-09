# BIOL 1002: Moose Ecology Study
# Liam Humphries

# Load dplyr package
library(dplyr)

# -------------------------------------------------------------------------
# PART 1: MOOSE POPULATIONS
# -------------------------------------------------------------------------

# Question 2 - Import the moose data
Moosedata <- read.csv("MoosePopulation.csv")

# Question 3 - Remove rows with missing values
Moosedata <- na.omit(Moosedata)

# Question 4 - Select only specific columns
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5a - Find the oldest year
min(Moosedata$Year)
# Answer: 1904

# Question 5b - Find the highest moose population
max(Moosedata$Estimated_Moose_Pop)
# Answer: 41250

# Question 6 - Calculate moose density
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7 - Make a line graph
plot(Moosedata$Year, Moosedata$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time",
     type = "l")

# Question 8a - Create dataset just for Western Forests
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")

# Question 8b - Make a line graph for Western Forests
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests ecoregion over time",
     type = "l")

# Question 9a - Filter for year 2020 only
MooseData_2020 <- filter(Moosedata, Year == 2020)

# Question 9b - Filter for moose density above 2.0
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)

# Question 9c - Sort by moose density (highest first)
arrange(MooseData_2020_b, desc(MooseDensity))

# Question 10 - Do it all with pipes
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# -------------------------------------------------------------------------
# PART 2: TREE SAPLING STUDY
# -------------------------------------------------------------------------

# Question 11a - Load the sapling data
Saplings <- read.csv("SaplingStudy.csv")

# Question 11b - Remove missing values
Saplings <- na.omit(Saplings)

# Question 12 - Calculate average browsing score by ecoregion
Saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
# Answer - Ecoregion with highest browsing: Northern_Peninsula_Forests
# Answer - Ecoregion with lowest browsing: StraitOfBelleIsleBarrens

# Question 13a - Calculate average tree height by ecoregion
Saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean(Height)) %>%
  print()
# Answer - Ecoregions with average height less than 20 cm: Northern_Peninsula_Forests,Western_Forests

# Question 14a - Calculate average browsing score by species
Saplings %>%
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
# Answer - Species with highest browsing: Black_Ash
# Answer - Species with lowest browsing: Black_Spruce

# Question 15 - Filter for Balsam Fir only, group by ecoregion
BalsamFir <- Saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))
print(BalsamFir)

# Question 16 - Make a bar graph for Balsam Fir
barplot(BalsamFir$`mean(BrowsingScore)`, 
        names.arg = BalsamFir$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score", 
        main = "Balsam Fir Browsing Intensity by Ecoregion", 
        col = "forestgreen", 
        cex.names = 0.6)

# Question 17a - Filter for Black Spruce only
BlackSpruce <- Saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))
print(BlackSpruce)

# Question 17b - Make a bar graph for Black Spruce
barplot(BlackSpruce$`mean(BrowsingScore)`, 
        names.arg = BlackSpruce$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score", 
        main = "Black Spruce Browsing Intensity by Ecoregion", 
        col = "darkgreen", 
        cex.names = 0.6)

# Question 17c - Compare Black Spruce and Balsam Fir
# Answer: Black Spruce is browsed more heavily in Northern_Peninsula_Forests, while Balsam Fir 
# is browsed more in Avalon_Forests. In other ecoregions, browsing is similar between the two species.

# Question 18 - Count how many trees in each ecoregion
EcoregionTally <- Saplings %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# Question 19 - Count how many trees of each species
SpeciesTally <- Saplings %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Question 20a - Is the dataset evenly distributed?
# Answer: No, the dataset is not evenly distributed. For ecoregions, North_Shore_Forests is 
# overrepresented with 8 trees, while StraitOfBelleIsleBarrens is severely underrepresented with only 
# 1 tree. For species, Balsam_Fir is overrepresented with 11 trees, while Black_Ash is extremely 
# underrepresented with only 1 tree.

# Question 20b - Why is it important to recognize bias?
# Answer: Recognizing bias is important because it affects the accuracy of our conclusions. 
# If some ecoregions or species have very few samples (like StraitOfBelleIsleBarrens with only 1 tree 
# or Black_Ash with only 1 tree), we cannot confidently say that our data represents those areas or 
# species. This could lead to wrong conclusions about moose browsing patterns across all of Newfoundland.

# -------------------------------------------------------------------------
# PART 3: CREATING AND JOINING DATASETS
# -------------------------------------------------------------------------

# Question 21a - Filter moose data for 2020 (already did this in Q9a)
# MooseData_2020 already exists from earlier

# Question 21b - Join moose data with sapling data
MooseSaplingData <- left_join(MooseData_2020, Saplings, 
                              by = "Ecoregion", 
                              relationship = "many-to-many")

# Question 22 - Calculate average browsing and moose density
BrowsingBySpeciesDensity <- MooseSaplingData %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()

# Question 23a - Is there evidence supporting the hypothesis?
# Answer: The evidence does not strongly support the hypothesis. At low densities 
# (like EasternHyperOceanicBarrens), browsing scores range from 3.0 to 4.0 across species. 
# At high densities (like Northern_Peninsula_Forests), scores still range widely from 1.0 to 5.0, 
# showing moose maintain preferences even at high density rather than becoming generalists.

# Question 23b - Which species do moose favor most/least?
# Answer: Based on the data, moose favor Alder the most, as it consistently has high browsing 
# scores (4.0-5.0) across most ecoregions. They browse Balsam_Fir the least, with scores as low as 
# 1.0 in some areas like Northern_Peninsula_Forests

# Question 23c - Which species is missing and why?
# Answer: Black_Ash is not shown in the figure. This is likely because it had very few samples 
# (only 1 tree in the entire study from Question 19), so there wasn't enough data to include it 
# in the visualization.

# Question 24 - Create collision data vectors
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
StudySites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", 
                "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", 
                "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")

# Create the data frame
MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)

# Question 25a - Try to join (this might not work - that's the point)
# Moose_joined <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
# Answer - Why doesn't this work? This doesn't work because the MooseCollisions dataset uses "StudySites" instead of "Ecoregion".

# Question 25b - Rename column to match
MooseCollisions <- rename(MooseCollisions, Ecoregion = StudySites)

# Question 25c - Now join them
MooseCollisionData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")

# Question 26a - Make scatterplot
plot(MooseCollisionData$MooseDensity, MooseCollisionData$Collisions_2020,
     xlab = "Moose Density", 
     ylab = "Number of Collisions (2020)",
     main = "Moose-Vehicle Collisions vs Moose Density")

# Question 26b - What trends do you see?
# Answer: There doesn't appear to be a strong relationship between moose density and collisions. 
# Some areas with low density have high collisions, and vice versa, suggesting other factors 
# like roads or human population.

# Question 27 - Calculate collisions per person
MooseCollisionData <- mutate(MooseCollisionData, 
                             CollisionsPerCapita = Collisions_2020 / HumanPopulation)

# Question 28 - Make scatterplot of collisions per capita vs human population
plot(MooseCollisionData$HumanPopulation, MooseCollisionData$CollisionsPerCapita,
     xlab = "Human Population", 
     ylab = "Collisions Per Capita",
     main = "Moose Collisions Per Capita vs Human Population")

# Question 29 - Describe the trends you see
# Answer: The graph shows that ecoregions with smaller human populations tend to have higher 
# collisions per person, while areas with larger populations have lower collisions per person. 
# This makes sense because in remote areas with fewer people, moose are more abundant and roads 
# may cut through moose habitat, while in cities with more people, moose are less common and 
# traffic moves slower.