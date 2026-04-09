# Title: BIOL1002 Assignment
# Name: Sara Khan
# Student ID: 202513369
# Date: 2026-02-22

# Question 1
# Load tidyverse
library(tidyverse)

# Question 2
# Set working directory and check files
setwd("C:/Users/96650/Desktop/BIOL1002_Coding")
list.files()
moosedata <- read.csv("MoosePopulation.csv")

# Question 3
# Remove missing values
moose_clean <- na.omit(moosedata)

# Question 4
# Select the columns Ecoregion, Year, Area, and Estimated_Moose_Pop and save as moose_sel
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)

# Question 5
# a) Oldest year in the dataset
year_min <- min(moose_sel$Year)
# b) Highest Estimated_Moose_Pop in the dataset
moose_max <- max(moose_sel$Estimated_Moose_Pop)
year_min
moose_max

# Question 6
# Creating a new column MooseDensity by dividing population by area and saving it  as moosedata2
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)

# Question 7
# Create a line graph to show how MooseDensity changes over Year
plot(moosedata2$Year,
     moosedata2$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8
# Creating a new dataset with only Western_Forests ecoregion
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)
# Question 8b
# Creating a line graph to show MooseDensity over Year for Western_Forests
plot(moose_west$Year,
     moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")



# Question 9a
# Filter data for year 2020
moose_2020 <- filter(moosedata2, Year == 2020)
View(moose_2020)
# Question 9b
# Keep only MooseDensity greater than 2.0
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
View(moose_2020_high)
# Question 9c
# Sort MooseDensity in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
View(moose_2020_high_byD)

# Question 10
# Filter year 2020, keep MooseDensity > 2.0, sort descending using pipes, saving as moosefinal
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
View(moosefinal)

# Part II: Tree Sapling Study

# Question 11
# Load SaplingStudy data
saplings <- read.csv("SaplingStudy.csv")
# Remove NA values
sap_clean <- na.omit(saplings)

# Question 12a
# Mean browsing score by ecoregion
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))
print(sap_reg_browse)
View(sap_reg_browse)
# Question 12b
# Sort AverageBrowsing high to low
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
# Highest = first row, Lowest = last row
print(avg_browse_reg)
View(avg_browse_reg)
# Highest browsing: Northern_Peninsula_Forests
# Lowest browsing: StraitOfBelleIsleBarrens

# Question 13a
# Mean tree height by ecoregion
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageHeight = mean(Height))
print(sap_reg_height)
View(sap_reg_height)
# Highest average height: Avalon_Forests
# Lowest average height: Western_Forests
# Question 13b
# Ecoregions with AverageHeight less than 20
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)
print(sap_reg_height_low)
View(sap_reg_height_low)
# Ecoregions with AverageHeight less than 20 cm:
# Northern_Peninsula_Forests and Western_Forests

# Question 14a
# Mean browsing score by species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))
print(sap_spe_browse)
View(sap_spe_browse)
# Question 14b
# Sort species by browsing score (high to low)
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
print(avg_browse_spe)
# Highest browsing: Black_Ash
# Lowest browsing: Black_Spruce

# Question 15
# Average browsing score of Balsam Fir by ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))
print(fir_reg_browse)

# Question 16
# Bar graph showing average browsing of Balsam Fir by ecoregion
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Balsam Fir browsing by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

# Question 17
# Average browsing score of Black Spruce by ecoregion
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))
print(spruce_reg_browse)
# Bar graph showing average browsing of Black Spruce by ecoregion
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Black Spruce browsing by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
# Black Spruce has lower browsing scores compared to Balsam Fir across most ecoregions.
# This would mean that moose browse Balsam Fir more heavily than Black Spruce.


# Question 18
# Count number of tree saplings in each ecoregion
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally()
print(sap_reg_tally)
View(sap_reg_tally)
# The number of saplings is not the same in each ecoregion.
# North_Shore_Forests has the most (8) and StraitOfBelleIsleBarrens has the least (1).


# Question 19
# Count number of tree saplings for each species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()
print(sap_spe_tally)
# Sapling counts are not the same for each species.
# Some species were sampled more than others.
# Balsam_Fir has the most saplings (11) and Black_Ash has the least (1).

# Question 20a
# The SaplingStudy dataset is not evenly distributed.
# Balsam_Fir and North_Shore_Forests were sampled more and Black_Ash and StraitOfBelleIsleBarrens were sampled less.
# Question 20b
# Recognizing bias is important because uneven sampling can affect the results.
# It may not accurately represent the real browsing patterns.

# Question 21a
# Filter for 2020 and create MooseDensity column
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
print(moose_2020b)
# Question 21b
# Joining moose density data with sapling data by ecoregion
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
print(moose_sap)
# Moose density and sapling browsing data are now combined into one dataset

# Question 22
# Calculate average browsing score and average moose density by species and ecoregion
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgMooseDensity = mean(MooseDensity)
  )
print(sum_spe_browse)
# Shows average browsing and moose density for each species in each ecoregion



# Question 23a
# Yes, there is some support. At low moose density, browsing differs more between species, but at higher density the browsing scores are higher and more similar across species.
# Question 23b
# Moose favour Willow and White_Birch the most (highest browsing scores), and Black_Spruce the least (lowest browsing scores).
# Question 23c
# Black_Ash is not shown because there were no browsing observations recorded for that in the combined dataset used for the plot.


# Question 24
# Creates a new dataset combining collisions, human population, and study sites
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 27000, 2300)
study_sites <- c("North_Shore_Forests",
                 "Northern_Peninsula_Forests",
                 "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens",
                 "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
print(moose_coll)

# Question 25a
# Rename the column holding site information in the moose_coll dataset and save the renamed result as moose_coll2
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)   # Renames study_sites to Ecoregion so it matches the moose dataset
# Question 25b
# Join the datasets into a new dataset using left_join() and save the joined dataset as coll_merge
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")   # Combines collision data with moose density data


# Question 26a
# Create a scatterplot of MooseDensity and collisions2020 to see their relationship
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions",
     main = "Moose Density vs Moose-Vehicle Collisions")
# Shows relationship between moose density and number of collisions
# Question 26b
# There is a positive relationship where higher moose density has more collisions.
# The highest density areas have the most collisions, and there are no major extreme outliers.

# Question 27
# Create a new column called coll_per_capita by dividing collisions2020 by human_pop
# and save the new dataset as coll_merge_per_capita
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)
# Avalon_Forests appears to have the highest collisions per person,
# meaning collision risk is highest there relative to population size.

# Question 28
# Create a scatterplot of collisions per capita vs human population
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs Human Population")
# Shows relationship between human population and collision risk per person

# Question 29
# Describe the trend in the scatterplot
# Collisions per capita decrease as human population increases.
# This makes sense because areas with fewer people have more moose per person, increasing collision risk.


