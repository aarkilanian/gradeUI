# Name: Emily Flynn
# Student Number: 202510985
# Course: BIOL1002
# Assignment: Moose Populations & Browsing

# Set working directory
# setwd("User/EmilyFlynn/Documents/RStudio")

# Install & load packages
# install.packages("dplyr") # only run once
library(dplyr)

# PART I: MOOSE POPULATIONS IN NEWFOUNDLAND

# Question 2: Import data
moosedata <- read.csv("MoosePopulation.csv")

# Question 3: View & remove NAs
View(moosedata)
moose_clean <- na.omit(moosedata)

# Question 4: Select columns of interest
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5a: Oldest observation
year_min <- min(moose_sel$Year)

# Question 5b: Highest estimated moose population
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Answer:
# Oldest year in dataset is: 1904
# Highest estimated moose population is: 41250

# Question 6: Calculate moose density
moosedata2 <- mutate(moose_sel,
                     MooseDensity = Estimated_Moose_Pop / Area)

# Question 7: Plot Moose Density over time
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per km²",
     main = "Moose Density in Newfoundland Ecoregions Over Time")

# Question 8a: Western Forests only
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8b: Plot Western Forests trend
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per km²",
     main = "Moose Density Over Time in Western Forests")

# Question 9: Filter for 2020 & high density
moose_2020 <- filter(moosedata2, Year == 2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10: Same task using pipes
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# PART II: TREE SAPLING STUDY

# Question 11: Load & clean sapling data
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

# Question 12a: Mean browsing by ecoregion
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 12b: Highest & lowest browsing regions
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# Answer:
# The ecoregion with the highest average browsing is Northern_Peninsula_Forests.
# The ecoregion with the lowest average browsing is StraitOfBelleIsleBarrens.

# Question 13: Mean height by ecoregion
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

# Answer:
# Ecoregions with average sapling height below 20 cm are considered severely browsed.

# Question 14a: Browsing by species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AvgBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AvgBrowsing))

# Answer:
# Species with highest browsing is Black Ash.
# Species with lowest browsing is Black Spruce.

# Question 15: Balsam Fir browsing by ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore))

# Question 16: Bar plot for Balsam Fir
barplot(fir_reg_browse$AvgBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing Intensity by Ecoregion",
        col = "yellow",
        cex.names = 0.6)

# Question 17: Black Spruce browsing
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore))

barplot(spruce_reg_browse$AvgBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "pink"
        cex.names = 0.6)

# Answer:
# Black Spruce generally shows lower browsing intensity than Balsam Fir across most ecoregions.

# Question 18: Trees per ecoregion
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# Question 19: Trees per species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Question 20:
# a) The SaplingStudy dataset is not evenly distributed. There are some ecoregions and species are sampled more heavily.
# b) Recognizing bias is important because uneven sampling can lead to misleading conclusions about browsing pressure.

# PART III: JOINING DATASETS

# Question 21: Moose density for 2020 + join
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")

# Question 22: Avg browsing & density by species/ecoregion
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()

# Question 23:
# Moose show selective browsing at low density and more uniform browsing at higher density.
# Balsam Fir appears to be most favored, and Black Spruce is browsed least.
# One species is not shown due to missing density data after grouping.

# MOOSE–VEHICLE COLLISIONS

# Question 24: Create collision dataset
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25: Rename & join
moose_coll2 <- rename_with(moose_coll,
                           ~"Ecoregion",
                           study_sites)

coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Question 26: Moose density vs collisions
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions",
     main = "Moose Density vs Vehicle Collisions")

# Answer:
# There is a positive trend between moose density and collisions, with some outliers.

# Question 27: Collisions per capita
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# Question 28: Plot per capita collisions
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Moose Collisions Per Capita vs Human Population")

# Question 29:
# Regions with low human populations but moderate moose density tend to have higher per-capita collision rates.
# This makes sense because fewer people share the same level of moose risk.

