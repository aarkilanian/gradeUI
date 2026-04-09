# Title: BIOL1002 R Assignment - Moose Populations
# Author: Anna Casey
# Date: 11-02-2026


# Question 1 
library(dplyr)

# Set working directory 
setwd("/Users/AnnaCasey/Documents/BioR")

# Question 2
moosedata <- read.csv("MoosePopulation.csv")

# Question 3
moose_clean <- na.omit(moosedata)

# Question 4
moose_sel <- select(moose_clean, 
                    Ecoregion, 
                    Year, 
                    Area, 
                    Estimated_Moose_Pop)

# Question 5a
year_min <- min(moose_sel$Year)

# Question 5b
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")

# Question 8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8b
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western Forests Over Time")

# Question 9a
moose_2020 <- filter(moosedata2, Year == 2020)

# Question 9b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Question 9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Question 11a
saplings <- read.csv("SaplingStudy.csv")

# Question 11b
sap_clean <- na.omit(saplings)

# Question 12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

# Question 12b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# Highest browsing: Northern_Peninsula_Forests (4.57)
# Lowest browsing: StraitOfBelleIsleBarrens (1.00)

# Question 13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>%
  print()

# Question 13b
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

# Northern_Peninsula_Forests (19.9 cm) and Western_Forests (18.9 cm) have average heights below 20 cm

# Question 14a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

# Question 14b
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

# Highest browsing species: Black_Ash (5.00)
# Lowest browsing species: Black_Spruce (2.33)

# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))

# Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

# Question 17a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))

# Question 17b
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "darkolivegreen",
        cex.names = 0.6)

# Comment: In avalon forests, balsam fir is browsed more, while in EasternHyperOceanicBarrens, black spruce is browsed more

# Question 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# Answer Q18: Number of saplings per ecoregion
# Avalon_Forests: 5
# Central_Forests: 5
# EasternHyperOceanicBarrens: 5
# Long_Range_Barrens: 5
# Maritime_Barrens: 3
# North_Shore_Forests: 8
# Northern_Peninsula_Forests: 7
# StraitOfBelleIsleBarrens: 1
# Western_Forests: 5

# Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Answer Q19: Number of saplings per species
# Alder: 8
# Balsam_Fir: 11
# Black_Ash: 1
# Black_Spruce: 9
# White_Birch: 7
# Willow: 8

# Question 20a
# The SaplingStudy dataset is not evenly distributed.
# Some ecoregions are overrepresented (e.g., North_Shore_Forests: 8, Northern_Peninsula_Forests: 7)
# while others are underrepresented (e.g., StraitOfBelleIsleBarrens: 1, Maritime_Barrens: 3).
# Some species are also overrepresented (e.g., Balsam_Fir: 11, Black_Spruce: 9)
# and some are underrepresented (e.g., Black_Ash: 1).

# Question 20b
# Bias matters because uneven sampling can give a misleading picture
# of moose browsing patterns.


# Question 21a
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Question 21b
library(dplyr)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

# Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()

# Question 23a
# At low moose densities, moose seem to pick their favorite species.
# At high densities, they browse more evenly across all species.

# Question 23b
# Most browsed species: Black_Ash and Willow
# Least browsed species: Black_Spruce
# Moose like Black_Ash and Willow the most.
# They browse Black_Spruce the least.

# Question 23c
# The sapling species not shown on the figure is Black_Ash.
# This is because there were no Black_Ash saplings recorded in 2020, so no data exists to plot.

# Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25a
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

# Question 25b
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Question 26a
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions",
     main = "Moose Density vs Moose-Vehicle Collisions",
     pch = 16)

# Question 26b
# Moose-vehicle collisions generally increase with moose density,
# but there is an outlier where there are about 100 collisions even though moose density is only 1.

# Question 27
# Create collisions per capita
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# The ecoregions with the highest collisions per person are:
# 1) Northern_Peninsula_Forests (0.005 collisions/person)
# 2) North_Shore_Forests (0.0031 collisions/person)


# Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population",
     pch = 16)

# Question 29
# Areas with fewer people tend to have more collisions per person, while bigger towns have more people but fewer collisions per person.
# This makes sense because moose are more likely to cross roads in less populated, forested areas.



