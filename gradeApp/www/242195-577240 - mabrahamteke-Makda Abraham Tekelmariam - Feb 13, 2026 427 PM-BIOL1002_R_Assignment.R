# BIOL 1002 – R Assignment
# Name: Makda Abraham Tekelmariam
# Student Number: 202513640

# ============================
# PART I: MOOSE POPULATIONS
# ============================

# Question 1
install.packages("dplyr")
library(dplyr)

# Question 2
moose <- read.csv("MoosePopulation.csv")

# Question 3
moose_clean <- na.omit(moose)

# Question 4
moose_clean <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5
min(moose_clean$Year)
# Oldest observation year is 1904

max(moose_clean$Estimated_Moose_Pop)
# Highest estimated moose population recorded was 41250, across all Newfoundland ecoregions

# Question 6
moose_clean <- mutate(moose_clean,
                      MooseDensity = Estimated_Moose_Pop / Area)

# Question 7
plot(moose_clean$Year, moose_clean$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8
MooseDataWest <- filter(moose_clean, Ecoregion == "Western_Forests")

plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")

# Question 9
MooseData_2020 <- filter(moose_clean, Year == 2020)

MooseData_2020_high <- filter(MooseData_2020, MooseDensity > 2.0)

MooseData_2020_high_sorted <- arrange(MooseData_2020_high, desc(MooseDensity))

# Question 10
MooseData_final <- moose_clean %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# ============================
# PART II: SAPLING STUDY
# ============================

# Question 11
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

# Question 12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
# Highest browsing: region at top of table
# Lowest browsing: region at bottom of table

# Question 13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20) %>%
  print()
# These regions are considered severely browsed

# Question 14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# Highest browsing: species at top
# Lowest browsing: species at bottom

# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

# Question 17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)

# Black spruce generally shows lower browsing intensity compared to Balsam Fir across most regions.

# Question 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Question 20
# The dataset is not evenly distributed. Some ecoregions and species are overrepresented, which may bias results.
# Recognizing bias is important because uneven sampling can lead to incorrect conclusions about ecological patterns.

# ============================
# PART III: DATA JOINING
# ============================

# Question 21
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")

# Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()

# Question 23
# Moose show more selective browsing at low densities and more uniform browsing at higher densities.
# Moose favor Balsam Fir the most and browse Black Spruce the least.
# Species with insufficient data are not shown due to missing values.

# Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)

coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")

# Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions",
     main = "Moose Density vs Vehicle Collisions")
# Higher moose density generally corresponds with higher collision numbers.

# Question 27
coll_merge_per_capita <- mutate(coll_merge,
                                coll_per_capita = collisions2020 / human_pop)

# Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population")

# Question 29
# Regions with smaller human populations often have higher collisions per capita.
# This trend makes sense as rural areas have more moose habitat and fewer people.
