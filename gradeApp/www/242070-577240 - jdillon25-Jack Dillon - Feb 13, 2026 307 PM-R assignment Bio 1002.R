
install.packages("dplyr")

library(dplyr)

MoosePopulation <- read.csv("C:/Users/Jack's PC/Downloads/MoosePopulation.csv")
View(MoosePopulation)

moose_clean <- na.omit(MoosePopulation)

moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

moosedata2 <- mutate(moose_sel, 
                     MooseDensity = Estimated_Moose_Pop / Area)

plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "p",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western Forests Over Time")

moose_2020 <- filter(moosedata2, Year == 2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, 
                               desc(MooseDensity))

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

SaplingStudy <- read.csv("C:/Users/Jack's PC/Downloads/SaplingStudy.csv")

sap_clean <- na.omit(SaplingStudy)

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  arrange(desc(AverageBrowsing))

# The first row has the highest average browsing.
# The last row has the lowest average browsing.

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  filter(AverageHeight < 20) %>%
  print()

# The ecoregions printed above have average heights less than 20 cm (severely browsed).

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  arrange(desc(AverageBrowsing))

# The first species listed has the highest browsing score.
# The last species listed has the lowest browsing score.

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "darkolivegreen",
        cex.names = 0.6)

# Black Spruce browsing intensity can be compared visually to Balsam Fir.
# Differences in bar height indicate which species experiences heavier browsing in each region.

sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()


# Question 20
# a) If tally results show unequal counts, then the dataset is not evenly distributed.
# Some ecoregions or species may be overrepresented or underrepresented.

# b) Recognizing bias is important because uneven sampling can distort conclusions.
# Overrepresentation can exaggerate trends, while underrepresentation may hide real ecological patterns.

moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, 
                       by = "Ecoregion",
                       relationship = "many-to-many")

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()


# Question 23
# Based on the figure:
# There is some evidence supporting the hypothesis. At lower moose densities,
# browsing appears more selective, while at higher densities browsing becomes
# more uniform across species.

# Moose appear to favor the species with consistently higher AvgBrowsing scores,
# while species with lower AvgBrowsing values are browsed the least.

# Any species not shown likely had missing data or insufficient observations
# after grouping, so they were excluded from the summarized dataset.

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
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

moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")

plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose/km²)",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions (2020)")

# There appears to be a positive relationship between moose density and collisions.
# Some regions may stand out as outliers with unusually high collisions.

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Person",
     main = "Collisions Per Capita vs Human Population")


# Question 29
# Regions with smaller human populations sometimes show higher collisions per capita.
# This makes sense because rural areas may have high moose density but fewer people,
# increasing the per-person collision rate.
