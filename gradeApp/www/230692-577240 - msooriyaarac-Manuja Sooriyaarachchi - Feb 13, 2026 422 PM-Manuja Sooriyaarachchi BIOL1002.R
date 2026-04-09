# Title: BIOL1002 R assignment
# Author: Manuja sooriyaarachchi
# Date: 13-2-2026


# PART I: Moose Populations in Newfoundland
# Question 1
library(dplyr)


# Question 2
moosedata <- read.csv("MoosePopulation.csv")


# Question 3
moose_clean <- na.omit(moosedata)


# Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)


# Question 5a
year_min <- min(moose_sel$Year)
year_min

# Question 5b
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max


# Question 6
moosedata2 <- mutate(moose_sel,
                     MooseDensity = Estimated_Moose_Pop / Area)


# Question 7
plot(moosedata2$Year,
     moosedata2$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")


# Question 8a
moose_west <- filter(moosedata2,
                     Ecoregion == "Western_Forests")

# Question 8b
plot(moose_west$Year,
     moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western Forests Over Time")


# Question 9a
moose_2020 <- filter(moosedata2, Year == 2020)

# Question 9b
moose_2020_high <- filter(moose_2020,
                          MooseDensity > 2.0)

# Question 9c
moose_2020_high_byD <- arrange(moose_2020_high,
                               desc(MooseDensity))


# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


# PART II: Sapling Study
# Question 11a
saplings <- read.csv("SaplingStudy.csv")

# Question 11b
sap_clean <- na.omit(saplings)


# Question 12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 12b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# Highest browsing region: Northern_Peninsula_Forests
# Lowest browsing region: StraitOfBelleIsleBarren


# Question 13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

# Question 13b
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

# Regions with average height < 20 cm: Northern_Peninsula_Forests, Western_Forests


# Question 14a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 14b
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

# Highest browsed species: Black_Ash
# Lowest browsed species: Black_Spruce


# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))


# Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)


# Question 17a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))


# Question 17b
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)

# Question 17c
# Black Spruce generally shows moderate browsing across ecoregions,
# while Balsam Fir tends to have lower browsing intensity overall.


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
# The dataset is not evenly ditributed
# Sampling bias matters because uneven representation can distort conclusions
# about browsing preference and ecological patterns.  


# PART III: Joining Datasets
# Question 21a
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Question 21b
moose_sap <- left_join(moose_2020b,
                       sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")


# Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()


# Question 23
# There is some evidence supporting the hypothesis, as browsing intensity
# appears to remain high and more uniform at higher moose densities,
# suggesting less selective feeding under increased competition.

# Moose appear to favor willow the most based on consistently high browsing scores,
# while black spruce tends to have lower browsing intensity in comparison.

# black ash is not shown on the figure likely because it had too few observations
# making it unreliable for inclusion in the visualization.


# Moose-Vehicle Collisions
# Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests",
                 "Western_Forests","EasternHyperOceanicBarrens",
                 "Maritime_Barrens","Avalon_Forests",
                 "StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020,
                         human_pop,
                         study_sites)


# Question 25a
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

# Question 25b
coll_merge <- left_join(moose_2020,
                        moose_coll2,
                        by = "Ecoregion")


# Question 26
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions",
     main = "Moose Density vs Collisions (2020)")

# There appears to be a positive relationship between moose density
# and the number of vehicle collisions, with higher density regions
# generally experiencing more collisions.

# One region appears as a potential outlier with unusually high collisions for 
#its low density


# Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)


# Question 28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Person",
     main = "Collisions per Capita vs Human Population")


# Question 29
# Regions with smaller human populations often show higher collisions per capita,
# likely due to relatively high moose densities and increased road crossings in rural areas.

