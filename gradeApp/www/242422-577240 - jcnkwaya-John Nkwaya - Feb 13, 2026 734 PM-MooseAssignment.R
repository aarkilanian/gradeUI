############################################
# Name: john nkwaya
# Student Number: 202514031
# Course: biology 1001
# Assignment: Moose & Sapling Study
############################################

# -----------------------------
# PART I: MOOSE POPULATIONS
# -----------------------------

# Question 1:
# Load the dplyr package so its functions are available.
library(dplyr)

# Question 2:
# Import the MoosePopulation.csv dataset and save it as moosedata.
moosedata <- read.csv("MoosePopulation.csv")

# Question 3:
# Remove rows with missing values and save the cleaned dataset as moose_clean.
moose_clean <- na.omit(moosedata)

# Question 4:
# Select only the columns Ecoregion, Year, Area, and Estimated_Moose_Pop.
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5a:
# What is the oldest year in the dataset?
year_min <- min(moose_sel$Year)

# Answer:
# The oldest observation in the dataset is given by the minimum year value.

# Question 5b:
# What is the highest estimated moose population recorded?
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Answer:
# The highest recorded moose population is given by the maximum value of Estimated_Moose_Pop.

# Question 6:
# Calculate moose density (population divided by area).
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Answer:
# Moose density standardizes population size by land area, allowing fair comparison between ecoregions.

# Question 7:
# Plot moose density over time.
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

# Answer:
# The plot shows how moose density has changed over time, generally increasing as populations expanded.

# Question 8a:
# Filter for Western Forests only.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8b:
# Plot moose density over time for Western Forests.
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")

# Answer:
# Moose density in the Western Forests ecoregion varies over time with an overall increasing trend.

# Question 9a:
# Filter for the year 2020.
moose_2020 <- filter(moosedata2, Year == 2020)

# Question 9b:
# Filter for high moose density (> 2.0).
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Question 9c:
# Arrange in descending order.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Answer:
# Ecoregions with moose density above 2.0 in 2020 are considered high-density regions.

# Question 10:
# Repeat using pipes.
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Answer:
# Pipes make the code easier to read and allow multiple steps to be combined into one workflow.

# -----------------------------
# PART II: TREE SAPLING STUDY
# -----------------------------

# Question 11a:
# Load the SaplingStudy dataset.
saplings <- read.csv("SaplingStudy.csv")

# Question 11b:
# Remove missing values.
sap_clean <- na.omit(saplings)

# Question 12a:
# Mean browsing score by ecoregion.
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Answer:
# Moose browsing pressure varies across ecoregions, indicating uneven browsing intensity.

# Question 12b:
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))

# Answer:
# The ecoregion with the highest average browsing has the most browsing pressure.
# The ecoregion with the lowest average browsing has the least browsing pressure.

# Question 13a:
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

# Question 13b:
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20) %>% print()

# Answer:
# Ecoregions with average sapling heights below 20 cm show evidence of heavy browsing.

# Question 14a:
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 14b:
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))

# Answer:
# The species with the highest average browsing score is the most preferred by moose.
# The species with the lowest average browsing score is the least preferred.

# Question 15:
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

# Question 16:
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

# Question 17a:
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

# Question 17b:
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)

# Question 17c:
# Black Spruce is generally browsed less than Balsam Fir, showing a species preference.

# Question 18:
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# Answer:
# Saplings were not sampled equally across all ecoregions.

# Question 19:
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Answer:
# Tree species were not sampled in equal numbers.

# Question 20a:
# The dataset is not evenly distributed; some ecoregions and species are overrepresented.

# Question 20b:
# Recognizing bias is important because uneven sampling can lead to incorrect conclusions.

# -----------------------------
# PART III: JOINING DATASETS
# -----------------------------

# Question 21a:
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Question 21b:
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

# Question 22:
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()

# Question 23a:
# The results support the hypothesis: browsing is more selective at low densities and more general at high densities.

# Question 23b:
# Moose favor highly palatable species such as Balsam Fir and browse Black Spruce the least.

# Question 23c:
# One species is not shown because it does not appear in enough ecoregions for comparison.

# -----------------------------
# PART IV: MOOSE COLLISIONS
# -----------------------------

# Question 24:
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25a:
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)

# Question 25b:
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Question 26a:
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Collisions (2020)",
     main = "Moose Density vs Vehicle Collisions")

# Question 26b:
# There is a positive trend where higher moose density tends to result in more collisions.
# Some ecoregions may act as outliers.

# Question 27:
coll_merge_per_capita <- mutate(coll_merge,
                                coll_per_capita = collisions2020 / human_pop)

# Question 28:
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population")

# Question 29:
# Regions with smaller human populations can still have high collision rates per person,
# which makes sense because moose are more common in rural areas.

