# STEP 1: Install and load dplyr package
# Only run this line once to install
install.packages("dplyr")  
library(dplyr)

# STEP 2: Load the Moose Population dataset
moosedata <- read.csv("C:/Users/Noah/Downloads/Moosepop.csv")

# STEP 3: Clean the data by removing missing values
moose_clean <- na.omit(moosedata)
 
# STEP 4: keep needed columbs 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# STEP 5a: Find the earliest year recorded
year_min <- min(moose_sel$Year)

# STEP 5b: Find the largest recorded moose population
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# STEP 6: moose density (population ÷ area)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# STEP 7: Visualize over time
plot(
  moosedata2$Year, moosedata2$MooseDensity,
  xlab = "Year",
  ylab = "Moose per sq km",
  main = "Moose density in Newfoundland ecoregions over time"
  )

# STEP 8a: Filter to only observe Western_Forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# STEP 8b: Plot density changes over time for Western_Forests
plot(moose_west$Year, moose_west$MooseDensity,
  type = "l",
  xlab = "Year",
  ylab = "Moose per sq km",
  main = "Moose Density Over Time in Western Forests")

# STEP 9a: year 2020
moose_2020 <- filter(moosedata2, Year == 2020)

# STEP 9b: high moose density
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# STEP 9c: highest to lowest density
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# STEP 10: Pipes
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# STEP 11a: Sapling data
saplings <- read.csv("C:/Users/Noah/Downloads/SaplingStudy.csv")

# STEP 11b: Clear missing data
sap_clean <- na.omit(saplings)

# STEP 12a: Moose browsing preaure 
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# STEP 12b: Most and least browsed regions
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# STEP 13a: Average sapling height by region
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

# STEP 13b: Regions that had saplings shorter than 20 cm
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

# STEP 14a: Average browsing score per sapling species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# STEP 14b: Most to least browsed species
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

# STEP 15: Group by region, balsam fir focus
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore)) %>%
  print()

# STEP 16: Plot Balsam Fir 
barplot(
  fir_reg_browse$AvgBrowsing,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Balsam Fir Browsing Intensity by Ecoregion",
  col = "forestgreen",
  cex.names = 0.6
)

# STEP 17a: repeat, Black Spruce
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore)) %>%
  print()

# STEP 17b: Bar chart for Black Spruce
barplot(
  spruce_reg_browse$AvgBrowsing,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Black Spruce Browsing Intensity by Ecoregion",
  col = "darkblue",
  cex.names = 0.6
)

# STEP 17c: Comment — Black Spruce is browsed less overall than Balsam Fir.

# STEP 18: Count saplings per region
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# STEP 19: Count saplings per species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# STEP 20a: Note on dataset distribution
# Some tree species and ecoregions are more prominant in the data sets then others are,which could mean uneven sampling

# STEP 20b: Importance of recognizing bias
# Ignoring bias can end up leading to incorrect conclusions, about how moose browse is different in species

# STEP 21a: Data for 2020 and add density column
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# STEP 21b: Join with sapling data 
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

# STEP 22: Summarize average browsing and moose density by species and region
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()

# STEP 23: ggplot to visualize browsing vs density
install.packages("ggplot2")

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
    x = "Average Moose Density",
    y = "Average Browsing Score"
  )

# Interpretation:
# At low moose densities, browsing is more selective; at higher densities, it becomes more uniform.
# Moose prefer Balsam Fir, and browse Black Spruce not as often.
# One species may not appear due to missing data or lack of overlap with moose density data.

# ---- Moose-Vehicle Collisions ----

# STEP 24: Create dataset with provided vectors
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c(
  "North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens",
  "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens",
  "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens"
)

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# STEP 25a: Match Ecoregion name
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

# STEP 25b: Join with moose_2020b data
coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")

# STEP 26a: Scatterplot of MooseDensity vs Collisions
plot(
  coll_merge$MooseDensity, coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Moose-Vehicle Collisions (2020)",
  main = "Moose Density vs Vehicle Collisions"
)

# STEP 26b: Notes
# Higher moose density is generally linked with more collisions,
# though some regions are clear outliers.

# STEP 27: Add collisions per capita column
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# STEP 28: Plot collisions per capita against human population
plot(
  coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  log = "x",  # Use log scale for x-axis
  xlab = "Human Population (log scale)",
  ylab = "Collisions Per Capita",
  main = "Moose Collisions Per Capita vs Human Population",
  pch = 16,
  col = "darkred",
  cex = 1.3
)

# STEP 29: Interpretation
# Collisions per capita are higher in areas with small human populations.
# This makes sense since moose habitats often overlap with roads in rural areas.
