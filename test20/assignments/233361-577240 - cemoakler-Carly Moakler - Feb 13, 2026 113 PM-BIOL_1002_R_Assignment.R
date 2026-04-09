# ============================================================
# Moose Population and Sapling Browsing Analysis
# ============================================================

# Install necessary packages
install.packages(c('dplyr', 'readr', 'tidyr'))
install.packages("ggplot2")

# Load required libraries
library(dplyr)

# -----------------------------
# Question 1:
# Load moose population data and clean it
#
# Answer:
# The moose population data is loaded and cleaned by removing NA values.
# The data is then filtered to select relevant columns such as Ecoregion,
# Year, Area, and Estimated Moose Population.
# -----------------------------

moosedata <- read.csv("moosepopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# -----------------------------
# Question 2:
# Calculate the minimum and maximum Year and Moose Population
#
# Answer:
# The minimum Year and maximum Moose Population values are calculated
# from the filtered data.
# -----------------------------

year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# -----------------------------
# Question 3:
# Calculate Moose Density and create plot
#
# Answer:
# A new column 'MooseDensity' is added by dividing the Estimated Moose 
# Population by the Area. A plot is created to visualize Moose Density 
# over the years.
# -----------------------------

moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year", ylab = "Moose per sq km", 
     main = "Moose Density in Newfoundland Ecoregions Over Time")

# -----------------------------
# Question 4:
# Filter and arrange moose data for the year 2020 with high Moose Density
#
# Answer:
# The data is filtered for the year 2020 and only Ecoregions with Moose Density 
# greater than 2.0 are kept. The data is then arranged by descending Moose Density.
# -----------------------------

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# -----------------------------
# Question 5:
# Load sapling study data and clean it
#
# Answer:
# The sapling data is loaded and cleaned by removing NA values. 
# The data is grouped by Ecoregion to calculate the average browsing score 
# for each Ecoregion.
# -----------------------------

saplings <- read.csv("saplingstudy.csv")
sap_clean <- na.omit(saplings)

# -----------------------------
# Question 6:
# Calculate Average Browsing Score by Ecoregion
#
# Answer:
# The average browsing score for each Ecoregion is calculated and sorted in descending order.
# -----------------------------

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

print(sap_reg_browse)
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

print(avg_browse_reg)

# -----------------------------
# Question 7:
# Calculate Average Height by Ecoregion and filter for low height
#
# Answer:
# The average height of saplings is calculated for each Ecoregion. 
# Ecoregions with an average height of less than 20 are filtered.
# -----------------------------

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height))

print(sap_reg_height)

sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)

print(sap_reg_height_low)

# -----------------------------
# Question 8:
# Calculate Average Browsing Score by Species
#
# Answer:
# The average browsing score for each species is calculated and sorted.
# -----------------------------

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

print(sap_spe_browse)

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

print(avg_browse_spe)

# -----------------------------
# Question 9:
# Check and clean species names for Balsam Fir
#
# Answer:
# Various steps are taken to clean the species names in the sapling data,
# including trimming whitespace, handling case sensitivity, and removing extra spaces.
# -----------------------------

fir_reg_browse <- sap_clean %>%
  filter(Species == "BalsamFir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))

# Debugging steps for cleaning species names
fir_reg_browse <- sap_clean %>%
  filter(grepl("balsam", Species, ignore.case = TRUE)) %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))

print(fir_reg_browse)

# -----------------------------
# Question 10:
# Create bar plot for Average Browsing Score of Balsam Fir by Ecoregion
#
# Answer:
# A bar plot is created to show the average browsing score of Balsam Fir by Ecoregion.
# -----------------------------

barplot(
  fir_reg_browse$AverageBrowsing,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Average Moose Browsing on Balsam Fir by Ecoregion",
  col = "forestgreen",
  cex.names = 0.6
)

# -----------------------------
# Question 11:
# Create bar plot for Average Browsing Score of Black Spruce by Ecoregion
#
# Answer:
# A similar bar plot is created for Black Spruce species.
# -----------------------------

spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

print(spruce_reg_browse)

barplot(
  spruce_reg_browse$AverageBrowsing,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Average Moose Browsing on Black Spruce by Ecoregion",
  col = "darkolivegreen",
  cex.names = 0.6
)

# -----------------------------
# Question 20:
# Compared to Balsam Fir, Black Spruce generally shows lower browsing intensity
# across most ecoregions, suggesting moose prefer Balsam Fir.
# -----------------------------

# -----------------------------
# Question 21:
# Check the distribution of species and ecoregions in the sapling study
#
# Answer:
# We check how evenly the sapling study dataset is distributed by tallying 
# the number of records by Ecoregion and Species.
# -----------------------------

sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()

print(sap_spe_tally)

# -----------------------------
# Question 23:
# How does moose density relate to browsing intensity across species?
#
# Answer:
# The figure provides partial support for the researchers’ hypothesis.
# At lower moose densities, browsing appears more selective, while at higher
# densities browsing intensity becomes more similar across species.
# Moose appear to favour more palatable species such as Willow and Alder,
# while species like Black Spruce tend to experience lower browsing intensity.
# -----------------------------

# Merge moose density and sapling data
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity = mean(MooseDensity, na.rm = TRUE)
  )

print(sum_spe_browse)

# -----------------------------
# Question 26:
# Relationship between moose density and vehicle collisions
#
# Answer:
# There is a positive relationship between moose density and vehicle collisions,
# although some ecoregions appear as outliers.
# -----------------------------

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests",
                 