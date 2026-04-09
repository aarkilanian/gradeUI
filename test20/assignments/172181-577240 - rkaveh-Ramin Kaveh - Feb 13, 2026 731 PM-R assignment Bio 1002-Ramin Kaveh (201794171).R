library(dplyr)
# BIOL 1002 – Winter 2026 – R Assignment
# Moose Ecology & Sapling Browsing Study
# Name: Ramin Kaveh
# Student Number: 201794171

# Part I: Moose Population Data

# Q1–2: Load the data
moose <- read.csv("MoosePopulation.csv")

# Q3: Remove rows with any NA values
moose_clean <- na.omit(moose)

# Q4: Select only the columns we need
moose_sel <- moose_clean %>%
  select(Ecoregion, Year, Area, Estimated_Moose_Pop)

# Q5: Minimum year and maximum moose population
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

cat("Earliest year in dataset:", year_min, "\n")
cat("Highest estimated moose population:", moose_max, "\n\n")

# Q6: Create new column – moose density (moose per unit area)
moosedata2 <- moose_sel %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Q7: Scatter plot of moose density over time (all ecoregions)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time",
     pch = 19, col = "blue")

# Q8: Line plot for Western_Forests only (sorted by year)
moose_west <- moosedata2 %>%
  filter(Ecoregion == "Western_Forests") %>%
  arrange(Year)  # important for line to connect properly

plot(moose_west$Year, moose_west$MooseDensity,
     type = "b",   # both points and lines
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density over time in Western_Forests",
     col = "darkgreen", lwd = 2)

# Q9–10: 2020 data, density > 2.0, sorted descending
moose_2020_high <- moosedata2 %>%
  filter(Year == 2020 & MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity))

print("Ecoregions with moose density > 2.0 in 2020 (sorted highest first):")
print(moose_2020_high)

# Part II: Tree Sapling Study

# Q11: Load and clean sapling data
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

# Q12: Average browsing score by ecoregion (highest to lowest)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  arrange(desc(AverageBrowsing))

print("Average browsing score by ecoregion (highest first):")
print(sap_reg_browse)

# Q13: Average height by ecoregion, filter < 20 cm
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageHeight = mean(Height))

sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)

print("Ecoregions with average sapling height < 20 cm:")
print(sap_reg_height_low)

# Q14: Average browsing by species (highest to lowest)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  arrange(desc(AverageBrowsing))

print("Average browsing score by species (highest first):")
print(sap_spe_browse)

# Q15: Average browsing on Balsam_Fir by ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))

print("Balsam Fir average browsing by ecoregion:")
print(fir_reg_browse)

# Q16: Barplot for Balsam Fir
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Balsam Fir browsing by ecoregion",
        col = "forestgreen",
        cex.names = 0.7, las = 2)

# Q17: Black Spruce browsing by ecoregion + barplot
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore))

print("Black Spruce average browsing by ecoregion:")
print(spruce_reg_browse)

barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Black Spruce browsing by ecoregion",
        col = "pink",
        cex.names = 0.7, las = 2)

# Note: Black Spruce generally has lower browsing than Balsam Fir in most ecoregions.

# Q18: Number of saplings per ecoregion
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally(name = "Number_of_Saplings")

print("Number of saplings sampled per ecoregion:")
print(sap_reg_tally)

# Q19: Number of saplings per species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally(name = "Number_of_Saplings")

print("Number of saplings sampled per species:")
print(sap_spe_tally)

# Q20: Written answer
# a) Sampling is uneven (some ecoregions/species have more observations).
# b) Uneven sampling can bias conclusions about moose preference.

# Part III: Joining Datasets

# Q21: 2020 moose data + density + join to saplings
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")

# Q22: Average browsing and moose density by species and ecoregion
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity  = mean(MooseDensity, na.rm = TRUE),
    .groups = "drop"
  )

print("Average browsing and moose density by species and ecoregion:")
print(sum_spe_browse)

# Q23: Written answers
# 1. At low moose density browsing is more selective; at high density it increases across species.
# 2. Moose prefer alder and willow; black spruce is browsed least.
# 3. Black Ash only appears in one ecoregion (Western_Forests), so trend not visible.

# Moose–Vehicle Collisions

# Q24: Create collisions data frame
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens",
                 "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens",
                 "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")

moose_coll <- data.frame(Ecoregion = study_sites,
                         collisions2020 = collisions2020,
                         human_pop = human_pop)

# Q25: Join with 2020 moose data
coll_merge <- left_join(moose_coll, moose_2020b, by = "Ecoregion")

# Q26: Plot density vs collisions
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose density (moose per km²)",
     ylab = "Moose–vehicle collisions (2020)",
     main = "Moose density vs vehicle collisions",
     pch = 19, col = "yellow")

# Note: Higher density often means more collisions, but Avalon is an outlier (high human pop/traffic).

# Q27: Collisions per capita
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

print("Collisions per capita (2020):")
print(coll_merge_per_capita %>% select(Ecoregion, coll_per_capita))

# Q28: Plot per capita vs human population
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human population",
     ylab = "Collisions per capita",
     main = "Collisions per capita vs human population (2020)",
     pch = 19, col = "purple")

# Q29: Written answer
# Per-capita collisions are higher in smaller-population ecoregions because moose risk is spread across fewer people.
# High-population areas have more total collisions but lower per-person risk due to "dilution" by larger traffic volume.

