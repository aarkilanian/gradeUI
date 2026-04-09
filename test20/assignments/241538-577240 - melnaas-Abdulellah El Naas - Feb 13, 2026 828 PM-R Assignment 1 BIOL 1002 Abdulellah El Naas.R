# R Assignment 1 BIOL 1002
# Abdulellah EL Naas
# Feb 13 2026

#Set Directory ------------------------------------

setwd("C:/Users/abade/OneDrive/Documents/Abdulellah R Assignment 1")

# Load libraries -----------------------------------
#install.packages("dplyr")
library(dplyr)

# Load data ----------------------------------------
#moosedata <- read.csv("MoosePopulation.csv")
#saplings <- read.csv("SaplingStudy.csv")

# Clean data ---------------------------------------
moose_clean <- na.omit(moosedata)
sap_clean <- na.omit(saplings)

# Moose population analysis ------------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year, na.rm = TRUE)      # Earliest year in dataset
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)  # Highest population
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Plot moose density over time ---------------------
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year", ylab = "Moose per sq km",
     main = "Moose Density Over Time", type = "p")
# Shows general decline in density over time with some fluctuations.

# Western Forests focus ----------------------------
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_west_ord <- moose_west[order(moose_west$Year), ]
plot(moose_west_ord$Year, moose_west_ord$MooseDensity, type = "l",
     xlab = "Year", ylab = "Moose per sq km",
     main = "Western Forests Moose Density")
# Western Forests show a gradual decline in moose density.

# 2020 data filtering ------------------------------
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
# High-density regions in 2020 have MooseDensity > 2.0.

# Sapling analysis ---------------------------------
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
# Regions at the top have the highest browsing pressure.

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE))
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)
# These regions show severe browsing impact (avg height < 20 cm).

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# Species at top are most browsed; bottom species least browsed.

# Balsam Fir browsing ------------------------------
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion", ylab = "Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion", cex.names = 0.6)
# Balsam Fir shows high browsing where moose densities are greater.

# Black Spruce browsing ----------------------------
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion", ylab = "Browsing Score",
        main = "Black Spruce Browsing by Ecoregion", cex.names = 0.6)
# Black Spruce experiences lower browsing than Balsam Fir.

# Sapling tallies ----------------------------------
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally()
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally()
# Dataset not evenly sampled; some ecoregions/species overrepresented.
# Recognizing this prevents biased results.

# Combine moose and sapling data -------------------
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
            AvgDensity = mean(MooseDensity, na.rm = TRUE), .groups = "drop")
# Positive relationship between MooseDensity and browsing supports hypothesis.
# Balsam Fir most preferred, Black Spruce least.
# Missing species likely not present in overlapping datasets.

# Moose-vehicle collision data ---------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests",
                 "Long_Range_Barrens", "Central_Forests", "Western_Forests",
                 "EasternHyperOceanicBarrens", "Maritime_Barrens",
                 "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Plot moose density vs collisions -----------------
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density", ylab = "Collisions (2020)",
     main = "Moose Density vs Vehicle Collisions")
# Higher moose density regions generally have more collisions.
# Avalon Forests appear as an outlier with many collisions due to high human pop.

# Per capita collision rates -----------------------
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population", ylab = "Collisions per Person",
     main = "Collisions per Capita vs Human Population")
# Smaller, rural populations show higher per-person collision rates.
# Large populations have low per-capita risk even with many total collisions.