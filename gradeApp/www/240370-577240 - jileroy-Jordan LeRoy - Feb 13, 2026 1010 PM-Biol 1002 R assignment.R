# Title: R Assignment BIO 1002
# Author: Jordan LeRoy 202511140
# Date: 13-02-2026

# Load libraries needed ---------------------------
install.packages("dplyr")
library("dplyr")
library("readr")

# Set working directory ---------------------------
setwd(Bio_1002R)

# Load data ---------------------------
library(readr)
MoosePopulation <- read_csv("MoosePopulation.csv")
View(MoosePopulation)
library(readr)
SaplingStudy <- read_csv("SaplingStudy.csv")
View(SaplingStudy)

# part I- Moose Population 

# question 3: omit missing data for moose population
moose_clean <- na.omit(MoosePopulation)

# question 4: select columns of interest 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# question 5: oldest year and highest moose population
#5 a) oldest observation
year_min <- min(moose_sel$Year)
    # oldest observation 1904
#5 b) highest estimated moose population
moose_max <- max(moose_sel$Estimated_Moose_Pop)
    # highest estimated moose population 41250

# question 6: calculate moose density= population/area- 41250
moosedata2 <- mutate(moose_sel, 
                     MooseDensity = Estimated_Moose_Pop / Area)
    # Northern Moose Density = 2.67 moose/km²


# question 7: plot moose density over time (all data)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

# question 8: western forests
#8 a) filter western forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#8 b) line graph of moose density over time 
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density over time in Western Forests ecoregion")

# question 9: focus on 2020 and high density
#9 a) filter for year 2020 
moose_2020 <- filter(moosedata2, Year == 2020)
#9 b) moose density greater than 2.0
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#9 c) sort by density descending 
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# question 10- addition of pipes
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
# In 2020, Northern had the highest moose density (2.67 moose/km²),
# while Central had the lowest density among regions above 2.0 (2.01 moose/km²).

# Part II: Tree Sapling study 

# question 11: Load Data and remove NAs
#11 a) load dataset
saplings <- read.csv("SaplingStudy.csv")
#11 b) remove missing values 
sap_clean <- na.omit(saplings)

# question 12: Browsing pressure by ecoregion 
#12 a) mean browsing score per ecoregion using pipes 
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#12 b) Arrange from highest to lowest browsing 
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
# Northern_Peninsula_Forests had the highest average browsing score (4.57),
# while StraitOfBelleIsleBarrens had the lowest (1.0).

# question 13: Average tree height by ecoregion 
#13 a) Mean height per ecoregion 
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
#13 b) Filter for severely browsed sites (<20cm)
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Northern_Peninsula_Forests and Western_Forests have average heights below 20 cm
# and are considered severely browsed.

# question 14: browsing by species 
#14 a) mean browsing score per species 
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#14 b) Arrange from highest to lowest browsing 
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
# Black_Ash had the highest average browsing score (5.0),
# while Black_Spruce had the lowest (2.33).

# question 15: Balsam Fir browsing by ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# question 16: Bar graph for Balsam Fir browsing 
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Average Balsam Fir browsing by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

#question 17: Black Spruce browsing 
#17 a) mean browsing by ecoregion 
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#17 b)
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Average Black Spruce browsing by ecoregion",
        col = "darkolivegreen",
        cex.names = 0.6)
# Balsam Fir generally experiences higher browsing intensity than Black Spruce
# across most ecoregions, suggesting moose preferentially browse Balsam Fir.


# question 18: Number of saplings per ecoregion 
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# question 19: Number of saplings per species 
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#question 20: sampling bias comments 
# The dataset is not evenly distributed. North_Shore_Forests (8 samples)
# and Balsam_Fir (11 samples) are overrepresented, while
# StraitOfBelleIsleBarrens (1 sample) and Black_Ash (1 sample)
# are underrepresented.

# question 21
#21 a) filter 2020+ calculate density
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
#21 b)
moose_sap <- left_join(moose_2020b, sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")
#Moose and sapling datasets successfully combined by ecoregion, 
#enabling analysis of browsing intensity relative to moose density.

# question 22:average browsing + average density by species and ecoregion 
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
# Ecoregions with higher moose densities (above ~2.0 moose/km²) generally have 
#high browsing scores (4–5) across multiple species, while regions with lower 
#densities (0.5–1.0 moose/km²) show more variable or lower browsing scores.

# question 23: interpretation 
# 23 a)
# There is evidence supporting the hypothesis. In ecoregions with lower moose
# densities (around 0.5–1.0 moose/km²), browsing scores are generally lower
# or more variable. In regions with higher densities (above ~2.0 moose/km²),
# browsing scores are consistently high (often 4–5) across multiple species,
# suggesting reduced selectivity at high population density.

#23 b)
# Moose appear to favour Alder, Black_Ash, and Willow, as they show the highest
# average browsing scores across multiple ecoregions. Black_Spruce has
# consistently lower browsing scores, suggesting it is less preferred.

#23 c)
# Black_Ash may not appear clearly on the figure because it was
# underrepresented in the dataset, with very few samples.

# question 24: create collision dataset 
#run vectors 
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

# question 25: rename and join 
#25 a) rename 
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
#25 b) join 
coll_merge <- left_join(moose_2020, moose_coll2,
                        by = "Ecoregion")

# question 26: scatterplot 
#26 a) 
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions",
     main = "Moose Density vs Collisions (2020)")
#26 b)
# The scatterplot shows a positive relationship between moose density and
# the number of collisions. Ecoregions with higher moose densities generally
# experience more collisions. However, Avalon_Forests stands out as an outlier
# with very high collision numbers, likely due to its very large human
# population and road network rather than density alone.

# question 27: collisions per capita 
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

#question 28: plot per capita vs human population 
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population")

# question 29: interpretation
# The plot suggests that collisions per capita tend to be higher in regions
# with smaller human populations. In sparsely populated areas, even a moderate
# number of collisions results in a higher per-person rate, while highly
# populated regions have lower per capita collision rates.





