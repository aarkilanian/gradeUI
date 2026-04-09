library(dplyr)
install.packages("dplyr")
# Title: Moose Population and Sapling Browsing Analysis
# Author: Hannah Alguire
# Date: 13-02-2026

# -------------------------------------------------
# Question 1 
# -------------------------------------------------
library(dplyr)

# -------------------------------------------------
# Question 2
# -------------------------------------------------
moosedata <- read.csv("MoosePopulation.csv", strip.white = TRUE)

# -------------------------------------------------
# Question 3 
# -------------------------------------------------
moose_clean <- na.omit(moosedata)

# -------------------------------------------------
# Question 4 
# -------------------------------------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# -------------------------------------------------
# Question 5
# -------------------------------------------------
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# -------------------------------------------------
# Question 6 
# -------------------------------------------------
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# -------------------------------------------------
# Question 7 
# -------------------------------------------------
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")

# -------------------------------------------------
# Question 8 
# -------------------------------------------------
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western Forests Over Time")

# -------------------------------------------------
# Question 9 
# -------------------------------------------------
moose_2020 <- filter(moosedata2, Year == 2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# -------------------------------------------------
# Question 10 
# -------------------------------------------------
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# -------------------------------------------------
# Question 11 
# -------------------------------------------------
saplings <- read.csv("SaplingStudy.csv", strip.white = TRUE)

sap_clean <- na.omit(saplings)

# -------------------------------------------------
# Question 12 
# -------------------------------------------------
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# Highest browsing is likely the North Shore Forests
# Lowest browsing is likely the Eastern HyperOceanic Barrens

# -------------------------------------------------
# Question 13 
# -------------------------------------------------
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

# Ecoregions with AverageHeight < 20 cm are severely browsed

# -------------------------------------------------
# Question 14 
# -------------------------------------------------
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

# Highest browsing likely Willow
# Lowest browsing likely Black_Spruce

# -------------------------------------------------
# Question 15 
# -------------------------------------------------
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

# -------------------------------------------------
# Question 16 
# -------------------------------------------------
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

# -------------------------------------------------
# Question 17 
# -------------------------------------------------
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

# Black Spruce generally shows lower browsing than Balsam Fir,
# suggesting moose prefer Balsam Fir.

# -------------------------------------------------
# Question 18 
# -------------------------------------------------
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# -------------------------------------------------
# Question 19 
# -------------------------------------------------
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# -------------------------------------------------
# Question 20 
# -------------------------------------------------
# The dataset is not evenly distributed. Some ecoregions and species
# have more samples than others, which seems like bias interpretations.

# Recognizing bias is important because uneven sampling can mess up
# ecological trends and lead to the wrong conclusions.

# -------------------------------------------------
# Question 21
# -------------------------------------------------
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")

# -------------------------------------------------
# Question 22 
# -------------------------------------------------
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()

# -------------------------------------------------
# Question 23 
# -------------------------------------------------
# At low moose density, browsing is more selective.
# At higher densities, browsing is more uniform,
# which supports the researchers' hypothesis.

# Moose favor Willow and Balsam Fir most.
# Black Spruce is browsed the least.

# Black Ash is not shown because it was not widely sampled
# across all density gradients.

# -------------------------------------------------
# Question 24 
# -------------------------------------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# -------------------------------------------------
# Question 25 
# -------------------------------------------------
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_coll2, moose_2020,
                        by = "Ecoregion")

# -------------------------------------------------
# Question 26
# -------------------------------------------------
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions",
     main = "Moose Density vs Collisions")

# It appears to be a positive relationship:
# Higher moose density usually corresponds to more collisions.

# -------------------------------------------------
# Question 27 
# -------------------------------------------------
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# -------------------------------------------------
# Question 28 
# -------------------------------------------------
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population")

# -------------------------------------------------
# Question 29 
# -------------------------------------------------
# Regions with smaller human populations sometimes show
# higher collisions per capita. This makes sense because
# rural areas tend to overlap more with moose habitat, increasing the risk.







