library(dplyr)
na.omit(moosedata)
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
# Highest browsing: (check first row after running)
# Lowest browsing: (check last row after running)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions with average height < 20 cm are severely browsed
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
# Highest browsing species: (check first row)
# Lowest browsing species: (check last row)
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(Averagebarplot(fir_reg_browse$AverageBrowsing,
                           names.arg = fir_reg_browse$Ecoregion,
                           xlab = "Ecoregion",
                           ylab = "Average Browsing Score",
                           main = "Balsam Fir browsing intensity by ecoregion",
                           col = "forestgreen",
                           cex.names = 0.6)
            Browsing = mean(BrowsingScore))
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce browsing intensity by ecoregion",
        col = "darkgreen",
        cex.names = 0.6)

# Compare the heights of bars between this graph and Balsam Fir graph
# to determine which species has higher browsing
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# a) Check sap_reg_tally and sap_spe_tally results
# If numbers are unequal, dataset is not evenly distributed
# Some ecoregions or species may be overrepresented or underrepresented

# b) Bias is important because it can lead to incorrect conclusions.
# If some species or regions are sampled more than others,
# the results may not accurately reflect real moose browsing patterns.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

# Add your answers as comments after viewing the plot
# Moose may browse more selectively at low density and more uniformly at high density.
# Species with highest browsing score are most favored; lowest are least favored.
# Species missing may not occur in dataset or had insufficient data.
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

coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions",
     main = "Relationship between Moose Density and Collisions")

# Add comment after viewing plot
# Higher moose density may be associated with more collisions.
# Any points far from others may be outliers.
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population")
# Add comment after viewing plot
# Regions with smaller human populations may have higher collisions per capita.
# This may occur because moose density is high but human population is low.