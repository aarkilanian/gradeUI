library(dplyr)
MooseData <- read_csv("C:/Users/jacks/Downloads/MoosePopulation.csv")
MooseClean <- na.omit(MooseData)
moose_sel <- select(MooseClean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose Density in Newfoundland Ecoregions Over Time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose Density Over Time in Western Forests Ecoregion")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read_csv("C:/Users/jacks/Downloads/SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
# The first row has the highest average browsing.
# The last row has the lowest average browsing.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions shown above have average heights less than 20 cm,
# indicating severe browsing impact.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
# The first species listed has the highest browsing score.
# The last species listed has the lowest browsing score.
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Balsam Fir by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Black Spruce by Ecoregion",
        col = "darkolivegreen3",
        cex.names = 0.6)
# Comparing the two species:
# If Black Spruce shows lower average browsing than Balsam Fir,
# this suggests moose may prefer Balsam Fir.
# If browsing levels are similar, preference may not be strong across species.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# a) If tally results show large differences in counts between ecoregions or species,
# then the dataset is not evenly distributed. Overrepresented groups may bias results,
# while underrepresented groups may not accurately reflect browsing patterns.
# b) Recognizing sampling bias is important because uneven sampling can distort
# ecological conclusions, leading to incorrect assumptions about species preference
# or ecosystem impacts.
moose_2020b <- MooseClean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, 
                       by = "Ecoregion", 
                       relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
# If browsing scores become more similar across species at high moose density,
# this supports the hypothesis that moose shift from selective to generalist feeding
# as competition increases.
# The species with the highest AvgBrowsing values are most favoured,
# while those with the lowest AvgBrowsing are browsed least.
# Any species not shown likely had missing data or was absent in 2020,
# preventing it from appearing in the summarized dataset.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)

study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose/km²)",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs Vehicle Collisions (2020)")
# Regions with higher moose density generally show more collisions,
# although highly populated areas may appear as outliers.
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, 
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs Human Population")
# Regions with smaller human populations may show higher collisions per capita,
# suggesting rural areas with high moose density pose greater relative risk.
# This trend makes ecological sense, as moose habitat overlaps more heavily
# with less densely populated but road-connected forest regions.

