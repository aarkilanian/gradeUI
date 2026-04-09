# BIOL 1002 R Assignment
#  Hassan Alahmad
# 202414388
install.packages("dplyr")

library(dplyr)
getwd()

# Part I: Moose Populations (Q1-Q10)

View(moosedata)
moose_clean <- na.omit(moosedata)

moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

year_min <- min(moose_sel$Year)
year_min
# Q5a: The oldest observation year is 1904.

moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
# Q5b: The highest Estimated_Moose_Pop is 41250.

moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")

moose_2020 <- filter(moosedata2, Year == 2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Part II: Tree Sapling Study (Q11-Q20)

saplings <- read.csv("SaplingStudy.csv")

sap_clean <- na.omit(saplings)

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
# Q12: The ecoregion with the highest average browsing is Northern_Peninsula_Forests.
# The ecoregion with the lowest average browsing is StraitOfBelleIsleBarrens.

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Q13: Ecoregions with average sapling height below 20 cm include
# Western_Forests and Northern_Peninsula_Forests.

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
# Q14: The species with the highest average browsing is Black_Ash.
# The species with the lowest average browsing is Black_Spruce.

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing intensity",
        main = "Balsam Fir browsing by ecoregion",
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
        ylab = "Average browsing intensity",
        main = "Black Spruce browsing by ecoregion",
        col = "steelblue",
        cex.names = 0.6)

# Q17: Balsam Fir generally shows higher browsing intensity than Black Spruce
# across most ecoregions, suggesting moose prefer browsing Balsam Fir.

sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Q20a: The Sapling Study dataset is not evenly distributed because some ecoregions
# and tree species have many more sampled saplings than others.

# Q20b: Recognizing sampling bias is important because uneven representation
# can affect averages and lead to incorrect conclusions about browsing patterns.

# Part III: Joining Datasets (Q21-Q29)

moose_2020b <- moose_clean %>%
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

# Q23a: The figure provides some support for the hypothesis.
# At low moose densities, browsing appears more selective, while at higher
# densities browsing becomes more uniform across species.

# Q23b: Moose appear to favor species such as Balsam Fir and Black Ash,
# while species like Black Spruce tend to have lower browsing intensity.

# Q23c: Some species are not shown in the figure because they were not present
# or not sampled in all ecoregions, so averages could not be calculated.

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Q25: The join fails at first because the site names are under 'study_sites' in moose_coll
# but under 'Ecoregion' in the moose dataset, so the column names don't match.

moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")

plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose density",
     ylab = "Number of collisions (2020)",
     main = "Moose density vs moose-vehicle collisions")
# Q26: There is no strong linear relationship between moose density and
# total collisions, and some ecoregions appear to be outliers.

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

coll_merge_per_capita

plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human population",
     ylab = "Collisions per capita",
     main = "Moose collisions per capita vs human population")

# Q29: Smaller human populations tend to show higher collisions per capita
# because even a moderate number of collisions represents a larger
# proportion of the population, which is consistent with rural regions
# in Newfoundland.

