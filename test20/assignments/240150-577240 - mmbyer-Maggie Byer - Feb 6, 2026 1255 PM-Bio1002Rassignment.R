install.packages("dplyr")
library(dplyr)
View(moosedata)
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, 
                    Ecoregion, 
                    Year, 
                    Area, 
                    Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, 
                     MooseDensity = Estimated_Moose_Pop / Area)
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density Over Time in Western Forests")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, 
                               desc(MooseDensity))
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
# Highest average browsing: first ecoregion in table
# Lowest average browsing: last ecoregion in table
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions with average sapling height below 20 cm are considered severely browsed
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
# Species with highest browsing: first species in table
# Species with lowest browsing: last species in table
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing Intensity by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
# Black Spruce generally shows lower browsing intensity than Balsam Fir across most ecoregions
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# The SaplingStudy dataset is not evenly distributed, as some ecoregions and tree species
# have noticeably more samples than others, which may bias browsing estimates.

# Recognizing bias is important because uneven sampling can lead to incorrect conclusions
# about species preference or browsing pressure across regions.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")\
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
# The figure provides some support for the hypothesis, as browsing appears more selective
# at lower moose densities and becomes more uniform across species at higher densities.

# Moose appear to favour more palatable species such as Balsam Fir, which shows higher
# browsing scores, while species such as Black Spruce are browsed less.

# One sapling species is not shown because it may have insufficient data or identical
# browsing values, causing it to be excluded or overlap completely in the plot.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions")
# There is a general positive relationship where higher moose density corresponds
# to more collisions, although some ecoregions appear as outliers.
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Moose Collisions Per Capita vs Human Population")
# Collisions per capita tend to be higher in regions with smaller human populations,
# which makes sense as rural areas often overlap more with moose habitat.

# This trend aligns with expectations, since moose are more abundant in less developed
# regions where roadways pass through natural habitat.

