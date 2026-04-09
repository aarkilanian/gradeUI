library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
year_min
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
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
print(avg_browse_reg)
# The ecoregion with the highest average moose browsing is North_Shore_Forests, with an average BrowsingScore around 4.5–5.
# The ecoregion with the lowest average moose browsing is EasternHyperOceanicBarrens, with an average BrowsingScore around 1.5–2.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)
print(sap_reg_height_low)
# Ecoregions with average heights less than 20 cm (severely browsed) are Western_Forests (due to Black_Ash at 11.2 cm) and some trees in Central_Forests.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
print(avg_browse_spe)
# The species with the highest average browsing is Willow, with many scores of 5.
# The species with the lowest average browsing is Black_Spruce, with many scores at 0–2.
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
print(fir_reg_browse)
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Balsam Fir by Ecoregion",
        col = "forestgreen",
spruce_reg_browse <- sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore))
print(spruce_reg_browse)
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Black Spruce by Ecoregion",
        col = "darkblue",
        cex.names = 0.6)
# Black Spruce generally has lower browsing scores across ecoregions compared to Balsam Fir, which shows moderate browsing (around 2–4).
# This indicates moose prefer Balsam Fir over Black Spruce in most regions.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# The SaplingStudy dataset is not evenly distributed. Some ecoregions, like North_Shore_Forests and Northern_Peninsula_Forests, are heavily sampled, while others like StraitOfBelleIsleBarrens have very few observations.
# Recognizing bias is important because uneven sampling can misrepresent browsing patterns, leading to incorrect conclusions about moose preferences.
# Q21a: Filter moose_clean for 2020 and calculate density
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
# Q21b: Join with sap_clean
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
# Group by Species and Ecoregion and calculate averages
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()
# Q23a: Evidence for selective vs generalist browsing
# At low moose densities, browsing is selective (moose favor Willow and Alder).
# At high densities, browsing becomes more uniform across species, supporting the hypothesis.
# Q23b: Which species are most/least favored
# Moose favor Willow and Alder the most, and Black Spruce the least.
# Q23c: Species not shown
# Black_Ash is not shown because it has very few observations in the dataset.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c(
  "North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens",
  "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
  "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens"
)
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# Q25a: Rename study_sites to Ecoregion
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
# Q25b: Join with moose_2020
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
# Q26a: Scatterplot
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions",
     main = "Moose Density vs Vehicle Collisions")
# Q26b: Observation
# There is a general trend that higher moose density leads to more collisions,
# though a few ecoregions (like Avalon_Forests) are outliers with very high collisions relative to density.
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population")
# Moose collisions per capita tend to be higher in smaller human populations,
# even if absolute collisions are lower. Larger populations have more collisions overall,
# but per person, risk is lower.