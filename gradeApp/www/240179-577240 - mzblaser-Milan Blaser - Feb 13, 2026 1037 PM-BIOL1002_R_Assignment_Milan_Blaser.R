# Question 1: Install dplyr (only needed once)
# install.packages("dplyr")

# Question 2: Load dplyr
library(dplyr)

# Question 2 (read.csv): Import MoosePopulation.csv, name it moosedata
moosedata <- read.csv("MoosePopulation.csv")

# Question 3: View data + remove missing values using na.omit(), save as moose_clean
View(moosedata)
moose_clean <- na.omit(moosedata)

# Question 4: Select columns of interest, save as moose_sel
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5a: Oldest observation (min Year), save as year_min
year_min <- min(moose_sel$Year)

# Question 5b: Highest Estimated_Moose_Pop, save as moose_max
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Question 6: Create MooseDensity column using mutate(), save as moosedata2
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7: Plot MooseDensity vs Year (basic plot)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8a: Filter Western_Forests only, save as moose_west
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8b: Line graph (type="l") for Western_Forests
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")

# Question 9: Filter for Year 2020, save as moose_2020
moose_2020 <- filter(moosedata2, Year == 2020)

# Q9: Filter MooseDensity > 2.0, save as moose_2020_high
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Q9: Arrange in descending order of MooseDensity, save as moose_2020_high_byD
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10: Repeat Q9 using pipes, print output, save final as moosefinal
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
# Question 11a: Load SaplingStudy.csv, name it saplings
saplings <- read.csv("SaplingStudy.csv")

# Q11: Remove NAs, save as sap_clean
sap_clean <- na.omit(saplings)

# Question 12a: Mean BrowsingScore by Ecoregion, save as sap_reg_browse
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

# Q12b: Arrange by decreasing AverageBrowsing, save as avg_browse_reg
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# # Comment: highest = Northern_Peninsula_Forests (4.57) ; lowest =  StraitOfBelleIsleBarrens (1.00)  (fill in after you look at avg_browse_reg)

# Question 13: Mean Height by Ecoregion, save as sap_reg_height
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>%
  print()

# Q13: Ecoregions with AverageHeight < 20, save as sap_reg_height_low
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

# # Comment: Regions with avg height < 20 cm are: ___

# Question 14a: Mean BrowsingScore by Species, save as sap_spe_browse
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

# Q14b: Arrange decreasing AvgBrowsing, save as avg_browse_spe
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AvgBrowsing))

# # Comment: highest species = Black_Ash (5.00) ; lowest species = Black_Spruce (2.33)

# Question 15: Balsam_Fir browsing by Ecoregion, save as fir_reg_browse
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE))

# Question 16: Barplot for Balsam Fir browsing by ecoregion
barplot(fir_reg_browse$AvgBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir browsing intensity by ecoregion",
        cex.names = 0.6)

# Question 17: Repeat for Black_Spruce, save as spruce_reg_browse
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE))

# Q17 barplot
barplot(spruce_reg_browse$AvgBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce browsing intensity by ecoregion",
        cex.names = 0.6)

# # Comment (1-2 sentences): Compare Black Spruce vs Balsam Fir browsing across ecoregions.
# Overall, Balsam_Fir experiences consistently moderate to high browsing across most ecoregions,
# whereas Black_Spruce shows lower browsing intensity and even zero browsing in some barrens regions.
# This suggests moose may prefer Balsam_Fir over Black_Spruce in several areas.

# Question 18: tally by Ecoregion, save as sap_reg_tally
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# Question 19: tally by Species, save as sap_spe_tally
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Question 20a: # Comment about whether dataset is evenly distributed (use sap_reg_tally & sap_spe_tally)
# The dataset is not evenly distributed across ecoregions or species.
# Some regions and species have many observations, while others (such as Black_Ash and StraitOfBelleIsleBarrens)
# have very few samples, which may influence the reliability of averages.

# Question 20b: # Comment (1-2 sentences) why bias matters
# Uneven sample sizes can bias results because averages based on very small sample sizes
# may not accurately represent true ecological patterns.

# Question 21a: From moose_clean, filter Year==2020 and compute MooseDensity, save as moose_2020b
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Q21b: left_join with sap_clean by Ecoregion, save as moose_sap
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

# Question 22: Avg browsing + Avg density by Species and Ecoregion, save as sum_spe_browse
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()

# Question 23: OPTIONAL ggplot code is provided in assignment
# # Comment answers (1-2 sentences each):
# # - Evidence supporting hypothesis?
# There appears to be a general positive relationship between moose density and browsing intensity,
# as regions with higher moose densities tend to have higher average browsing scores.

# # - Which species favoured most/least?
# Willow and Alder appear to be among the most heavily browsed species,
# while Black_Spruce tends to have lower browsing intensity.
# # - Which species missing and why?
# Black_Ash appears only in one region, which limits our ability to compare it across densities.

# Question 24: Create moose_coll data frame
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens",
                 "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                 "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25: Rename study_sites column to match Ecoregion, save as moose_coll2
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

# Join with moose_2020 (from Q9 Part I) — note: moose_2020 was from moosedata2
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Question 26a: Scatterplot MooseDensity vs collisions2020
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose/km^2)",
     ylab = "Collisions in 2020",
     main = "Moose density vs moose-vehicle collisions (2020)")

# Q26b: # Comment (1-2 sentences): trends/outliers
# There is a general trend where regions with higher moose density tend to have more vehicle collisions.
# However, regions with large human populations may show higher collision numbers even if moose density is moderate,
# suggesting that both moose density and human population influence collision rates.

# Question 27: Create coll_per_capita, save as coll_merge_per_capita
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# Question 28: Scatterplot coll_per_capita vs human_pop
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human population",
     ylab = "Collisions per capita",
     main = "Collisions per capita vs human population (2020)")

# Question 29: # Comment (1-2 sentences): describe trends + whether it makes sense
# Collisions per capita do not increase proportionally with human population size.
# Smaller regions may show higher per capita collision rates, suggesting local moose density
# and road distribution play important roles.