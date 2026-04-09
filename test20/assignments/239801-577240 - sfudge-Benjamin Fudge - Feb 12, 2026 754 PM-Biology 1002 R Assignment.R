# Part 1
# Question 1
install.packages("dplyr")
library("dplyr")
# Question 2
# I imported via the "Import Dataset" option in the environment tab.
# Question 3
moose_clean <- na.omit(moosedata)
# Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
colnames(moose_sel)
dim(moose_sel)
# Question 5
# a.
year_min <- min(moose_sel$Year)
year_min
# Oldest observation year: 1904
# b.
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
# Highest Estimated_Moose_Pop: 41250
# Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)
# Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
# Question 8
# a.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)
# b.
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density over time in Western Forests")
# Question 9
# a.
moose_2020 <- filter(moosedata2, Year == 2020)
View(moose_2020)
# b.
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
View(moose_2020_high)
# c.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
View(moose_2020_high_byD)
# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Part 2
# Question 11
# a.
saplings <- read.csv("SaplingStudy.csv")
saplings <- SaplingStudy
# b.
sap_clean <- na.omit(saplings)
# Question 12
# a.
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Moose browsing pressure is highest in Northern_Peninsula_Forests and Western_Forests, and lowest in StraitOfBelleIsleBarrens and Maritime_Barrens.
# This shows browsing intensity varies substantially across ecoregions.
# b. 
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

avg_browse_reg
# Highest browsing: Northern_Peninsula_Forests (4.57)
# Lowest browsing: StraitOfBelleIsleBarrens (1.00)
# Question 13
# a. 
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
# Average tree height varies across ecoregions.
# Highest average height: Avalon_Forests (32.4 cm)
# Lowest average height: Western_Forests (18.9 cm)
# b. 
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions with average height < 20 cm (severely browsed):
# Northern_Peninsula_Forests (19.9 cm)
# Western_Forests (18.9 cm)
# Question 14
# a.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Average browsing varies substantially among species.
# Black_Ash has the highest mean browsing score (5.00), while Black_Spruce has the lowest (2.33), indicating strong species-specific browsing preference.
# b.
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_spe
# Highest browsing species: Black_Ash (5.00)
# Lowest browsing species: Black_Spruce (2.33)
# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Highest Balsam Fir browsing: North_Shore_Forests (4.5)
# Lowest Balsam Fir browsing: Long_Range_Barrens (1.0)
# Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Balsam Fir Browsing Score",
        main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
# Question 17
# a.
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# b.
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Black Spruce Browsing Score",
        main = "Average Black Spruce Browsing Intensity by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
# c.
# Black Spruce generally shows lower browsing intensity than Balsam Fir across most ecoregions.
# Balsam Fir tends to have higher average browsing scores, indicating moose prefer Balsam Fir over Black Spruce.
# Question 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
# No, sapling counts vary by ecoregion (e.g., North_Shore_Forests = 8, StraitOfBelleIsleBarrens = 1), showing that the number of trees sampled was not equal across regions.
# question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# No, sapling counts vary by species (e.g., Balsam_Fir = 11, Black_Ash = 1), showing that some species were sampled more frequently than others.
# Question 20
# a.
# The SaplingStudy dataset is not evenly distributed across ecoregions and species.
# Some ecoregions (e.g., North_Shore_Forests) have more saplings sampled than others, and certain species such as Balsam_Fir appear more frequently than species like Black_Ash.
# This suggests potential overrepresentation and underrepresentation in the dataset.
# b.
# Recognizing bias in ecological datasets is important because uneven sampling can distort interpretations of browsing patterns and species preference.
# If certain regions or species are overrepresented, conclusions about moose browsing intensity may not accurately reflect the true ecological system.

# Part 3
# Question 21
# a.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
# b.
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
# Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AverageBrowsing = mean(BrowsingScore),
    AverageMooseDensity = mean(MooseDensity)
  ) %>%
  print()
# Question 23
install.packages("ggplot2")
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AverageMooseDensity, 
                           y = AverageBrowsing, 
                           color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# a.
# There is partial support for the hypothesis. At lower moose densities (0.5–1.0), browsing scores vary widely among species (from about 0 to 4), indicating selective feeding.
# At higher densities (2.0–2.7), most species show high browsing scores (3.5–5), suggesting more generalized browsing under greater competition.
# b.
# Moose favour Willow and Alder the most, as they consistently show high browsing scores (4–5).
# Black_Spruce is browsed the least, often showing low scores (0–3) across densities.
# c.
# Black_Ash is minimally represented because it has very few observations in the dataset, which reduces its visibility and influence in the density–browsing pattern shown.

# Part 4
# Question 24
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
print(moose_coll)
# Question 25
# a.
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
print(moose_coll2)
# b.
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")
print(coll_merge)
# Question 26
# a.
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions")
# b.
# There is a positive relationship between moose density and collisions.
# At low densities (0–0.6), collisions are generally below 20, while at higher densities (2.0–2.6), collisions increase to roughly 40–110, with Avalon_Forests appearing as a high-collision outlier.
# Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)
# Northern_Peninsula_Forests has the highest collisions per capita (0.005), while Long_Range_Barrens has the lowest (0.00035), indicating substantial regional differences when adjusting for human population size.
# Question 28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs Human Population")
# Question 29
# There is a general negative relationship between human population and collisions per capita.
# Regions with smaller populations (e.g., 12,000 people) have higher per-capita rates (about 0.005), while highly populated regions like Avalon_Forests (270,000 people) have much lower rates (about 0.0004).
# This makes ecological sense because rural, low-population areas overlap more with moose habitat, increasing collision risk relative to the number of people.