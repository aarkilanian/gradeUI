library(dplyr)
moosedata <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/main/BIOL1002-data/MoosePopulation.csv")
saplings <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/main/BIOL1002-data/SaplingStudy.csv")

moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moosedata$Year)
year_min
moose_max <- max(moosedata$Estimated_Moose_Pop, na.rm = TRUE)
moose_max
moosedata2 <- mutate(moose_sel, 
                     MooseDensity = Estimated_Moose_Pop / Area)
head(moosedata2)
plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")
moose_west <- filter(moosedata2, 
                     Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western Forests Over Time")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
head(saplings)
sap_clean <- na.omit(saplings)
sum(is.na(sap_clean))
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_reg
# Western_Forests had the highest average browsing
# Avalon_Forests had the lowest average browsing
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Western_Forests and Northern_Peninsula_Forests have average heights less than 20 cm
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_spe
 # # Balsam_Fir had the highest browsing score
# Black_Spruce had the lowest browsing score
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Balsam Fir Browsing by Ecoregion",
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
        ylab = "Average Browsing Intensity",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
fir_reg_browse
spruce_reg_browse
# # Balsam Fir generally has higher average browsing intensity than Black Spruce across most ecoregions.
# This suggests that moose show a stronger browsing preference for Balsam Fir compared to Black Spruce.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
# The same number of saplings were counted in each ecoregion.
# No, the number of saplings counted varies across ecoregions.
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# The same number of saplings were counted for each species.
# No, the number of saplings counted differs among species.
sap_reg_tally
sap_spe_tally
# The SaplingStudy dataset is not evenly distributed across ecoregions.
# North Shore and Northern Peninsula have more sampled trees, while Strait of Belle Isle and Maritime Barrens are underrepresented.
# Recognizing sampling bias is important because uneven representation can influence results and lead to misleading conclusions.
# If some regions or species are overrepresented, it may distort our interpretation of moose browsing patterns.
moose_clean
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
sap_clean
moose_sap <- left_join(moose_2020b,
                       sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")
head(moose_sap)
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgMooseDensity = mean(MooseDensity)) %>%
  print()
colnames(sum_spe_browse)
library(ggplot2)

ggplot(sum_spe_browse,
       aes(x = AvgMooseDensity, y = AvgBrowsing, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# The results support the researchers’ hypothesis.
# At lower moose densities, browsing varies more among species, suggesting selectivity, while at higher densities browsing intensity is high across multiple species, indicating less selective feeding.
# Moose appear to favour Willow and Balsam Fir, which show consistently high browsing scores.
# Black Spruce is browsed the least, showing lower average browsing intensity across ecoregions.
# Black Ash is not clearly represented in the figure because it was sampled only once in a single ecoregion.
# With limited observations, it does not provide enough data for meaningful comparison across regions.
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
moose_coll
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020,
                        moose_coll2,
                        by = "Ecoregion")
head(coll_merge)
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions (2020)",
     pch = 19)
# There appears to be a positive relationship between moose density and the number of collisions, with higher density regions generally having more collisions.
# Avalon Forests appears to be an outlier, with disproportionately high collisions compared to its moose density.
coll_merge
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita))
coll_merge_per_capita
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs Human Population",
     pch = 19)
# There is a negative relationship between human population and collisions per capita, with smaller populations experiencing higher collision rates per person.
# This makes sense because rural regions have more moose habitat and roadways passing through natural areas, increasing the likelihood of moose-vehicle collisions.





