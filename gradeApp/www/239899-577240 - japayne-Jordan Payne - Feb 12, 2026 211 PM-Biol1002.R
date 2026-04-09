#Jordan Payne 
#Question 1 
install.packages("dplyr")
library(dplyr)
#Question 2 
moosedata <- read.csv("MoosePopulation.csv")
#Question 3
moose_clean <- na.omit(moosedata)
#Question 4 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5 (a) 
year_min<-min(moose_sel$Year)
#The year is 1904
#Question 5(b)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
#The moose max population is 41250 L
#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7(a) 
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")
#Question 8 (a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Question 8(b)
plot(moose_west$Year,moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose Density",
     main = "Moose Density Over Time in Western Forests")
#Question 9 (a)
moose_2020 <- filter(moosedata2, Year == "2020")
 #Question 9(b) 
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#Question 9(c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Question 11 (a)
saplings <- read.csv("SaplingStudy.csv")
#Question 11 (b)
sap_clean <- na.omit(saplings)
#Question 12 (a)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_browse = mean(BrowsingScore)) %>%
  print()
#Question 12(b)
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(mean_browse))
avg_browse_reg
# Highest average browsing: (Northern_peninsula_forest (4.57))
# Lowest average browsing: (StraitOfBelleIsleBarrens (1))
#Question 13 (a)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
#Question 13(b)
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions with average height less than 20 cm:
# Northern_Peninsula_Forests and Western_Forests
#Question 14 (a)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#Question 14(b)
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_spe
# Highest browsing species: (Black_Ash (5.00))
# Lowest browsing species: (Black_Spruce (2.33))
#Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
  #Question 16 
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
#Question 17 (a)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print() 
#Question 17 (b)
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
#Question 17 (c)
fir_reg_browse
spruce_reg_browse
# Black Spruce has lower browsing intensity than Balsam Fir across most ecoregions.
# This suggests that moose prefer Balsam Fir over Black Spruce.
#Question 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# No, the number of saplings counted was not the same across ecoregions.
# Some regions had many more sampled trees than others.
#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# No, the same number of saplings were not counted for each species.
# Some species (Balsam_Fir(11)) had many more individuals sampled than others (Black_Ash (1))
#Question 20 (a)
# The SaplingStudy dataset is not evenly distributed.
# Some ecoregions and species have more samples than others.
# For example, Balsam_Fir has more trees counted, while Black_Ash only has one.
# Also, some ecoregions have many trees sampled and others have very few.
#Question 20 (b)
# It is important to recognize bias because it can affect how we interpret the results.
# If some species or regions have more samples, it might make them seem more important
# even if that is not actually true.
#Question 21 (a)
colnames(moose_clean)
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
#Question 21 (b)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    MeanBrowsing = mean(BrowsingScore),
    MeanMooseDensity = mean(MooseDensity)
  ) %>%
  print()
#Question 23 (a) 
# The figure shows that at low moose density, browsing is higher on certain preferred species.
# As moose density increases, browsing scores increase across more species, suggesting they become more generalist.
#Question 23 (b)
# Willow appears to be the most favoured species because it consistently has the highest browsing scores.
# Black Spruce and White Birch appear to be browsed the least compared to the other species.
#Question 23 (c)
# Black Ash is not shown in the figure.
# This is likely because there were too few Black Ash samples in the dataset to properly represent it.
#Question 24
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
#Question 25 (a)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
#Question 25(b)
coll_merge <- left_join(moose_2020, moose_coll2,
                      by = "Ecoregion")
#Question 26 (a)
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions",
     pch = 19)
#Question 26 (b)
# The graph shows that as moose density increases, the number of collisions also seems to increase.
# Avalon_Forests stands out as a possible outlier because it has a much higher number of collisions compared to other regions.
#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita))
#Question 28
plot(coll_merge_per_capita$coll_per_capita,
     coll_merge_per_capita$human_pop,
     ylab = "Human Population",
     xlab = "Collisions Per Person",
     main = "Collisions Per Capita vs Human Population",
     pch = 19)
#Question 29
# It looks like areas with lower human populations have higher collisions per person.
# This makes sense because there are probably more moose in those areas and fewer people, so the collision rate per person ends up being higher.
