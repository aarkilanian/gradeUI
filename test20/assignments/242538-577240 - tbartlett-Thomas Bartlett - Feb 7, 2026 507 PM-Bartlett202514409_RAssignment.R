# Title: Biology 1002 R Assignment
# Author: Thomas Bartlett
# Date: 07-02-2026

# Set working directory ---------------------------
setwd("C:/Users/Thoma/OneDrive/Documents/BIOL1002_RAssignment")
install.packages("dplyr")

#####Part 1: Moose Populations in Newfoundland#####

# Question 1 --------------
library(dplyr)

# Question 2
Moosedata <- read.csv("MoosePopulation.csv")

# Question 3
View(Moosedata)
na.omit(Moosedata)
moose_clean <- na.omit(Moosedata)

# Question 4
Moosedata <- select(Moosedata,Ecoregion,Year,Area,Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5A
min(Moosedata$Year)
# Answer: 1904

# Question 5B
max(moose_sel$Estimated_Moose_Pop)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", 
ylab = "Moose per sq km", 
main = "Moose density in Newfoundland ecoregions over time")

# Question 8A
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8B
plot(moose_west$Year,moose_west$MooseDensity,type = "l",xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western Forests Over Time")

# Question 9A
moose_2020 <- filter(moosedata2, Year == 2020)

# Question 9B
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Question 9C
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#####Part 2: Tree Sapling Study#####

# Question 11A
saplings <- read.csv("SaplingStudy.csv")

# Question 11B
sap_clean <- na.omit(saplings)

# Question 12A
sap_reg_browse <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  +     print()

# Questions 12B
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# Comment: Northern_Peninsula_Forests has the highest average browsing,
# StraightOfBelleIsleBarrens has the lowest average browsing.

# Question 13A
sap_reg_height <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageHeight = mean(Height))
print(sap_reg_height)

# Question 13B
sap_reg_height_low <- sap_reg_height %>%
  + filter(AverageHeight < 20)
print(sap_reg_height_low)
# Comment: Northern_Peninsula_Forests and Western_Forests have heights below
# 20cm and are severely browsed by moose.

# Question 14A
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 14B
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

# Comment: Black_Ash has the highest AverageBrowsing score and Black_Spruce
# has the lowest.

# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

# Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing intensity",
        main = "Average moose browsing on Balsam Fir per ecoregion",
        col = "red",
        cex.names = 0.6)

# Question 17A
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

# Question 17B
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing intensity",
        main = "Average moose browsing on Black Spruce by ecoregion",
        col = "red",
        cex.names = 0.6)

# Question 17C
# Comment: Black spruce shows lower browsing averages than Balsam Fir
# This provides strong evidence that moose prefer Balsam fir over Black spruce.

# Question 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Question 20A
# Comment: The sapling dataset is not evenly distribute, some ecoregions and
# species have sampled more than others, this can cause misrepresented data.

# Question 20B
# Comment: Its important because uneven sampling can cause wrong interpretations
# of the data.

#####Part III: Creating and Joining Datasets#####

# Question 21A
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Question 21B
moose_sap <- left_join(moose_2020b, sap_clean,
  by = "Ecoregion",
  relationship = "many-to-many")

# Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
  AvgDensity = mean(MooseDensity))
  print(sum_spe_browse)

# Question 23A
# Comment: It supports his hypothesis. At lower moose density, browsing differs
# among each species. At higher density it becomes more more uniform among each
# of the species.
  
# Question 23B
# Comment: Moose tend to favour Black Ash, Willow, and Alder the most, while
# they browse Black spruce the least.

# Question 23C
# Comment: Black ash is not shown because of too few observations, it's averages 
# are unreliable.
  
# Question 24
  collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
  human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
  study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                   "Long_Range_Barrens","Central_Forests","Western_Forests",
                   "EasternHyperOceanicBarrens","Maritime_Barrens",
                   "Avalon_Forests","StraitOfBelleIsleBarrens")
  moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25A
  moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
  
# Question 25B
  coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")

# Question 26A
  plot(coll_merge$MooseDensity, coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of Moose-Vehicle Collisions (2020)",
  main = "Moose Density vs Vehicle Collisions")
  
# Question 26B
# Comment: The higher moose density corresponds to higher collisons reported,
# Avalon_Forests is an outlier because of its huge population.
  
# Question 27
  coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# Question 28
  plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita,
  xlab = "Human Population",ylab = "Moose Collisions per Capita",
  main = "Moose-Vehicle Collisions per Capita vs Human Population")
  
# Question 29
# Comment: collisions per capita seem to be higher in areas with smaller human
# populations, it proves theres a higher number of collisions reported in rural
# areas. This trend matches up with what I know of Newfoundland because moose
# tend to have much larger populations/families in rural areas of Newfoundland,
# although it's a lower human population, more collisions happen because of the
# amount of moose present in these rural areas.