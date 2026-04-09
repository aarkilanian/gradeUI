# Title: R script for MoosePopulation.csv Bio 1002 R Assignment Part 1
# Author: Sara Young
# Date: 8-02-2026

# Question 1:
install.packages("dplyr")
library(dplyr)


# Question 2:
data1 <- read.csv(file = "MoosePopulation.csv")
moosedata <- read.csv("MoosePopulation.csv")
MoosePopulation <- read.csv(file = "MoosePopulation.csv")

# QUestion 3:
View(moosedata)
moose_clean <- na.omit(MoosePopulation)

# Question 4:
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5a:
year_min <- min(moose_sel$Year)


# Question 5b:
moose_max <- max(moose_sel$Estimated_Moose_Pop) 

# Question 6:
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7:
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")

# Question 8a:
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8b:
plot(moose_west$Year, moose_west$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Western Forests over time", type = "1")

# Question 9a:
moose_2020 <- filter(moosedata2, Year == 2020)
View(moose_2020)

# Question 9b:
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Question 9c:
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD

# Question 10:
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
                
# Title: R Script for SaplingStudy.csv Bio 1002 R Assignment
# Author: Sara Young
# Date: 10-02-2026
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
getwd()
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>% 
  arrange(desc(AverageBrowsing))
print(avg_browse_reg)
# Highest browsing = 4.57
# Lowest browsing = 1
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)
# These ecoregions have average heights under 20cm: Northern Peninsula Forests, Western Forests
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
print(avg_browse_spe)
# Black Ash (5) = highest browsing species
# Black Spruce (2.33) = lowest browsing species
fir_reg_browse <- sap_clean %>% 
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
fir_reg_browse <- sap_clean %>%
  mutate(Species = trimws(Species)) %>%
  filter(grepl("balsam", Species, ignore.case = TRUE)) %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore))
print(fir_reg_browse)
barplot(fir_reg_browse$MeanBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing intensity",
        main = "Balsam Fir: Average Browsing by Ecoregion",
        las = 2, 
        cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore))
print(spruce_reg_browse)
barplot(spruce_reg_browse$MeanBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing intensity", main = "Black Spruce: Average Browsing by Ecoregion",
        las = 2,
        cex.names = 0.6)
# Black Spruce has higher brosing than Balsam Fir in most ecoregions, 
# but both show the highest browsing in the North Shore Forests. 
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally()
print(sap_reg_tally)
# The dataset is not evenly distributed because some ecoregions have far more sampled trees than others.
# This means browsing averages may be more reliable in high-sample regions than low-sample regions.
# Bias matters because uneven sampling can distort patterns and make some species/ecoregions look more or less browsed than they truly are.
# This can lead to incorrect ecological conclusions.

# Title: R script for "Creating and Joined Datasets" Bio 1002 R Assignment Part 3
# Author: Sara Young
# Date: 11-02-2026

library(dplyr)
install.packages("ggplot2")
# Question 21a:
names(moose_clean)
moose_2020 <- moose_clean %>%
  filter(Year == 2020)
moose_2020b <- moose_2020
print(moose_2020b)

# Question 21b:
names(moose_2020b)
names(sap_clean)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
print(moose_sap)

# Question 22:
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE), 
    (AvgDensity = mean(MoosePopulation, na.rm = TRUE))

print(sum_spe_browse)
  
# Question 23:
plot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color = Species)) + 
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Browsing Intensity Across Moose Density by Species",
    x = "Average Moose Density", 
    y = "Average Browsing Score"
  )

# Question 23a:
# Yes, there is evidence supporting the researchers hypothesis. At low moose density, browsingscores vary more between species, while at higher moose density browsing scores are generally high across most sepcies, suggesting less selective feeding 

# Question 23b: 
# Moose appear to favour species with the highest average browsing scores, especially Willow. They browse the least on the species with the lowest scores, which include Black Ash and sometimes White Birch at low density.

# Question 23c
# A species is missing because it likely had no observations in the merged 2020 dataset, so it couldn't be plotted. 

# Title: R script for Moose-vehicle Collisions Bio 1002 R Assignment Part 4
# Author: Sara Young
# Date: 11-02-2026

# Question 24:
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c(
  "North_Shore_Forests",
  "Northern_Peninsula_Forests",
  "Long_Range_Barrens",
  "Central_Newfoundland",
  "South_Coast",
  "Avalon",
  "Notre_Dame_Bay",
  "Northeast_Coast",
  "West_Coast")
)

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
print(moose_coll)

# Question 25:
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
names(moose_coll2)
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")
print(coll_merge)

# Question 26:
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions",
     main = "Moose Density vs Vehicle Collisions")
length(coll_merge$MooseDensity)
length(coll_merge$collisions2020)
# Collisions generally increase as moose density increases.
# One or two regions may appear higher than expected and could be outliers.

# Question 27:
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)

# Question 28: 
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita", 
     main = "Collisions Per Capita vs Human Population")

# Question 29:
# Regions with smaller human populations tend to have higher collisions per capita
# This makes sense because rural areas have more moose habitat and fewer roads designed to reduce collisions.

