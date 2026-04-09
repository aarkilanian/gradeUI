title: "BIOL 1002 R Assignment"
author: "Emma Howell"
date: "2026-02-13"


install.packages("dplyr")

# Part 1:Moose Populations in Newfoundland
# Question 1
library(dplyr)

# Question 2
moosedata <- read.csv("MoosePopulation.csv")

# Question 3
moose_clean <- na.omit(moosedata)

# Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5
# a. What is the oldest observation in the dataset?
year_min <- min(moose_sel$Year)
  print(year_min)
# 1904
# b. What is the highest ‘Estimated_Moose_Pop’ recorded?
moose_max <- max(moose_sel$Estimated_Moose_Pop)
  print(moose_max)
# 41250

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7
# a. Plot of changes in moose density over time
plot(moosedata2$Year, moosedata2$MooseDensity, 
  xlab = "year", 
  ylab = "Moose per sq km", 
  main = "Moose density in Newfoundland Ecoregions Over Time")

# Question 8
# a.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# b.
plot(moose_west$Year, moose_west$MooseDensity,
  type = "l",
  xlab = "Year",
  ylab = "Moose per sq km",
  main = "Moose Density Over Time in Western Forests")

# Question 9
# a.
moose_2020 <- filter(moosedata2,Year == 2020)
# b.
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
# c.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()



# Part 2: Tree Sampling Study
# Question 11
# a.
saplings <- read.csv("SaplingStudy.csv")
# b.
sap_clean <- na.omit(saplings)

# Question 12
# a.
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# b.
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
# Highest average browsing: Northern Peninsula Forests
# Lowest average browsing: Strait Of Belle Isle Barrens


# Question 13
# a.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
# b.
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions with average height < 20 cm: Northern Peninsula Forests, and Western Forests

# Question 14
# a.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()
#b.
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(MeanBrowsing)) %>%
  print()
# Highest browsing: Black ash
# Lowest browsing: Black spruce

# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 16
barplot(fir_reg_browse$MeanBrowsing,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Average Balsam Fir Browsing by Ecoregion",
  col = "green",
  cex.names = 0.6)

# Question 17
# a.
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()
# b.
barplot(spruce_reg_browse$MeanBrowsing,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Average Black Spruce Browsing by Ecoregion",
  col = "green",
  cex.names = 0.6)
# c.
# Black spruce generally has less browsing than balsam fir across most ecoregions.
# This suggests that moose prefer to browse balsam fir rather than black spruce.

# Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

# Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Question 20
# a.
# The SaplingStudy dataset is not evenly distributed since some ecoregions
# have more trees sampled then others.
# Most of the tree species samples are around 8 with some tree species are overrepresented
# (like balsam fir with 11) while others are underrepresented (like black ash with 1).
# b.
# Recognizing bias in ecological datasets is important because it can lead to unreliable conclusions being drawn
# which directly affects the accuracy of the datasets and can misrepresent the ecosystems being studied.



# **Part 3: Creating and Joining Datasets**
# Question 21
# a.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
# b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', multiple = "all")

# Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
  AvgDensity = mean(MooseDensity)) %>%
  print()

# Question 23
# a.
# There is moderate evidence supporting the researchers' hypothesis.
# At lower moose densities, browsing does appear more selective and at higher densities,
# browsing scores become more similar across species, suggesting less selectivity and more generalist feeding.
# b.
# Moose appear to prefer willow and alder trees based on the average browsing scores. 
# Black spruce is browsed the least.
# c.
# Black ash is not shown on the figure because it only occurs in one ecoregion,
# so it does not provide enough variation across density levels and is therefore omitted from the plot

# Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests", 
                 "EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25
# a. 
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
# b.
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion", multiple = "all")

# Question 26
# a.
plot(coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of Collisions (2020)",
  main = "Moose Density vs Number of Collisions")
# b.
# There appears to be a positive trend between moose density and the number of collisions.
# Avalon_Forests seems to be an outlier since it has a much higher number of collisions then the other areas of similar moose density.

# Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# Question 28
plot(coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  xlab = "Human Population",
  ylab = "Collisions Per Person",
  main = "Collisions Per Capita vs Human Population")

# Question 29
# Based on the Collisions Per Capita vs Human Population scatterplot regions with smaller
# human populations tend to have higher collisions per capita.
# This makes sense because in Newfoundland rural areas tend to have a lower human population
# and have more moose habitat overlap with roads and
# commonly frequented areas compared to highly populated urban areas, increasing individual risk despite lower total human population.