# Title: Bio 1002 r assignment 1
# Author: Makayla Murphy
# Date: 10-02-2026

# Load libraries needed ---------------------------
library(dplyr)

# Set working directory ---------------------------
setwd("~/rscripts")

# Load data ---------------------------
moosedata <- read.csv("MoosePopulation.csv", header = TRUE)

# remove rows from dataset with missing values
moose_clean <- na.omit(moosedata)

# simply data to only include relevant data
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# find oldest year in dataset
year_min <- min(moose_sel$Year)

# find highest moose population
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# calcuate moose density for each ecoregion and put it in a new column called MooseDensity
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# plot date in a line graph
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# dataset showing only Western_Forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density over time in Western Forests",
     # type = "l" is used to make it a line graph
     type = "l")

# filter data from moosedata2 to find only the year 2020
moose_2020 <- filter(moosedata2, Year == 2020)

# filter data for high density 2.0 moose/km
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# sort the MooseDensity column in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Pipes %>% allow you to connect one line of code to the next allowing you to save to one dataset instead of multiple like above
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#
# --------------------------------------------------------------------------------------------
#

# Load data ---------------------------
saplings <- read.csv("SaplingStudy.csv", header = TRUE)

# remove rows from dataset with missing values (NA)
sap_clean <- na.omit(saplings)

# Pipes %>% allow you to connect one line of code to the next allowing you to save to one dataset instead of multiple like above
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
  print(sap_reg_browse)

# rearrange dataset in order of descreasing average browsing score
# Region with highest average browsing scores is: Northern_Peninsula_Forests
# Region with lowest average browsing scores is: StraitOfBelleIsleBarrens
avg_browse_reg <- sap_reg_browse %>%  
  arrange(desc(AverageBrowsing))
  print(avg_browse_reg)

# calculate the mean tree Height for each group   
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height))
  print(sap_reg_height)
  
# find average heights less than 20 cm
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)
  print(sap_reg_height_low)
  # ecoregions have average heights less than 20 cm are:
  # Northern_Peninsula_Forests          19.9
  # Western_Forests                     18.9  
  
# calculate the mean tree BrowsingScore for each group
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
  print(sap_spe_browse)
  
# Rearrange the data according to decreasing mean browsing score
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
  print(avg_browse_spe)
  # species with the highest browsing score: Black_Ash - 5
  # species with the lowest browsing score: Black_Spruce - 2.33
  
# filter the Species column for only Balsam_Fir and sort by Ecoregion to determine mean moose BrowsingScore
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
  print(fir_reg_browse)
  
# plot data using bar plot with axis names, color change and x-axis size change
barplot(fir_reg_browse$AverageBrowsing, 
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Average Balsam Fir Browsing by Ecoregion",
  col = "forestgreen", 
  cex.names = 0.5)
  
# filter the Species column for only Black_Spruce and sort by Ecoregion to determine mean moose BrowsingScore
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
  print(spruce_reg_browse)

# plot data using bar plot with axis names, color change and x-axis size change
barplot(spruce_reg_browse$AverageBrowsing, 
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Average Black Spruce Browsing by Ecoregion",
  col = "darkgreen", 
  cex.names = 0.4)

# The Black Spruce has a lower average Browsing intesity across all ecoregions compared to
# the Balsam Fir. This suggests moose prefer Balsam Fir over Black Spruce.

# determine how many trees were counted in each Ecoregion
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally()
  print(sap_reg_tally)

# determine how many individual trees were counted in for each Species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()
  print(sap_spe_tally)
  
# the dataset is not evening distributed, as some of the ecoregions/species have more sampled saplings than others.
# for example Black_Ash only has 1 sapling compared to the Balsam_Fir which has 11
  
# Its important to recognize bias because having uneven sampling can provide incorrectd ecological patterns. 
# Over represented regions or species can exaggerate tends, which leads to inaccurate conclusions.

# using moose_clean dataset, select only year = 2020 and added a new column call MooseDensity
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# joining moose_2020b with sap_clean matching rows by common ecoregion column
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

# calculate the average browsing score and average moose density for each species within each ecoregion
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore), AvgDesnity = mean(MooseDensity)) %>%
  print()

# Yes, the figure supports the researchers hypothesis. At lower moose densities, moose browsing is concentrated
# on a few species, while at higher densities the browsing intensity increases across almost all species. This
# suggests that feeding choices are more generic with high competition.

# Moose appear to favour willow the most, because it shows the highest browsing scores. They browse black spruce
# the least, this species shows the lowest average browsing scores.

# The sapling species that is not showing on the figure is Black_Ash, likly because there were no black_ash
# saplings in the sampled ecoregions used to calculate the averages.

# create 3 new vectors then create a dataset using the new vectors
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# rename a column so we can join 2 datasets together. the question said to use rename_with() but that doesn't work,
# so I had to use rename().
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

# merge moose_2020 and moose_coll2 now that they have both have a column named "Ecoregion"
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# make a scatterplot showing Moose Density vs Collisions
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions",
     main = "Moose Density vs Collisions")

# There tends to be a positive relationship between Moose Density and the number of collisions. With higher moose density,
# there tends to be more collisions. There is a noticeable outlier with an extreme ammount of collisions compared to the
# moose density.

# Create a new column called coll_per_capita that is equal to collisions2020 divided by human_pop using mutate()
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# create a scatterplot showing Collisions Per Capita vs Human Population
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs Human Population")

# Collisions per capita tend to be higher in regions with lower human populations. This makes sense because many
# rural areas have more moose and less traffic control. This increases collision risk relative to population size