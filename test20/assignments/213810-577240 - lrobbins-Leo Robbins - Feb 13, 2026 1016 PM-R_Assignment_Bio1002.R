# BIOL 1002 -Part I - Moose Populations in Newfoundland
# Leo Robbins 202226387
# 13/02/26
# QUESTION 1- Load libraries needed---------------------------------------------
library(dplyr)

# QUESTION 2 - Rename and import dataset----------------------------------------
moosedata <- read.csv("MoosePopulation (1).csv")

# QUESTION 3 - Clean data, remove NAs-------------------------------------------
moose_clean <- na.omit(moosedata)

# QUESTION 4 - Simplify data to the most relevant columns-----------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#QUESTION 5 Min and Max Calculations--------------------------------------------
#a) The oldest year is 1904.
year_min <- min(moose_clean$Year)

#b) The highest estimated moose population was 41250.
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#QUESTION 6 - Create a Column for Moose Density---------------------------------
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#QUESTION 7 - Plot Moose Density over Year--------------------------------------
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type = "l" ,
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# QUESTION 8 - Change in populations over time in the Western Forests Exoregion----
# a)- Create new dataset for just Western forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# b)- Plot how Moose Density changed over time in Western Forests
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "Year", 
     ylab = "Moose per km²", 
     main = "Moose density in Western Forests Ecoregion")

# QUESTION 9 - Ecoregions for just recent Years---------------------------------
# a) - Filter for the year 2020
moose_2020 <- filter(moosedata2, Year == 2020)

# b) - Filter for Densities above 2.0 moose/km^2
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# c - Arrange Moose Density in 2020 by descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# QUESTION 10 Pipe practice ----------------------------------------------------
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Part II: Tree Sapling Study
# QUESTION 11 Renaming and Cleaning dataset-------------------------------------
saplings <- read.csv("SaplingStudy (1).csv")
# b) - Clean dataset
sap_clean <- na.omit(saplings)

# QUESTION 12 Ecoregion Descriptives--------------------------------------------
# a) Mean browsing by ecoregion 
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# b) Highest and lowest browsing ecoregions
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# HIGHEST average browsing score is: Northern_Peninsula_Forests
# LOWEST average browsing score is: StraitOfBelleIsleBarrens 

# QUESTION 13 Difference in tree height across regions--------------------------
# a) Average tree height by ecoregion
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
# b) Ecoregions with average height < 20 cm
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions with average height less than 20 cm are: 
# Northern_Peninsula_Forests and Western_Forests   

# QUESTION 14 Browsing by species ----------------------------------------------
# a) Average browsing score by tree species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# b) Species with highest and lowest browsing
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()

# Species with HIGHEST average browsing score: Black_Ash
# Species with LOWEST average browsing score: Black_Spruce

# QUESTION 15 Balsam Fir browsing-----------------------------------------------
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# QUESTION 16 Barplot Balsam Fir------------------------------------------------
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing Intensity by Ecoregion",
        col = "blue",
        cex.names = 0.6,
        las = 2) 

# QUESTION 17 Barplot Balsam Fir-------------------------------
# a) Black Spruce browsing by ecoregion
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# b) Barplot of Black Spruce browsing by ecoregion 
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6,
        las = 2)

# c) Compare Black Spruce to Balsam Fir
# Balsam Fir has higher browsing scores than Black Spruce in most ecoregions.
# Black Spruce has zero browsing in EasternHyperOceanicBarrens and Maritime_Barrens,
# while Balsam Fir is browsed in every ecoregion.
# Both species have the highest browsing in North_Shore_Forests and
# Northern_Peninsula_Forests.

# QUESTION 18 Tree count by ecoregion-------------------------------------------
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally()

# QUESTION 19  Tree count by species--------------------------------------------
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()

# QUESTION 20 Is the dataset evenly distributed? -------------------------------
  # Ecoregion counts show:
  # - Most ecoregions have 5 samples, but:
  # - North_Shore_Forests (8) and Northern_Peninsula_Forests (7) are overrepresented
  #- Maritime_Barrens (3) and StraitOfBelleIsleBarrens (1) are underrepresented
  
  # Species counts show:
  # - Balsam_Fir (11) is overrepresented
  # - Black_Ash (1) is heavily underrepresented
  # - Other species range from 7-9 samples
  # The dataset is NOT evenly distributed. Some ecoregions and species have 
  # many more samples than others.
  
# Part III:  CREATING AND JOINING DATASETS

# QUESTION 21 Join moose & sapling ---------------------------------------------
# a) Prepare moose 2020 data for joining
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# b) Combine datasets by matching Ecoregion columns
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", 
relationship = "many-to-many")

# QUESTION 22 Summary browse/density--------------------------------------------
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgMooseDensity = mean(MooseDensity),
    .groups = "drop") %>%
  print()

# QUESTION 23 Evaluate Results -------------------------------------------------
# Is there evidence that supports the researchers' hypothesis? 
# Do moose show strong preferences at low density and shift to more generalist 
# browsing at higher density?

# Yes, the data supports the hypothesis. At low moose densities (0.5-1.0), 
# there is more variation in browsing scores between species. At high densities 
# (2.5-2.67), most species have high browsing scores (4-5),
# suggesting that moose become less selective.


# Which sapling specie(s) do moose favour the most? 
# Which do they browse the least?

# Alder and Balsam Fir are browsed the most, with scores reaching 5 in multiple 
# ecoregions. Black Spruce is browsed the least, with scores of 0-2
# in most ecoregions.


# Which sapling species is not shown on the figure and why?

# Black_Ash is not shown because it had very low sample size (only 1 tree total),
# so it was likely removed from the figure to avoid misleading conclusions 
# based on insufficient data.

# MOOSE COLLISIONS -------------------------------------------------------------

# QUESTION 24 Create moose-vehicle collisions dataset --------------------------
study_sites <- c("Avalon_Forests", "Central_Forests", 
                 "EasternHyperOceanicBarrens", 
                 "Long_Range_Barrens", "Maritime_Barrens", 
                 "North_Shore_Forests", 
                 "Northern_Peninsula_Forests", "StraitOfBelleIsleBarrens", 
                 "Western_Forests")

collisions2020 <- c(5, 12, 2, 3, 4, 18, 15, 1, 10)

human_pop <- c(50000, 15000, 500, 1000, 2000, 8000, 6000, 300, 12000)

moose_coll <- data.frame(study_sites, collisions2020, human_pop)

# QUESTION 25 Join collisions --------------------------------------------------
# a) 
moose_coll2 <- moose_coll %>%
rename(Ecoregion = study_sites)
# b)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
head(coll_merge)

# QUESTION 26 Plot density vs collisions ---------------------------------------
# a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (per km²)",
     ylab = "Number of Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs Vehicle Collisions by Ecoregion",
     pch = 16,
     col = "darkred")

# b) 
# There is a positive relationship - ecoregions with higher moose density 
# tend to have more collisions. North_Shore_Forests and Northern_Peninsula_Forests 
# have the highest density and highest collisions. Central_Forests stands out 
# as having moderate density (2.01) but relatively high collisions (12).

# QUESTION 27 Create collisions per capita column-------------------------------
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

print(coll_merge_per_capita)

# QUESTION 28 Scatterplot of coll_per_capita vs human_pop-----------------------
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Vehicle Collisions per Person vs Human Population",
     pch = 16,
     col = "darkblue")

# QUESTION 29 Interpret the relationship----------------------------------------

# Looking at the data, there is a strong negative relationship: ecoregions with 
# smaller human populations have much higher collisions per person.

# This makes sense because in remote areas with few people,
# there are more moose and roads are less protected, so the chance per person
# of hitting a moose is higher. In urban areas, moose are less common and
# roads often have fencing or higher traffic which would reduce per capita risk.
