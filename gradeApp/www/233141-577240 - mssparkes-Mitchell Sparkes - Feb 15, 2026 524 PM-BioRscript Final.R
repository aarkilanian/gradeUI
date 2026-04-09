# Title: R Script for Assignment 1 - Biology 1002
# Author: Mitchell Sparkes
# Date: February 11th, 2026

# Question 1 ---------------------------------------------------------------------------
install.packages("dplyr")
library("dplyr")

# question 2 ---------------------------------------------------------------------------
# I used the import data set button in the environment tab to import the data set

# Question 3 ---------------------------------------------------------------------------
moose_clean<- na.omit(MoosePopulation)
moose_clean
# Question 4 --------------------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel
# Question 5 ---------------------------------------------------------------------------
# a.
year_min<- min(select(moose_sel, Year))
year_min
# b.
moose_max<- max(select(moose_sel, Estimated_Moose_Pop))
moose_max
# Question 6 ---------------------------------------------------------------------------
moosedata2<- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
moosedata2
# Question 7 ---------------------------------------------------------------------------
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose/km^2", main = 'Moose Density over time in Newfoundland Ecoregions')

# Question 8 ---------------------------------------------------------------------------
# a.
moose_west<- filter(moosedata2, Ecoregion == 'Western_Forests')
# b.
plot(moose_west$Year, moose_west$MooseDensity, pch = 17, type = "l", xlab = "Year", ylab = "Moose/km^2", main = "Moose Density over time in Western Forests Ecoregion of Newfoundland")

# Question 9 ---------------------------------------------------------------------------
# a.
moose_2020<- filter(moosedata2, Year == "2020")
moose_2020
# b.
moose_2020_high<- filter(moose_2020, MooseDensity > "2.0")
moose_2020_high
# c.
moose_2020_high_byD<- arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD
# Question 10 --------------------------------------------------------------------------
moosefinal<- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Question 11 --------------------------------------------------------------------------
# I loaded the file using the "Import Data set" button in the environment tab
sap_clean<- na.omit(SaplingStudy)
sap_clean
# Question 12 --------------------------------------------------------------------------
# a.
sap_reg_browse<- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore))
print(sap_reg_browse)

# b.
avg_browse_reg<- sap_reg_browse %>%
  arrange(desc(mean_browsing))
print(avg_browse_reg)

# Northern_Peninsula_Forests has the highest mean browsing with 4.571429, and 
#Strait Of Belle Isle Barrens has the lowest mean browsing with 1.000000

# Question 13 --------------------------------------------------------------------------
# a.
sap_reg_height<- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height))
print(sap_reg_height)

# b.
sap_reg_height_low<- sap_reg_height %>%
  filter(mean_height < 20)
print(sap_reg_height_low)

# Northern_Peninsula_Forests and Western_Forests had average heights less than 20cm

#Question 14 --------------------------------------------------------------------------
# a.
sap_spe_browse<- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_browsing = mean(BrowsingScore))
print(sap_spe_browse)

# b.
avg_browse_spe<- sap_spe_browse %>%
  arrange(desc(mean_browsing))
print(avg_browse_spe)

# Black_Ash has the highest browsing score with 5, Black_Spruce has
# the lowest browsing score with 2.33

#Question 15 --------------------------------------------------------------------------
fir_reg_browse<- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore))
print(fir_reg_browse)

# Question 16 --------------------------------------------------------------------------
barplot(fir_reg_browse$mean_browsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Browsing Intensity of Balsam Fir Trees in Different NL Ecoregions", col = "red", cex.names = 0.6)

# Question 17 --------------------------------------------------------------------------
# a.
spruce_reg_browse<- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore))
print(spruce_reg_browse)

# b.
barplot(spruce_reg_browse$mean_browsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Browsing Intensity of Black Spruce Trees in Different NL Ecoregions", col = "blue", cex.names = 0.6)

# c.
#Fir is usually browsed more often in ecoregions compared to Black Spruce,
#except in Long range barons. Fir browsing is typically higher and more
#consistent across ecoregions, while the Black Spruce shows more variability. 
#Spruce was also sampled in 1 more region than Fir.

# Question 18 --------------------------------------------------------------------------
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# No, the same number of tree samples were not counted in each region

# Question 19 --------------------------------------------------------------------------
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# No, the same number of tree samples were not counted in each species

# Question 20 --------------------------------------------------------------------------
# a.
# The SaplingStudy dataset was not evenly distributed. 
# Over-represented: Ecoregion - Northern_Penisula_Forests, and North_Shore_Forests. Species - Balsam Fir
# Under-represented:Ecoregion - StraitOfBelleIsleBarrens. Species - Black Ash

# b.
# It is important to recognize bias in ecologocial datasets, because an 
#over/under representation of a dataset can lead to incorrect assumptions 
#about the dataset.


# Question 21 --------------------------------------------------------------------------
#a.
moose_2020b<- moose_clean %>%
  filter(Year == "2020") %>%
  mutate (MooseDensity = Estimated_Moose_Pop / Area)
print(moose_2020b)

#b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
moose_sap
# Question 22 -------------------------------------------------------------------------
sum_spe_browse<- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore), mean_density = mean(MooseDensity))
print(sum_spe_browse)

# Question 23 -------------------------------------------------------------------------
# a.
# Yes this hypothesis is correctly shown within the graph. At a low Average 
# Moose Density, there is only Alder and Willow trees in Average Browsing Score, 
# but at a high Average Moose Density there is only trees found in high Average
# Browsing Score.

# b.
# Moose prefer the willow tree the most because throughout the graph willow 
# trees were always at the high Average Browsing Score. Moose browse the Black
# Spruce the least, as shown in the graph, Black Spruce is typically at the 
# bottom compared to other trees, regardless of Average Moose Density.

# c.
# Black Ash was not on the graph, most likely due to a lack of sufficient data
# within the Average Densities of the study.

# Question 24 -------------------------------------------------------------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll
# Question 25 -------------------------------------------------------------------------
# a.
moose_coll2<- moose_coll %>%
  rename("Ecoregion" = study_sites)
moose_coll2
#b.
coll_merge<- left_join(moose_coll2, moose_2020, by = 'Ecoregion', relationship = "many-to-many")
coll_merge
# Question 26 -------------------------------------------------------------------------
# a.
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density (Moose/km^2)", ylab = "Number of Vehicle Collisions with Moose in 2020", main = "Moose Density vs Moose-Vehicle Collisions")

# b.
# Typically, as the collisions increase, the density also increases. The only
# outlier was at 1.0 moose density, where the collisions suddenly rise over 100.
# This is likely due to the frame being taken in "Avalon_Forests" while the
# human pop was 270000. This is much larger than the other Ecoregions.

# Question 27 -------------------------------------------------------------------------
coll_merge_per_capita<- coll_merge %>%
  mutate(collisions_per_capita = collisions2020 / human_pop)
coll_merge_per_capita
# Question 28 -------------------------------------------------------------------------
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$collisions_per_capita, xlab = "Human Population", ylab = "Collisions per Capita", main = "Collisions per Capita vs Human Population")
  
# Question 29 -------------------------------------------------------------------------
# The graph shows that generally as Human Population increases, Collisions per
# Capita decreases. This supports our previous data of moose vs human population
# in Newfoundland. In the Avalon, human population was higher, but had a much 
# lower moose population. Compared to rural areas, as the moose population 
# increased, the human population was much lower.
