# Title: R Script for Assignment 1 - Biology 1002
# Author: Matthew Oliver
# Date: February 7th, 2026

# Question 1 ---------------------------------------------------------------------------
install.packages("dplyr")
library("dplyr")

# question 2 ---------------------------------------------------------------------------
# I used the import data set button in the environment tab to import the data set

# Question 3 ---------------------------------------------------------------------------
View(moosedata)
moose_clean<- na.omit(moosedata)
print(moose_clean)

# Question 4 --------------------------------
moose_sel<- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
print(moose_sel)

# Question 5 ---------------------------------------------------------------------------
# a.
year_min<- min(select(moose_sel, Year))
print(year_min)

# b.
moose_max<- max(select(moose_sel, Estimated_Moose_Pop))
print(moose_max)

# Question 6 ---------------------------------------------------------------------------
moosedata2<- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
print(moosedata2)

# Question 7 ---------------------------------------------------------------------------
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose/km^2", main = 'Moose Density over time in Newfoundland Ecoregions')

# Question 8 ---------------------------------------------------------------------------
moose_west<- filter(moosedata2, Ecoregion == 'Western_Forests')
print(moose_west)
plot(moose_west$Year, moose_west$MooseDensity, pch = 17, type = "l", xlab = "Year", ylab = "Moose/km^2", main = "Moose Density over time in Western Forests Ecoregion of Newfoundland")

# Question 9 ---------------------------------------------------------------------------
moose_2020<- filter(moosedata2, Year == "2020")
moose_2020_high<- filter(moose_2020, MooseDensity > "2.0")
moose_2020_high_byD<- arrange(moose_2020_high, desc(MooseDensity))
print(moose_2020_high_byD)

# Question 10 --------------------------------------------------------------------------
moosefinal<- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Question 11 --------------------------------------------------------------------------
# I loaded the file using the "Import Data set" button in the environment tab
sap_clean<- na.omit(saplings)
print(sap_clean)

# Question 12 --------------------------------------------------------------------------
# a.
sap_reg_browse<- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore)) %>%
  print()

# b.
avg_browse_reg<- sap_reg_browse %>%
  arrange(desc(mean_browsing)) %>%
  print()

# Northern_Peninsula_Forests had the highest mean browsing of 4.571429

# Strait Of Belle Isle Barrens had the lowest with 1.000000

# Question 13 --------------------------------------------------------------------------
# a.
sap_reg_height<- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height)) %>%
  print()

# b.
sap_reg_height_low<- sap_reg_height %>%
  filter(mean_height < 20) %>%
  print()

# Northern_Peninsula_Forests and Western_Forests both had avergae heights less than 20cm

#Question 14 --------------------------------------------------------------------------
# a.
sap_spe_browse<- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_browsing = mean(BrowsingScore)) %>%
  print()

# b.
avg_browse_spe<- sap_spe_browse %>%
  arrange(desc(mean_browsing)) %>%
  print()

# Black_Ash had the highest with 5, Black_Spruce had the lowest with 2.33

#Question 15 --------------------------------------------------------------------------
fir_reg_browse<- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore)) %>%
  print()

# Question 16 --------------------------------------------------------------------------
barplot(fir_reg_browse$mean_browsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Browsing Intensity of Balsam Fir Trees in Different NL Ecoregions", col = "red", cex.names = 0.6)

# Question 17 --------------------------------------------------------------------------
# a.
spruce_reg_browse<- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore)) %>%
  print()

# b.
barplot(spruce_reg_browse$mean_browsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Browsing Intensity of Black Spruce Trees in Different NL Ecoregions", col = "blue", cex.names = 0.6)

# c.
# Fir tends to be browsed more in most ecoregions compared to the Black Spruce, except for the Long range barons. Fir browsing is generally higher and more consistent accross ecoregions while the Black Spruce shows more variability. Spruce was also sampled in 1 more region than the fir.

# Question 18 --------------------------------------------------------------------------
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# The same number was not counted in each ecoregion

# Question 19 --------------------------------------------------------------------------
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# No the same number wasn't counted for each species

# Question 20 --------------------------------------------------------------------------
# a.
# The SaplingStudy dataset isn't evenly distributed. Some Ecoregions like North_Shore_Forests and Northern_Peninsula_Forests are over-represented, while StraitOfBelleIsleBarrens is under-represented. Among tree species, Balsam Fir is over-represented and Black Ash is under-represented. So the dataset is unequally represented.

# b.
# Recognizing bias is important because uneven representation of data can skew our understanding of patterns and lead to incorrect conclusions about ecology.

# Question 21 --------------------------------------------------------------------------
#a.
moose_2020b<- moose_clean %>%
  filter(Year == "2020") %>%
  mutate (MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()

#b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many") %>%
  print(moose_sap)

# Question 22 -------------------------------------------------------------------------
sum_spe_browse<- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore), mean_density = mean(MooseDensity)) %>%
  print()

# Question 23 -------------------------------------------------------------------------
# a.
# Yes the hypothesis is clearly shown on this graph, at low moose density only a couple trees species have high average browsing values while at high moose density all trees have a high avergae browsing value.

# b.
# Moose favor the Willow tree as it had high browsing scores across all densities. The Black Spruce was the least favored, it was vrowsed the least and its scores at low density were 0.

# c.
# Black Ash was not included in the graph likely because it didn't have enough data across densities to include in the figure

# Question 24 -------------------------------------------------------------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites) %>%
  print(moose_coll)

# Question 25 -------------------------------------------------------------------------
# a.
moose_coll2<- moose_coll %>%
  rename("Ecoregion" = study_sites) %>%
  print()

#b.
coll_merge<- left_join(moose_coll2, moose_2020, by = 'Ecoregion', relationship = "many-to-many") %>%
  print()

# Question 26 -------------------------------------------------------------------------
# a.
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density (Moose/km^2)", ylab = "Number of Vehicle Collisions with Moose in 2020", main = "Moose Density vs Moose-Vehicle Collisions")

# b.
# Generally, the number of collisions increases as the density increases. The outlier is at around 1.0 moose density where the collisions was above 100, however this is because this frame was taken in "Avalon_Forests" where the human pop is 270000, which is significantly larger than all the other Ecoregions.

# Question 27 -------------------------------------------------------------------------
coll_merge_per_capita<- coll_merge %>%
  mutate(collisions_per_capita = collisions2020 / human_pop) %>%
  print()

# Question 28 -------------------------------------------------------------------------
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$collisions_per_capita, xlab = "Human Population", ylab = "Collisions per Capita", main = "Collisions per Capita vs Human Population")
  
# Question 29 -------------------------------------------------------------------------
# The Graph demonstrates that generally as population increases, Moose Collisions decreases, this makes sense given what we know about moose/human populations in NL. In the AValon, the human population is much higher but has a much lower moose population, while in the more rural areas, the moose population is much more elevated but the human population is much lower.

