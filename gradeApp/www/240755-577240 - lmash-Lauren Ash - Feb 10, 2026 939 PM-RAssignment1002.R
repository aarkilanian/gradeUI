# Title: My R script for 1002 assignment 1
# Author: Lauren Ash
# Date: 09-02-2026

# Load libraries needed
install.packages("dplyr")
library(dplyr)

# Set working directory
getwd()
# My working directory was set as it was supposed to be so I was able to leave it where it was

# Load data
moosedata<- MoosePopulation
saplings<- SaplingStudy

# Analyze data
na.omit(moosedata)
moose_clean<-na.omit(moosedata)
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min<-(1904)
moose_max<-(41250)
moosedata2<-mutate(moose_sel, MooseDensity = Estimated_Moose_Pop/Area)
moose_west<-filter(moosedata2, Ecoregion == "Western_Forests")
moose_2020<-filter(moosedata2, Year == "2020")
moose_2020_high<-filter(moose_2020, MooseDensity > "2.0")
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
moosefinal<-moosedata2 %>%
  filter(Year==2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
na.omit(saplings)
sap_clean<-na.omit(saplings)
sap_reg_browse<-sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg<-sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
# Northern_Pininsula_Forests has the highest average browsing
# StraitOfBelleIsleBarrens has the lowest average browsing
sap_reg_height<-sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low<- sap_reg_height %>%
  filter(AverageHeight < "20") %>%
  print()
#The following ecoregions have average heights less than 20cm: Northern_Peninsula_Forests and Western_Forests
sap_spe_browse<- sap_clean %>%
  group_by(Species) %>%
  summarize(meanBrowsing = mean(BrowsingScore, na.rm =TRUE)) %>%
  print()
avg_browse_spe<-sap_spe_browse %>%
  arrange(desc(meanBrowsing)) %>%
  print()
# Black_Ash has the highest browsing score
# Black_Spruce has the lowest browsing score
fir_reg_browse<-sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(meanBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
spruce_reg_browse<-sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(meanBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
sap_reg_tally<-sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally<-sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# The Sapling Study does not seem to be evenly distributed, as some ecoregions and species were sampled more than others
# Recognizing bias in ecological datasets is important because uneven sampling can influence results and lead to false conclusions that may not accurately reflect natural conditions
moose_2020b<-moose_clean %>%
  filter(Year == 2020)
moose_sap<-left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse<-moose_sap %>%
  group_by(Species, Ecoregion) %>%
  tally() %>%
  print()
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2<- moose_coll %>%
  rename_with(~"Ecoregion", study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion", relationship = "many-to-many")
coll_merge_per_capita<-coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita))

# Plot data
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab="Moose per sq km",
     main="Moose density in Newfoundland ecoregions over time")
plot(moose_west$Year, moose_west$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forest ecoregions over time",
     type ="l")
barplot(fir_reg_browse$meanBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Browsing on Balsam Fir by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
barplot(spruce_reg_browse$meanBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Browsing on Black Spruce by Ecoregion",
        col="pink",
        cex.names = 0.6)
# Black_Spruce shows lower average browsing scores than Balsam_Fir in most ecoregions
# Based on figure in Question 23:
# a. Yes, the figure supports the hypothesis. Browsing is focused on a few species at low density and becomes more evenly spread across species at higher moose density
# b. Moose browse Willow the most and Black_Spruce the least
# c. The Black_Ash species is not shown because there was not browsing data to calculate an average
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions (2020)")
#The scatterplot shows a generally positive relationship between moose density and the number of vehicle collisions. There is an outlier at moderate moose density with a high amount of collisions, which suggests other factors may also influence the collision rates
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population")
#The scatterplot shows a negative relationship between human population and moose collisions per capita. This makes sense because rural areas of Newfoundland with lower populations have higher noose densities