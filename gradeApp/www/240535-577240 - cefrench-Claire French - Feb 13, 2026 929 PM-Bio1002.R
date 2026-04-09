# Title: R Assignment Moose Populations & Sapling Study
# Author: Claire French
# Date: 13-02-2026

#Part 1: Moose Populations in Newfoundland

#Q1
# Load libraries needed
library(dplyr)
       
# Set working directory
setwd("~/Downloads/Bio1002.R")

# Load data
#Q2
moosedata <- MoosePopulation

#Q3
moose_clean <- na.omit(moosedata)

#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5
# A.
year_min <- min(moose_sel$Year)
#1904
# B.
moose_max <- min(moose_sel$Estimated_Moose_Pop)
#4

#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Q8
# A.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# B.
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km",
     type = "l" ,
     main = "Moose density in Newfoundland ecoregions over time")

#Q9
# A.
moose_2020 <- filter(moosedata2, Year == "2020")
# B.
moose_2020_high <- filter(moose_2020, MooseDensity == ">2.0")
# C.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Part 2: Tree Sapling Study

#Q11
# A.
saplings <- SaplingStudy
# B.
sap_clean <- na.omit(saplings)

#Q12
# A.
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

"print(sap_reg_browse)"

#B.
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

print(avg_browse_reg)
#Highest Average:Northern_Peninsula_Forests (4.57) 
#Lowest Average:StraitOfBelleIsleBarrens (1) 

#Q13
#A.
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>%
  summarize(AverageHeight=mean(Height))%>%
  print()
#B.
# <20:Northern_Peninsula_Forests, Western_Forests
sap_reg_height_low <- sap_reg_height  %>% 
  filter(AverageHeight <20) %>%
  print

#Q14
#A.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsingScore=mean(BrowsingScore))%>%
  print()

#B.
#Highest: Black_Ash
#Lowest: Black_Spruce
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsingScore))

#Q15
fir_reg_browse <- saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))

#Q16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Browsing in each Ecoregion", col = "forestgreen", cex.names = 0.6)

#Q17
#A. 
spruce_reg_browse <- saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))

#B.
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Browsing in each Ecoregion", col = "blue", cex.names = 0.6)

#C.
#Black Spruce browsing intensity is generally lower than Balsam Fir browsing across most Ecoregions.
#Balsam Fir tends to experience higher browsing pressure, suggesting moose prefer browsing Balsam Fir over Black Spruce

#Q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Q19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Q20
#A.
# The SaplingStudy dataset does not appear to be evenly distributed because some ecoregions and tree species have more samples than others.
# This means certain regions or species may be overrepresented, while others may be underrepresented, which could affect conclusions about browsing intensity.

#B.
# Recognizing bias is important because an uneven dataset can lead to incorrect conclusions about ecological patterns.
# Overrepresented species or regions may make browsing pressure seem higher or lower than it truly is across the full ecosystem.

#Part 3: Creating and Joining Datasets

#Q21
#A.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Area)

print(moose_2020b)

#B.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Q22
sum_spe_browse <- moose_sap %>%
  group_by(Ecoregion, Species) %>%
  summarize(MooseDensity = mean(BrowsingScore))

#Q23
#A.
#Yes, the figure supports the hypothesis. At low moose density, browsing is concentrated on a few preferred species,
# but at higher moose density, browsing scores increase across multiple species, suggesting more generalist browsing.

#B.
#Moose appear to favour Willow the most because it has consistently high browsing scores (near 4–5).
# Black Spruce appears to be browsed the least since it shows the lowest browsing scores, including values near 0 at low density.

#C.
#Black_Ash is not shown on the figure because there were likely no usable data points (missing values or no samples),
# so it could not be included in the plot.

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25
#A.
moose_coll2 <- moose_coll %>%
  rename_with(~"Ecoregion", .cols = study_sites)

print(moose_coll2)

#B.
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")

print(coll_merge)

#Q26
#A.
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose/km^2)",
     ylab = "Number of Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions (2020)",
     pch = 16,
     col = "purple")
#B.
# There appears to be a positive relationship between moose density and the number of collisions, where regions with higher moose density tend to have more collisions.
# One or two ecoregions appear to be outliers, showing unusually high collision numbers compared to other regions with similar moose density.

#Q27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

print(coll_merge_per_capita)

#Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs Human Population",
     pch = 16,
     col = "pink")

#Q29
# The plot shows that regions with higher moose densities tend to have more moose-vehicle collisions,
# though collisions per capita are higher in areas with smaller human populations. 
# This trend makes sense because moose are more likely to encounter vehicles in areas where 
# humans are present but not densely populated, and dense moose populations naturally increase 
# the likelihood of collisions.

