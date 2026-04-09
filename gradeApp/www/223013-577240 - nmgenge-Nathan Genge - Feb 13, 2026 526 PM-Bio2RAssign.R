# Title: My R script for R assignment 1, biology 1002
# Author: Nathan Genge
# Date: 13-02-2026

#PART I

# Load libraries needed ---------------------------
#### run the command install.packages("dplyr") if not already installed
#Question 1
library(dplyr)
# Set working directory ---------------------------
setwd("/Users/natha/Documents/R")

# Load data ---------------------------
#Question 2
moosedata <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv")

# Analyze data ---------------------------
#Question 3
View(moosedata)
moose_clean <- na.omit(moosedata)
View(moose_clean)

#Question 4
moose_sel = select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)

#Question 5
year_min = min(moose_sel$Year)
year_min
moose_max = max(moose_sel$Estimated_Moose_Pop)
moose_max

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)

#Question 8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)
#Question 8 continues in the "Plot data" section

#Question 9
moose_2020 <- filter(moosedata2, Year == "2020")
View(moose_2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
View(moose_2020_high)

moose_2020_high_byD = arrange(moose_2020_high, desc(MooseDensity))
View(moose_2020_high_byD)

#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Plot data ---------------------------
#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8 Cont.
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests ecoregion over time")




#PART II
# Load data ---------------------------
#Question 11
saplings = read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
View(saplings)

# Analyze data ---------------------------
#Question 11 Cont.
sap_clean <- na.omit(saplings)
View(sap_clean)

#Question 12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
print(sap_reg_browse)

#rearrange decreasing average browsing score
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(mean_BrowsingScore))
print(avg_browse_reg)
#Highest average browsing score is "Northern_Peninsula_Forests"
#Lowest average browsing score is "StraitOfBelleIsleBarrens"

#Question 13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_Height = mean(Height))
print(sap_reg_height)

sap_reg_height_low <- sap_reg_height %>%
  filter(mean_Height < 20)
print(sap_reg_height_low)
#The ecoregions with less than 20cm average height are Northern_Peninsula_forests and Western_Forests

#Question 14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
print(sap_spe_browse)

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(mean_BrowsingScore))
print(avg_browse_spe)
#Highest scores: Black_Ash and Willow
#Lowers scores: Black_Spruce and Balsam_Fir

#Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
print(fir_reg_browse)

#Question 17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
print(spruce_reg_browse)
#Question 17 continues in "Plot data"

#Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

# Plot data ---------------------------

#Question 16
barplot(fir_reg_browse$mean_BrowsingScore, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score (0-5)", 
        main = "Average Balsam Fir Browsing Intensity by Ecoregion", 
        col = "forestgreen", 
        cex.names = 0.6)  # Reduces x-axis label size for readability

#Question 17 Cont.
barplot(spruce_reg_browse$mean_BrowsingScore, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score (0-5)", 
        main = "Average Black Spruce Browsing Intensity by Ecoregion", 
        col = "darkgreen", 
        cex.names = 0.6)  # Reduces x-axis label size for readability
#Balsam Fir is in different ecoregions than black spruce. Black spruce has consistantly lower scores than Balsam Fir.

#Question 20
#the dataset is not evenly distributed. The Balsam Fir has 10 trees and Black spruce has 9, while other trees like black ash only have 1 tree
#the same is true with regions, where the strait of belle isle barrens has only 1 tree while the north shore forests have 8 trees





#PART III

#Question 23
#A) Yes the hypothesis is supported. At low moose density there is a large spread in browsing scores, where at high density the browsing scores converge to the top of the scale
#B) The most preferred is Willow and Alder, and the least preferred is black spruce
#C) Black ash is not shown because it is overlapped by another point with the exact same value

# Analyze data ---------------------------
#Question 21
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
View(moose_2020b)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
View(moose_sap)

#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Ecoregion, Species) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore),
            mean_MooseDensity = mean(MooseDensity)) %>%
  print()
View(sum_spe_browse)

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25 EXEMPT FROM GRADING
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
View(moose_coll2)

coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
View(coll_merge)

#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
View(coll_merge_per_capita)
#Northern_Peninsula_Forests has the highest number of moose collisions per person


# Plot data ---------------------------
#Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose per sq km)",
     ylab = "Number of Moose-Vehicle Collisions (2020)",
     main = "Relationship Between Moose Density and Vehicle Collisions",
     pch = 19,
     col = "brown")

#to see a trend, I can add a trend line. this will also help to show out liars
abline(lm(collisions2020 ~ MooseDensity, data = coll_merge), col = "red", lwd = 2)
#This line shows a general upward trend, with an out liar on the 1.0 Moose Density mark with above 100 collisions.
#Generally, higher moose density means more collisions

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Relationship Between Human Population and Collisions per Capita",
     pch = 19,  # Solid circles for points
     col = "darkred",  # Red points
     cex = 1.5)  # Larger point size
#Question 29
#This trend makes sense, as more moose are around less populated areas, and this plot shows that the lower the population, the higher amount of collsions per person in the area.
