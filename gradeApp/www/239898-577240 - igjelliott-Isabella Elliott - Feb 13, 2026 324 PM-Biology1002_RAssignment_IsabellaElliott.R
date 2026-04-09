# Title: My R script for Biology 1002 R Assignment 
# Author: Isabella Elliott
# Date: 13-02-2026
#Question 1
library(dplyr)
#Question 2
MoosePopulation <- read.csv("/cloud/project/RAssignment/MoosePopulation.csv")
moosedata <- MoosePopulation
rm(MoosePopulation)
#Question 3
View(moosedata)
na.omit(moosedata)
moose_clean <- na.omit(moosedata)
#Question 4
moose_clean<- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5
#a) The oldest observation in the dataset is in the year 1904 
year_min <- min(moose_sel$Year, na.rm = TRUE)
#b) The highest ‘Estimated_Moose_Pop’ recorded was 41250
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "b",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
#Question 8
#a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#b)
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density Over Time in Western Forests")
#Question 9
#a)
moose_2020 <- filter(moosedata2, Year == 2020)
#b)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Question 11
#a)
SaplingStudy <- read.csv("/cloud/project/RAssignment/SaplingStudy.csv")
saplings<-SaplingStudy
rm(SaplingStudy)
#b)
sap_clean<-na.omit(saplings)
#Question 12
#a)
sap_reg_browse <- sap_clean %>%
      group_by(Ecoregion) %>%
      summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
#b)
avg_browse_reg <- sap_reg_browse %>%
       rename(AverageBrowsing = mean_browsing) %>%
       arrange(desc(AverageBrowsing))
print(avg_browse_reg)
#The Northen_Peninsula_Forests had the highest average browsing scores, while StraitOfBelleBarrens had the lowest.
#Question 13 
#a)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height, na.rm = TRUE))
print(sap_reg_height)
#b)
# The Northern_Peninsula_Forests and Western_Forests regions have average heights less than 20 cm, indicating severe browsing.
sap_reg_height_low <- sap_reg_height %>%
    filter(mean_height < 20)
print(sap_reg_height_low)
#Question 14
#a) 
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
#b)
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(mean_browsing))
print(avg_browse_spe)
# The Black_Ash species has the highest browsing score, while the Black_Spruce species has the lowest.
#Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(fir_reg_browse)
#Question 16
barplot(fir_reg_browse$mean_browsing, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Intensity", 
        main = "Balsam Fir Browsing Intensity by Ecoregion", 
        col = "forestgreen", 
        cex.names = 0.6)
#Question 17
#a)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(spruce_reg_browse)
#b)
barplot(spruce_reg_browse$mean_browsing, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Intensity", 
        main = "Black Spruce Browsing Intensity by Ecoregion", 
        col = "steelblue",
        cex.names = 0.6)
#c) Generally, Balsam Fir experiences much higher browsing intensity than Black Spruce across most ecoregions. 
#Black Spruce is largely avoided in the Eastern Hyper Oceanic and Maritime Barrens, though it shows higher relative browsing in the Long Range Barrens. 
#Moose prefer fir because it is more nutritious, and easier to digest. Black Spruce is often avoided due to its chemical defenses, making it harder for moose to consume them.

#Question 18
sap_reg_tally <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  tally()
print(sap_reg_tally)
#No, the sampling effort was different across regions. While there was a consistent effort in many areas (several have exactly 5 trees), 
#the sampling was not uniform across all ecoregions.

#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()
print(sap_spe_tally)
#No, the same number of tree saplings was not counted for each species. 

#Question 20
#a)The SaplingStudy dataset is not evenly distributed. 
#Overrepresented: North_Shore_Forests (n=8)  and Northern_Peninsula_Forests (n=7) has the most data points.
# Underrepresented: StraitOfBelleIsleBarrens (n=1) is severely underrepresented, along with Maritime_Barrens (n=3).
#b)Recognizing bias is important because small or uneven sample sizes can lead to misleading 
# conclusions; for example, a single tree in an underrepresented region might not accurately
# reflect the browsing pressure of that entire ecosystem.

#Question 21
#a) 
moose_2020b <- moose_clean %>%
    filter(Year == 2020) %>%
    mutate(MooseDensity = Estimated_Moose_Pop / Area)
#b) 
moose_sap <- left_join(sap_clean, moose_2020b, by = "Ecoregion")

#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    mean_browsing = mean(BrowsingScore, na.rm = TRUE),
    mean_density = mean(MooseDensity, na.rm = TRUE),
    .groups = "drop")
print(sum_spe_browse)

#Question 23
#a)Yes, the evidence supports the hypothesis. At low moose densities (0.5–1.0), 
#there is a wide spread in browsing scores showing clear preference (Willow is high, while Black Spruce is at zero), 
#but as density increases (2.0+), the scores for all species converge toward the top of the scale (3–5), indicating more general browsing. 
#b) Moose favour Willow the most, as it consistently has the highest browsing scores across all density levels. 
#They browse Black Spruce the least, as it remains at zero or very low intensity until moose density becomes very high.
#c) Black Ash is not shown on the figure despite being in the legend. This likely occurred because there were no data points 
#for this species in the 2020 subset, or the ecoregions where it was found lacked corresponding moose density data, resulting in missing values that cannot be plotted.

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", 
                 "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", 
                 "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
print(moose_coll)

#Question 25
#a) 
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
#b) 
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")

#Question 26
#a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
    xlab = "Moose Density (Moose per km²)",
    ylab = "Moose-Vehicle Collisions (2020)",
    main = "Relationship Between Moose Density and Road Collisions",
    pch = 19,      
    col = "darkred")
#b) Generally, there is a positive relationship where higher moose density leads to more collisions, as seen by the cluster 
#of points increasing toward the right. However, there is a major outlier with over 100 collisions at a moderate density (~1.0), 
#which likely represents the Avalon_Forests where high human population and traffic are the primary causes of accidents rather than moose density alone.

#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita)) %>%
  select(Ecoregion, coll_per_capita, collisions2020, human_pop)
#Based on the calculation, the ecoregions with the highest risk per person are:
#Northern_Peninsula_Forests: This region has the highest rate (approx 0.005 collisions per person). Even though it doesn't have the most total collisions, its smaller population means an individual is more likely to be involved in a moose-vehicle accident here. 
#Long_Range_Barrens: (approx 0.0035 per person).
#North_Shore_Forests: (approx 0.0031 per person).

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Human Population vs. Individual Collision Risk",
     pch = 19, 
     col = "blue")

#Question 29
#The trend shows a strong negative relationship where the individual risk of a collision is highest in regions with the smallest human populations. 
#This makes sense for Newfoundland because rural areas have a much higher "moose-to-human" ratio. 
#On the other hand, for populated areas like the Avalon, the risk per person is lowered by the high number of residents.

