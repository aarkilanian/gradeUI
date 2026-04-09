# Title R Assignment Biology 1002
# Author Mark Penney
# Date 20-01-2026

# 1.---------------------------
library(dplyr)

# 2. ---------------------------
MooseData <- read.csv("~/Downloads/MoosePopulation.csv")

# 3. ---------------------------
MooseData <- na.omit(MooseData)

# 4. ---------------------------
select(MooseData, Ecoregion, Year, Area, Estimated_Moose_Pop)
MooseData <- select(MooseData, Ecoregion, Year, Area, Estimated_Moose_Pop)

# 5. ---------------------------
min(MooseData$Year)
# 1904

# 6. ---------------------------
mutate(MooseData, Moosedensity = Estimated_Moose_Pop / Area)
MooseData <- mutate(MooseData, Moosedensity = Estimated_Moose_Pop / Area)

# 7. ---------------------------
plot(MooseData$Year, MooseData$Moosedensity, type = "l", xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# 8. ---------------------------
western_forests_data <- MooseData %>%
  filter(Ecoregion == "Western_Forests")
MooseDataWest <- filter(MooseData, Ecoregion == "Western_Forests")

plot(MooseDataWest$Year, MooseDataWest$Moosedensity, type = "l", 
       xlab = "Year", 
       ylab = "Moose per sq km",
       main = "Moose density in Western Forests")
# 9. ---------------------------
MooseData2020 <- filter(MooseData, Year == "2020")

MooseData2020_b <- filter(MooseData2020, Moosedensity > 2.0)

MooseData2020_b <- arrange(MooseData2020_b, desc(Moosedensity))

# 10. --------------------------
MooseData_final <- MooseData %>%
  filter(Year == 2020) %>%
  filter(Moosedensity > 2.0) %>%
  arrange(desc(Moosedensity)) %>%
  print()
# 11. --------------------------
Saplings<- read.csv("~/Downloads/SaplingStudy.csv")

Saplings <- na.omit(Saplings)
# 12. --------------------------
BrowsingScore_by_Eco <- Saplings %>% 
  group_by(Ecoregion) %>% 
  summarize(mean(BrowsingScore)) %>% 
  print()
# StraightOfBellesIsleBarrens has the lowest with 1
# Norther_Peninsula_Forsets has the highest with 4.57

# 13. --------------------------
Height_by_Eco <- Saplings %>% 
  group_by(Ecoregion) %>% 
  summarize(mean(Height)) %>% 
  print()
#Northern_Peninsula_Forests and Western_Forests have been severely browsed by moose

# 14. --------------------------
BrowsingScore_by_Spec <- Saplings %>% 
  group_by(Species) %>% 
  summarize(mean(BrowsingScore)) %>% 
  print()
#Black_Ash has the highest score and Black_Spruce the lowest

# 15. --------------------------
BalsamFir <- Saplings%>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

# 16. --------------------------
barplot(BalsamFir$`mean(BrowsingScore)`, names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
        
# 17. --------------------------
BlackSpruce <- Saplings%>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

barplot(BlackSpruce$`mean(BrowsingScore)`, names.arg = BlackSpruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Black Spruce Browsing Intensity by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
#BlackSpruce differs because it has bars that are sitting at zero where as all Balsam Fir bars have a significant value.      
        
# 18. --------------------------       
EcoregionTally<- Saplings %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()    
#Different in each

# 19. --------------------------
Species_Tally <- Saplings %>%
  group_by(Species) %>%
  tally() %>%
  print()
#Different in each

# 20. --------------------------
# a) The Saplings dataset does not appear to be evenly distributed. Some tree species and certain ecoregions are overrepresented, while others have fewer sampled saplings. This may bias the interpretation of the moose browsing patterns.

# b) Recognizing bias in datasets is important because it can form misleading conclusions. In this case, it would cause misinformation about species and their ecosystems.


# 21. --------------------------
moose_2020b <- MooseData %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, Saplings, by = 'Ecoregion', relationship = "many-to-many")

# 22. ---------------------------
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()


# 23. ---------------------------
#a) The figure supports the hypothesis. At low density the browsing is concentrated on few species. However at a higher density it becomes more general.
#b) They favour Willow because it is shown with the highest browsing average. They favour Black Spruce the least because it is at the bottom of the graph.
#c) Black ash is not shown at all because it has only one sample when the others have multiple.

# 24. --------------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# 25. ---------------------------
moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", study_sites)

coll_merge <- left_join(MooseData2020, moose_coll2, by = "Ecoregion")

# 26. ---------------------------
plot(
  coll_merge$Moosedensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Moose Vehicle Collisions 2020",
  main = "Moose Density vs Moose-Vehicle Collisions")

# There is a general increase in collisions across higher densities besides the outliers when the Moose density is 1.0. 

# 27. --------------------------
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# 28. --------------------------
plot(coll_merge_per_capita$human_pop, 
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population")

# 29. --------------------------
# As the human population goes up, there is a decrease in Moose collisions per capita. This makes sense because in a highly populated place such as St.Johns, there aren't as many collisions as in a less populated place such as Whitbourne, where there are many more collisions.

