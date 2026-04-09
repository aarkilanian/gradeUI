#Q1
install.packages("dplyr") 
library(dplyr)
#Q2
MooseData <- read.csv(file = "MoosePopulation.csv") 
#Q3
Moosedata <- na.omit(MooseData) 
#Q4
MoosedataS <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop) #
#Q5
min(MoosedataS$Year) #1904
max(MoosedataS$Estimated_Moose_Pop) #41250
#Q6 
MoosedataM <- mutate(MoosedataS, MooseDensity = Estimated_Moose_Pop / Area)
#Q7
plot(MoosedataM$Year, MoosedataM$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
#Q8 
MoosedataWest <- filter(MoosedataM, Ecoregion == "Western_Forests")
plot(MoosedataWest$Year, MoosedataWest$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forest over time",
     type = "l")
#Q9
Moosedata2020 <- filter(MoosedataM, Year == 2020)
Moosedata_2020_high <- filter(Moosedata2020, MooseDensity >=2.0)
Moosedata_2020_high_byD <- arrange(Moosedata_2020_high, desc(MooseDensity))
#Q10
moosefinal <- MoosedataM %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#PART II SAPLING STUDY
#Q11
saplings <- read.csv(file = "SaplingStudy.csv")
sap_clean <- na.omit(saplings)
#Q12
Moosebrowsing_byEcoregion <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
#northern peninsula and north shore had the highest browsing scores and
#straight of bell isle and maritime barrens had the lowest browsing scores
#Q13
TreeHeight_byEcoregion <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE)) %>%
  print()
SeverelyBrowsed <- TreeHeight_byEcoregion %>%
  filter(Mean_Height < 20) %>%
  print()
#Northern peninsula and Western forests had average heights less than 20cm
#Q14
Browsing_bySpecies <- sap_clean %>%
  group_by(Species) %>%
  summarize(Mean_Browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
#Black ash has the highest browsing, and black spruce had the lowest browsing
#Q15
BalsamFir <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
#Q16
barplot(BalsamFir$Mean_Browsing,
        names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Moose Browsing Score",
        main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        col = "limegreen",
        cex.names = 0.4)
#Q17
BlackSpruce <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
barplot(BlackSpruce$Mean_Browsing,
        names.arg = BlackSpruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Moose Browsing Score",
        main = "Average Black Spruce Browsing Intensity by Ecoregion",
        col = "pink",
        cex.names = 0.5)    
#The Black Spruce has a much lower average that Balsam Fir, which suggest
# that Balsam Fir is the mooses favorite.
#Q18
EcoregionTally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#Q19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally () %>%
  print()
#No, the saplings are not evenly distributed.
#The number of saplings range from a minimum of 1, to a maximum of 11
#indicating that some specices are overpresented while others are underrepresented
#Unequal sample sizes can bias the analysis and affect the accuary of the comparisions between tree types
#PART III
#Q21
Moosedata2020 <- filter(MooseData, Year == 2020)
View(Moosedata2020)
MooseDataSaplings <- left_join(MooseData, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#Q22  
BrowsingbySpeciesDensity <- MooseDataSaplings %>%
  group_by(Species, Ecoregion) %>%
  summarize(Avg_Browsing = mean(BrowsingScore, na.rm = TRUE),
            Avg_MooseDensity = mean(Estimated_Moose_Pop, na.rm = TRUE)) %>%
  print()
#Q23
install.packages("ggplot2")
library(ggplot2)
ggplot(BrowsingbySpeciesDensity, aes(x = Avg_MooseDensity, y = Avg_Browsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
                  labs(title = "Browsing Intensity Across Moose Density by Species",
                       x = "Average Moose Density",
                       y = "Average Browsing Score")
#yes, moose show strong preferences at low density, and at higher density,
#moose are less selective in their browsing.
#Moose favour the Willow the most because its the highest all throughout the Avg Moose densities,
#Black Spruce is the least favortie for most average moose densities.
#Black ash was not on the graph, this suggests that there was little to none data for moose browsing in that area.
#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraightOfBelleIsleBarrens")
MooseCollisions <- data.frame(study_sites, human_pop, collisions2020)
#Q25
MooseCollisions <- MooseCollisions %>%
  rename_with(~"Ecoregion", study_sites)
MooseCollisions_2020 <- left_join(MooseCollisions, Moosedata2020, by = "Ecoregion")
View(MooseCollisions_2020)
#Q26  
plot(MooseCollisions_2020$Estimated_Moose_Pop, MooseCollisions_2020$collisions2020,
     xlab = "Moose Density (moose per sq km)",
     ylab = "Number of Moose-Vechile Collisions (2020)",
     main = "Relationship Between Moose Density and Vechile Collisions",
     pch = 19,
     col = "darkred")
# the higher the moose density, the more collions there is, its directly proportional.
# there is one outlier at around 5000 moose density, there was alor of collions.
#Q27  
MooseCollisions_2020 <- MooseCollisions_2020 %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
MooseCollisions_2020 %>%
  arrange(desc(coll_per_capita)) %>%
  print()
#Q28  
plot(MooseCollisions_2020$human_pop,
     MooseCollisions_2020$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions Per Capita (2020)",
     main = "Moose-Vechile Collisions Per Capita vs Human Population",
     pch = 19,
     col = "coral")
#Q29
#The graph shows lower human population = more collisions which makes sense in
#Newfoundland because there is no moose collisions in higher populated areas, 
#There is more moose collisions in more rural areas.