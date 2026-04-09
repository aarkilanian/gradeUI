#START OF MOOSE POPU. IN NEWFOUNDLAnD
#BLAKE JAcKSON FOR BIOL 1002, 202510503

#Question 1
library("dplyr") #Loads dplyr
#Question 2
Moosedata <- read.csv("MoosePopulation.csv")
#Question 3
View(Moosedata)
#Question 4
Moosedata <- na.omit(Moosedata)
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5, A)
min(Moosedata$Year) #1904
#Question 5, B)
max(Moosedata$Estimated_Moose_Pop) #41250
#Question 6
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7
plot(Moosedata$Year, Moosedata$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland Ecoregions over time")
#Question 8, A)
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")
#Question 8, B) Creating a line graph for moose population in Western Forests
plot(MooseDataWest$Year, MooseDataWest$Estimated_Moose_Pop, 
     type = "l", 
     xlab = "Year", 
     ylab = "Estimated Moose Population", 
     main = "Moose Population Over Time, Western Forests")
#Question 9, A)
MooseData_2020 <- filter(Moosedata, Year == 2020)
#Question 9, B)
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)
#Questions 9, C)
arrange(MooseData_2020_b, desc(MooseDensity))
#Question 10
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#START OF TREE SAPLING STUDY

#Question 11, A)
SaplingStudy <- read.csv("SaplingStudy.csv")
#Question 11, B)
View(SaplingStudy)
sap_clean <- na.omit(SaplingStudy)
#Question 12, A)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore))
print(sap_reg_browse)
#Question 12, B)
#Northern_Peninsula_Forests has the most moose browsing
#Question 13, A)
HeightSummary <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Height = mean(Height))
print(HeightSummary)
#Question 13, B)
#Based on the data, the ecoregions are:
#Northern_Peninsula_Forests and Western_Forests
#Question 14, A)
SpeciesSummary <- sap_clean %>%
  group_by(Species) %>%
  summarize(Mean_Browsing = mean(BrowsingScore))
#Question 14, B)
# The species with the highest browsing score is Black Ash.
# The species with the lowest browsing score is Black Spruce.
#Question 15
BalsamFir <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore))
print(BalsamFir)
#Question 16
barplot(BalsamFir$Mean_Browsing, 
        names.arg = BalsamFir$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Intensity", 
        main = "Balsam Fir Browsing Intensity by Ecoregion", 
        col = "forestgreen", 
        cex.names = 0.6)
#Question 17, A)
BlackSpruce <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore))
print(BlackSpruce)
#Question 17, B) 
barplot(BlackSpruce$MeanBrowsing, 
        names.arg = BlackSpruce$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Mean Browsing Score", 
        main = "Average Black Spruce Browsing Intensity by Ecoregion", 
        col = "blue", 
        cex.names = 0.6)
#Question 17, C)
#Balsam Fir tends to have higher browsing scores across most ecoregions 
#Question 18
EcoregionTally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#Question 19
SpeciesTally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#Question 20, A)
# The sap_clean dataset is not evenly distributed. 
# The North Shore Forests (8) and Northern Peninsula Forests (7) are overrepresented, 
# while the Strait of Belle Isle Barrens (1) is underrepresented. 
# For species, Balsam Fir (11) and Black Spruce (9) are overrepresented, while Black Ash (1) is underrepresented.

#Question 20, B)
# It is important to recognize bias because if certain areas or species are sampled more than others, 
# the average results will be skewed toward those groups and won't accurately represent the whole island. 

#START OF "CREATING AND JOINING DATASETS"

# Question 21, A)
moose_2020b <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
# Question 21, B)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
head(moose_sap)
# Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()
# Question 23
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color = Species)) + 
  geom_point(size = 3) + 
  theme_minimal() + 
  labs(
    title = "Browsing Intensity Across Moose Density by Species",
    x = "Average Moose Density (per sq km)",
    y = "Average Browsing Score"
  )
#Question 23, A)
#Yes, the figure supports this hypothesis. 
#At low moose densities (0–1), browsing scores vary widely (from 0 to 4.5) showing a selective preference, 
#whereas at high densities (2–3), almost all species cluster at the top of the scale (3.5–5), suggesting a shift toward generalist browsing.

#Question 23, B)
#Willow is favored the most, consistently maintaining the highest browsing scores across all moose densities. 
#Black Spruce is browsed the least, as it consistently holds the lowest browsing scores, particularly at low to moderate moose densities.

#Question 23, C)
#Black Ash is not visible on the figure because its data points are likely being "overplotted" or hidden behind other species with identical browsing scores. 
#Given its absence from the visible field despite being in the legend, it likely shares the same low browsing values as Black Spruce or Balsam Fir.

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25

moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")

#Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density", 
     ylab = "Moose Collisions", 
     main = "Moose density related to moose-vehicle collisions")
#Question 26 b)
#The active trend is upwards, with an outlier being at 1.0 on density

#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

coll_merge_per_capita %>% 
  arrange(desc(coll_per_capita)) %>% 
  select(Ecoregion, coll_per_capita)

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs. Human Population",
     pch = 19, col = "darkred")

#Question 29
#The graph shows a downward trend, where collision rates per person are much higher
#in small communities than in big ones. This makes sense for Newfoundland because in 
#rural areas you've got fewer people but way more moose, and drivers are constantly 
#going through prime moose habitat.
