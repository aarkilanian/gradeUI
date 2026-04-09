#Biology1002_Raina_Summers
install.packages("dplyr")
library(dplyr)
#Question 2
library(readr)
MoosePopulation <- read_csv("~/Desktop/R assignment 1002/MoosePopulation.csv")
View(MoosePopulation)
#Question 3
moose_clean <- na.omit(MoosePopulation)
View(moose_clean)
#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
#Question 5
min(moose_sel$Year)
max(moose_sel$Year)
#Question 6
View(moose_sel)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)
#Question 7 
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Question 8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Question 9 
Moosedata_2020 <- moosedata2 %>% 
filter(Year == 2020)

moose_2020_high <- filter(Moosedata_2020, MooseDensity > 2.0)
#Question 10
arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Question 11

library(readr)
SaplingStudy <- read_csv("~/Desktop/R assignment 1002/SaplingStudy.csv")
View(SaplingStudy)

sap_clean <- na.omit(SaplingStudy)
View(sap_clean)
#Question 12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion)%>%
  summarize(mean_BrowsingScore = mean (BrowsingScore))
print(sap_reg_browse)
#b
avg_browse_reg<-arrange(sap_reg_browse,desc(mean_BrowsingScore))
#Highest is Northern_Peninsula_Forests
#Lowest is StraitOfBelleIsleBarrens
#Question 13 
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>%
  print()
sap_reg_height_low <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>%
  filter(AverageHeight < 20) %>%
  print()

# Ecoregions in sap_reg_height_low are considered severely browsed (avg height < 20 cm)
#Question 14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
avg_browse_spe <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  arrange(desc(AverageBrowsing)) %>%
  print()

# Highest browsing species = first row of avg_browse_spe
# Lowest browsing species  = last row of avg_browse_spe
#Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col = "hotpink",
        cex.names = 0.6)
#Question 17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "hotpink",
        cex.names = 0.6)
#North Shore Forest has the highest average browsing score for both Black Spruce and Balsam Fur, and some of the Balsam Fur have very low Average Browsing Scores but some have zero average browsing scores for Black Spruce, meaning the lowest isn't zero for  
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
#Question 20
# The SaplingStudy dataset is not evenly distributed.
# Some ecoregions and tree species have many more saplings sampled than others,
# which means they are overrepresented, while others are underrepresented.
#b
# Recognizing bias in ecological datasets is important because uneven sampling
# can lead to incorrect conclusions about species abundance or browsing pressure.
# Bias can make some patterns appear stronger or weaker than they actually are in nature.
#Question 21
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
View(moose_sap)
#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity  = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()
#Question 23

library(ggplot2)
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#a 
# There is partial evidence supporting the researchers’ hypothesis.
# At lower moose densities, browsing intensity differs more among tree species,
# suggesting selective feeding, while at higher moose densities browsing becomes
# more similar across species, indicating more generalist feeding.
#b
# Moose appear to favour Willow the most, as it has the highest average browsing scores.
# Black Spruce shows the lowest browsing intensity, suggesting it is less preferred by moose.
#c
# White Spruce is not shown in the figure because it does not occur in all ecoregions
# or did not have enough observations after filtering and averaging to appear in the plot.
#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Question 25
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
moose_coll2 <- moose_coll %>%
  rename_with(~"Ecoregion", .cols = "study_sites")
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
moose_2020 <- moosedata2 %>% filter(Year == 2020)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
#Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose/km²)",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions (2020)")
# Trend: Ecoregions with higher moose density tend to have more collisions, but the pattern is not perfect.
# There may be outliers where collisions are high despite moderate density (likely due to higher traffic/roads).
#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita
#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Person (2020)",
     main = "Moose Collisions per Capita vs Human Population (2020)")
#Question 29
# Trend: Collisions per capita tend to be higher in smaller human populations and lower in larger populations.
# This makes sense because rural areas can have lots of moose and high-speed roads but fewer people, so collisions per person are higher.
