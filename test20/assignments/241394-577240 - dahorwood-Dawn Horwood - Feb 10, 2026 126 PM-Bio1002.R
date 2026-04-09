#Question 1
library(dplyr)
#Question 2
moosedata <- read.csv("~/Downloads/MoosePopulation.csv")
View(moosedata)
#Question 3
moose_clean <- na.omit(moosedata)
#Question 4 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5a
year_min <- min(moose_sel$Year)
#Question 5b
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
#Question 8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Question 8b
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density over time in the Western Forests Ecoregion")
#Question 9a
moose_2020 <- filter(moosedata2, Year == "2020")
#Question 9b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#Question 9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Part 2 --------------------------------------

#Question 11a
saplings <- read.csv("~/Downloads/SaplingStudy.csv")
View(saplings)
#Question 11b
sap_clean <- na.omit(saplings)
#Question 12a
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(Height)) %>%
  print()
#Question 12b
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
# Northern Peninsula Forests has the highest browsing score
# Strait of Belle Isle Barrens has the lowest average browsing score
#Question 13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>% 
  summarize(AverageHeight = mean(Height)) %>%
  print()
#Question 13b
sap_reg_height_low <- filter(sap_reg_height, AverageHeight <20)
# The Western Forests and Northern Peninsula Forests have average heights less than 20cm.
#Question 14a
sap_spe_browse <- sap_clean %>% 
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#question 14b
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
#Black Ash has the highest browsing score.
#Black Spruce has the lowest browsing score.
#Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#Question 16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregions",
        ylab = "Average Browsing Intensity",
        main = "The Average Browsing Intensity of Balsam Fir in Different Ecoregions",
        col = "forestgreen", cex.names = 0.6) 
#Question 17a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBorwsing = mean(BrowsingScore)) %>% 
  print()
#Question 17b
barplot(spruce_reg_browse$AverageBorwsing, names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregions", 
        ylab = "Average Browsing Intensity", 
        main = "The Average Browsing Intensity of Black Spruce in Different Ecoregions", 
        col = "yellow", cex.names = 0.6)
#Question 17c
#Balsam fir has higher browsing in regions such as North Shore Peninsula Forests and Central Forests, so does black spruce but it's average browsing is still lower than Balsam Firs.
#Question 18
sap_spe_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print() 
#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#Question 20a
#The SaplingStudy dataset isn't evenly distributed because certain tree species and ecoregions are underrepresented or overrepresented. 
#It is important to recognize bias in ecological datasets.If it is not recognized it can lead you to make conclusions about the data that are not accurate.

# Part 3 --------------------------------

#Question 21a
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
#Question 21b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', 
                       relationship = "many-to-many")
#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Ecoregion) %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore),
            AverageMooseDensity = mean(MooseDensity)) %>%
  print()
#Question 23a
#This figure doesn't support the hypothesis. For low moose density there are high browsing scores for certain species of tree, but for high moose density there is an increasing browsing score across many of the tree species. This shows how there is a shift towards generalist browsing at high moose densities.
#Question 23b
#Moose seem to browse black spruce least and willow most. 
#Question 23c
#The species not shown in the figure is black ash. This may be because the species was not frequently found enough at the site of the study to be able to collect usable data.
#Question 24
collisions2020 <- c(56, 60, 14,36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Question 25a
moose_coll2 <- rename_with(moose_coll, ~ "Ecoregion", "study_sites")
#Question 25b
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', 
                        relationship = "many-to-many")
#Question 26a
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions (2020)", 
     main = "Moose Density in relation to Moose-Vehicle Collisions")
#Question 26b
#When moose density increases, the number of moose-vehicle collisions also increases.
#There are outliers in some areas because when moose density is moderate, the number of moose-vehicle collisions are very high.
#Question 27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions per Capita",
     main = "Moose Collisions per Capita in relation to Human Population")
#Question 29
#Regions with smaller human populations have higher moose collisions per capita which means the two variables must have a negative relationship.
#This makes sense because I know that places around the bay tend to have smaller populations but they have a lot more moose sightings so it is more likely that there will be more moose-vehicle collisions in these areas.
