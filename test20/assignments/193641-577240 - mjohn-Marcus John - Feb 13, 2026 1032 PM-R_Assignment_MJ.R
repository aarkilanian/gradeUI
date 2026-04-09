getwd()
setwd("Desktop/R_Assignment")
install.packages("dplyr")

## Part 1 - Moose Populations in Newfoundland

#Question 1
library("dplyr")

#Question 2
moosedata <- read.csv("MoosePopulation.csv")

#Question 3
moose_clean <- na.omit(moosedata)

#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#Question 8b
plot(moose_west$Year, moose_west$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forest Region over time")

#Question 9a
moose_2020 <- filter(moosedata2, Year == 2020)

#Question 9b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

#Question 9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10
moosefinal <- moosedata2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()

##Part 2 - Tree Sapling Study

#Question 11a
saplings <- read.csv("SaplingStudy.csv")

#Qustion 11b
sap_clean <- na.omit(saplings)

#Question 12a
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()

#Question 12b
avg_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowsing))

#Question 13a
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height)) %>% print()

#Question 13b
sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20) %>% print()

#Question 14a
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()

#Question 14b
avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing))

#Question 15
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))

#Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "green",
        cex.names = 0.6)

#Question 17a
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))

#Question 17b
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "yellow",
        cex.names = 0.6)
# Black Spruce shows lower browsing intensity compared to Balsam Fir in most ecoregions, suggesting moose prefer Balsam Fir.

#Question 18
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()

#Question 19
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()

#Question 20
# Some ecoregions appear more sampled than others, this suggests the dataset may not be evenly distributed.
# Recognizing bias is important because uneven sampling couldf lead to incorrect conclusions about moose browsing preferences.

##Part 3 - Creating and Joining datasets

#Question 21a
moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)

#Question 21b
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

#Question 22
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(AvgBrowsing = mean(BrowsingScore), AvgDensity = mean(MooseDensity)) %>% print()

#Question 23
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,
               3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests",  "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25a
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)

#Question 25b
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

#Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Number of Collisions", main = "Moose Density vs Vehicle Collisions")

#Question 27
coll_merge_per_capita <- coll_merge %>%  mutate(coll_per_capita = collisions2020 / human_pop)

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population")

#Question 29
# The results say that areas with higher moose density have more vehicle collisions overall. However, when you adjust for collisions per capita, human population also plays a key role.
# Regions with a larger human population experience more encounters because there are more drivers on the road.
# So, both moose density and human population contribute to collision risk.















