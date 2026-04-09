install.packages("dplyr")
library(dplyr)
library(readr)
SaplingStudy <- read_csv("C:/Users/katie/Downloads/SaplingStudy.csv")
library(readr)
MoosePopulation <- read_csv("C:/Users/katie/Downloads/MoosePopulation.csv")
moosedata <- MoosePopulation
View(MoosePopulation)
moose_clean <- na.omit(moosedata)
View(moose_clean)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose Density", main = "Moose Density Change Over Time in Western Forests Region")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- SaplingStudy
sap_clean <- na.omit(saplings)
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(MeanHeight = mean(Height))
print(sap_reg_height)
#Western forests and Northern Peninsula Forests have average tree heights less than 20cm.
sap_reg_height_low <- filter(sap_reg_height, MeanHeight < 20)
print(sap_reg_height_low)
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(MeanBrowsingScore = mean(BrowsingScore))
print(sap_reg_height)
avg_browsing_spe <- arrange(sap_spe_browse, desc(MeanBrowsingScore))
print(avg_browsing_spe)
#Highest Browsing score: Black Ash, Lowest Browsing Score: Black Spruce
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(MeanBrowsingScore = mean(BrowsingScore))
barplot(fir_reg_browse$MeanBrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Balsom Fir Browsing Intensity by Ecoregion", col = "maroon", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(MeanBrowsingScore = mean(BrowsingScore))
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(MeanBrowsingScore = mean(BrowsingScore))
barplot(spruce_reg_browse$MeanBrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Black Spruce Browsing Intensity by Ecoregion", col = "violet", cex.names = 0.6)
#Balsam Fir have a higher average browsing intensity than Black Spruce. This suggests that moose prefer Balsam Fir trees over Black Spruce.
sap_reg_tally<- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     tally() %>% 
  +     print()
sap_spe_tally<- sap_clean %>%
  +     group_by(Species) %>%
  +     tally() %>% 
  +     print()
#The sapling study dataset is not evenly distributed. Black Ash trees are underrepresented with only 1 sample and Balsam Fir trees are over represented with 11 samples.
#It is important to recognize bias in ecological datasets because uneven data distribution can negatively affect how the data is interpreted, yielding inaccurate conclusions.
moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(AverageBrowsingScore = mean(BrowsingScore), AverageMooseDensity = mean(MooseDensity))
#Yes, there is evidence that supports the researchers' hypothesis. Average browsing score is generally higher at higher average moose density, this suggests that moose are less selective when the population density is higher as predicted.
#As shown in the data, moose favour Willow saplings the most and browse Black Spruce the least. Willow saplings generally have a higher browsing score while Black Spruce have a lower browsing score.
#Black ash saplings are not shown on the figure because the dataset only had one sample of Black Ash which is not sufficient data to compare results. 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', relationship = "many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Population Density", ylab = "Moose-Vehicle Collisions", main = "Moose Population Density vs Vehicle Collisions")
#The data shows that a higher moose density does correlate to a greater amount of moose-vehicle collisions. There is an outlier in the data of 110 collisions in the Avalon Forests study site with a relatively low moose population density. This is most likely due to its high human population of 270000.
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions Per Capita", main = "Collisions Per Capita vs Human Population")
#The data shows that Collisions per capita are higher in areas with a lower human population. This makes sense as areas with a lower human population are generally more rural and therefore have a higher population of moose, increasing the possibility of moose-vehicle collisions.