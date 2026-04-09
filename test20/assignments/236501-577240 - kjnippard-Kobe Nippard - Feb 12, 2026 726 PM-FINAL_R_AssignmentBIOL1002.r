install.packages("dplyr")
library("dplyr")
library(readr)
MoosePopulation <- read_csv("~/Downloads/BIOL 1002 R/MoosePopulation.csv")
library(readr)
SaplingStudy <- read_csv("~/Downloads/BIOL 1002 R/SaplingStudy.csv")
View(MoosePopulation)
View(SaplingStudy)
MoosePopulation <- na.omit(MoosePopulation)
MoosePopulation <- select(MoosePopulation, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(MoosePopulation$Year)
#the oldest observation in the data set is from 1904
max(MoosePopulation$Estimated_Moose_Pop)
#the highest recorded moose population was 41250, and the ecoregion for this population was Central Forests
MoosePopulation <- mutate(MoosePopulation, MooseDensity = Estimated_Moose_Pop / Area)
plot(MoosePopulation$Year, MoosePopulation$MooseDensity, xlab = "Year",  ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
MooseDataWest <- filter(MoosePopulation, Ecoregion == "Western_Forests")
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, type = "l", xlab = "Year",  ylab = "Moose per sq km", main = "Moose density in Western Forest ecoregions over time")
MooseData2020 <- filter(MoosePopulation, Year == "2020")
MooseData2020b <- filter(MooseData2020, MooseDensity > 2.0)
arrange(MooseData2020b, desc(MooseDensity))
MooseData_final <- MoosePopulation %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()
#part 2
sap_clean <- na.omit(SaplingStudy)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
avg_browse_reg <- sap_reg_browse %>% arrange(desc(mean_browsing))
#the ecoregion with the lowest average browsing score was the Strait Of Belle Isle Barrens
#the ecoregion with the highest average browsing score was Northern Peninsula Forests
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(Mean_Height = mean(Height, na.rm = TRUE))
print(sap_reg_height)
sap_reg_height_low <- sap_reg_height %>% filter(Mean_Height < 20)
print(sap_reg_height_low)
#the two ecoregions that have average heights less than 20 cm are Northern Peninsula Forests (19.9 cm) and Western Forests (18.9 cm)
sap_spe_browse <- sap_clean %>%
group_by(Species) %>%
summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
avg_browse_spe <- sap_spe_browse %>%
arrange(desc(mean_BrowsingScore))
print(avg_browse_spe)
#the species with the highest browsing score is Black Ash (5) and the species with the lowest browsing score is Black Spruce (2.33)
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%       
group_by(Ecoregion) %>%                    
summarise(mean_browsing_score = mean(BrowsingScore, na.rm = TRUE)) 
barplot(fir_reg_browse$mean_browsing_score, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Browsing Intensity", main = "Browsing Intensity of Moose on Balsm Fir Trees by Ecoregion", col = "forestgreen", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))
barplot(spruce_reg_browse$mean_BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Browsing Intensity", main = "Browsing Intensity of Moose on Spruce Trees by Ecoregion", col = "forestgreen", cex.names = 0.6)
#the browsing intensity is the most for Spruce trees in the North Shore Forest and Northern Peninsula Forest ecoregions, and the least in Maritime Barrens and Eastern Hyper Oceanic Barrens ecoregions
#the browsing intensity is the most for Fir trees in the North Shore Forests ecoregion (same as the Spruce trees) and the least in the Long Range Barrens ecoregion
sap_reg_tally<- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>% 
print()
sap_spe_tally <- sap_clean %>%
group_by(Species) %>%
tally() %>% 
print()
#the sapling study data set is evenly distributed for the most part, however there are some places that it is not evenly distributed. There was only 1 Black Ash tree sampled, and 11 Balsam Fir Trees, so this could have been better distributed. The ecoregions were evenly distributed except for there only being 1 sample in the Strait of Belle Isle Barrens.
#it is important to recognize bias in ecological data sets because if there was a bias against or towards, for example, a particular ecoregion or sapling species, the data would not be accurate and would not be a good representation of the overall data that is trying to be collected.
#part 3
moose_2020b <- filter(MoosePopulation, Year == "2020")
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE), mean_MooseDensity = mean(MooseDensity, na.rm = TRUE),.groups = 'drop' ) %>% print()
#a) based on the graph, it appears that the moose show more of a variety in browsing preferences at low density, with many different sapling species and various browsing scores. At a higher density, it appears that the moose are less selective with their browsing and take a more generalist approach, which supports the researchers' hypothesis.
#b) the sapling species that the moose favour the most is the Willow, and the species that they browse the least is the Black Spruce.
#c) the sapling species that is not shown on the figure is Black Ash, and this is likely because there was very limited data for this species, with only one piece of data recorded, meaning it is not a reliable source of data and therefore was omitted from the graph.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
coll_merge <- left_join(MooseData2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Number of Collisions", main = "Comparing Number of Moose Collisions With Moose Density", col = "forestgreen")
#the general trend in the graph is that as moose density increases, so does the number of collisions. However, there is one outlier, as there are over 100 collisions at a density of only 1.0.
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions per capita", main = "Comparing Number of Moose Collisions per capita With Human Population", col = "forestgreen")
#based on the data trends from the graph, the number of moose collisions per capita is highest in locations with lower human populations. This may be because moose tend to spend more time in rural areas where there are more forests and wildlife, and also a smaller human population.
