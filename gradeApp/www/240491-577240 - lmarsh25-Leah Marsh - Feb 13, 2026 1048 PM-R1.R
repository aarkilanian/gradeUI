install.packages("dplyr")
library("dplyr")
moosedata<-read.csv("moosepopulation.csv")
View(moosedata)
View(MoosePopulation)
moose_clean<-na.omit(MoosePopulation)
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop / Area)
year_min<-min(moose_sel$Year)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
moosedata2<-mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "moose per square km", main = "Moose Density in Newfoundland Ecoregions Over Time", type = "l")
moose_west<-filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per square km", main = "Moose Density in Newfoundland Western Forests Over Time")
moose_2020<- filter(moosedata2, Year == "2020")
moose_2020_high<- filter(moosedata2, Year == "2020", MooseDensity > 2)
moose_2020_high_byD<- arrange(moose_2020_high, desc(MooseDensity))
moosefinal<- moosedata2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>%arrange(desc(MooseDensity)) %>% print()
#part 2
saplings <-(SaplingStudy)
View(saplings)
sap_clean<- na.omit(saplings)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarise(BrowsingScore = mean(BrowsingScore)) %>% print()
avg_browse_reg <- arrange(sap_reg_browse, desc(BrowsingScore))
print(avg_browse_reg)
#the northern peninsula had the highest average browsing score
#the strait of belle isle barrens had the lowest average browsing score
sap_reg_height <- sap_clean
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarise(height = mean(height)) %>% print()
sap_reg_height_low <- filter(sap_reg_height, height < 20)
print(sap_reg_height_low)
# the northern and western forests have average hights lower than 20
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarise(BrowsingScore = mean(BrowsingScore)) %>% print()
avg_browse_spe <- arrange(sap_spe_browse, desc(BrowsingScore))
print(avg_browse_spe)
#black ash has the highest browsing score while black spruce has the lowest
fir_reg_browse <- sap_clean %>% filter(Species =="Balsam_Fir") %>% group_by(Ecoregion) %>% summarise(BrowsingScore = mean(BrowsingScore))
barplot(fir_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Score of the Balsam Fir by Ecoregion", col = "blue", cex.names = 0.6
spruce_reg_browse<- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarise(BrowsingScore = mean(BrowsingScore))
barplot(spruce_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Score of the Black Spruce by Ecoregion", col = "pink", cex.names = 0.6
#the black spruce has a lower browsing score than the balsam fir in all ecoregions, but has a higher score in the western forests
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()
#the "n" shows that a different amount of trees were counted in each ecoregion
sap_reg_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()
# the "n" shows a different amount of saplings were collected for each species
# the sapling study is not evenly distributed. there are different amounts of trees counted in each region and different amounts of samples collected
#its important to notice bias in ecological datasets because unequal numbers collected reduce the accuracy of the data
#part 3
moose_2020b <- moose_clean %>% filter(Year == "2020") %>% mutate(MooseDensity = Estimated_Moose_Pop/Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarise(BrowsingScore = mean(BrowsingScore), MooseDensity = mean(MooseDensity) %>% print()
#Yes, the figure supports the researchers’ hypothesis. At low moose density, browsing is concentrated mainly on preferred species like willow and alder, while at higher densities browsing scores increase across more species
#Moose appear to favour willow the most followed by alder. They browse black spruce the least, as it shows the lowest average browsing scores
#Black ash is not shown in the figure because there are no visible data points for it, likely due to little or no recorded browsing 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_forests", "Long_Range_Barens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Bareens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, type = "p", xlab ="Moose Density", ylab = "Collisions in 2020", main = "Collisions in 2020 vs Moose Density")
# based on the scatterplot graph, the locations with a higher moose population showed more crashes in 2020.
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020/human_pop)
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop, type = "p", xlab = "Collisions in 2020", ylab = "Human Population", main = "Collisions per capita vs Population")
getwd()
