library(dplyr)
moosedata<-read.csv("MoosePopulation.csv")
View(moosedata)
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year) 
# Minimum year = 1904
moose_max <-max(moose_sel$Estimated_Moose_Pop) 
# Max Moose = 41250
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,type ="l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose Density in Western Forests Over Time")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange (moose_2020_high, desc (MooseDensity))
moosefinal <- moosedata2 %>% filter (Year == 2020) %>% filter (MooseDensity > 2.0) %>% arrange (desc(MooseDensity)) %>% print()

# Part II: Tree Sapling Study
saplings <- read.csv ("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize (AverageBrowsing = mean (BrowsingScore)) %>% print()
avg_browse_reg <- sap_reg_browse %>% arrange (desc(AverageBrowsing)) %>% print()
#Highest Browsing = Northern_Peninsula_Forests
#Lowest Browsing = StraitOfBelleIsleBarrens
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height)) %>% print()
sap_reg_height_low <- sap_reg_height %>% filter (AverageHeight < 20) %>% print()
# Ecoregions with heights < 20cm: Northern Peninsula Forests and Western Forests
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing)) %>% print()
# Highest Browsing: Black Ash
# Lowest Browsing: Black Spruce
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print ()
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing", main = "Balsam Fir Browsing by Ecoregion", col = "forestgreen", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize (AverageBrowsing = mean(BrowsingScore)) %>% print ()
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab ="Ecoregion", ylab = "Average Browsing", main = "Black Spruce Browsing by Ecoregion", col = "forestgreen", cex.names = 0.6) 
# The Black Spruce and Balsam Fir both showed similar patterns for browsing, with both of their highest averages being in the north. However, the Black Spruce had ecoregions with little to no browsing, suggesting that moose may prefer Balsam Fir in those regions.         
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally () %>% print()
sap_sep_tally <- sap_clean %>% group_by(Species) %>% tally () %>% print()
# The SaplingStudy dataset appears to be not evenly distributed. Some ecoregions such as North_Shore_Forests and Northern_Peninsula_Forests are overrepresented, while StraitOfBelleIsleBarrens is highly underrepresented. As well, some species, ex. Balsam_Fair, are sampled more than others, ex. Black_Ash.
# Recognizing bia in ecological dataset is important because this leads to false results and wrong conclusions.

# Part III: Creating and Joining Datasets
moose_2020b <- moose_clean %>% filter(Year == "2020") %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore), AverageDensity = mean(MooseDensity), .groups = "drop") %>% print()
#Question-23a: There is some support to the researchers' hypothesis. At lower densities, browsing seems more selective across the species but at higher densities, browsing is more similar across most species, suggesting preference when at low densities and more generalized with high densities.
#Question-23b: Moose appear to favor Willow and Alder, while Black spruce have a lower average browsing.
#Question-23c: Black Ash is not shown on the the figure because it only had one observation so making an average for it would be impossible.
collisions2020 <-c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 751000, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninusula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <-data.frame(collisions2020, human_pop, study_sites)                 
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "2020 Collisions", main = "Moose Density vs. 2020 Moose Collisions")
# There seems to be a relationship where regions with high moose density have more collisions. Although, when the density is 1.0 it has a high collision compared to 2.5 but it could be due to an increase in human population and the amount of traffic.
col_merge_per_capita <- coll_merge %>% mutate (col_merge_per_capita = collisions2020 / human_pop)
plot(col_merge_per_capita$human_pop, col_merge_per_capita$col_per_capita, xlab = "Human Population", ylab = "Collison Per Capita", main = "Moose Collisions Per Capita vs. Human Population")
# There does not to seem to be a highly consistent trend but in areas with smaller populations, there seems to be more collisions which makes sense because rural areas have fewer people so more moose would habitat the area and if they're crossing roads, there is an increase in the risk for collisions.

