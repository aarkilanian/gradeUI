install.packages("dplyr")
moosedata <- read.csv("MoosePopulation.csv")
na.omit(moosedata)
moose_clean <- na.omit(moosedata)
library(dplyr)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > "2.0")
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()
#part2
saplings <- read.csv("SaplingStudy.csv")
na.omit(saplings)
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
avg_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowsing))
#northern_peninsula_forests has the highest and straitofbelleislebarrens has the lowest
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height)) %>% print()
sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20) %>% print()
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing))
#black_ash has the highest browsing score and black_spruce has the lowest
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "darkolivegreen",
        cex.names = 0.6)
#black spruce generally shows lower browsing intensity than balsam fir,showing moose prefer balsam fir across ecoregions
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# (A) The sapling data set is not evenly distributed, some ecoregions are over represented like northshore forests, while others are under represnted like maritime barrens.
# (B) Recognizing bias is important because uneven sampling can lead to misleading conclusions about browsing pressure and species preference.
#part3
library(dplyr)
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(AvgBrowsing = mean(BrowsingScore),AvgDensity = mean(MooseDensity))
#(question 23)Yes, the figure supports the hypothesis. At low moose densities (0.5–1.0), there is a wide range in browsing scores across species, indicating strong preferences, whereas at high densities (above 2.0), the scores for all species converge toward the maximum, showing a shift to generalist browsing as food resources become scarcer.
#Moose favour Willow and Alder the most, as these species have high browsing scores even at low moose densities. They browse Black Spruce and Balsam Fir the least, as indicated by their consistently low scores when moose density is low.
#Black Ash is not shown on the figure. This is likely because it was not present at the specific study sites sampled or it was never browsed, resulting in no data points for the researchers to plot.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>% rename_with(~"Ecoregion", study_sites)
coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,xlab = "Moose Density", ylab = "Number of Collisions (2020)", main = "Moose Density vs Moose-Vehicle Collisions")
# The trend I see is that higher moose density corresponds to a larger amount of collisions there is one outlier, where a lower moose density has the highest amount of collisions.
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,xlab = "Human Population",ylab = "Moose Collisions per Capita",main = "Moose Collisions per Capita vs Human Population")
#Regions with smaller human populations tend to have higher collisions per capita, likely because roads overlap more with moose habitat. This trend makes sense in Newfoundland, where rural areas have fewer people but relatively high moose densities and frequent road overlap with forests.
