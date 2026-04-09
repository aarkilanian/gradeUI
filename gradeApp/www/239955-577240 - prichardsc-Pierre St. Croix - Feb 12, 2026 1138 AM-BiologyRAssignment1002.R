install.packages("tidyverse")
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
View("MoosePopulation.csv")
na.omit(moosedata)
moose_clean <- na.omit(moosedata)
select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min<-min(moose_sel$Year)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
moosedata2<-mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
    xlab = "year", 
    ylab = "Moose per sq km", 
    main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(type = "line", moose_west$Year, moose_west$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland western forests over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > "2.0")
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
AverageBrowsing <- sap_reg_browse$`mean(BrowsingScore)`
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
#max: Northern_Peninsula_Forests (4.571429) min:StraitOfBelleIsleBarrens (1.000000)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(Height)) %>%
  print()
Height <- sap_reg_height$`mean(Height)`
sap_reg_height_low <- sap_reg_height %>%
  filter(Height < "20.00000") %>%
  print()
#Northern_Peninsula_Forests (19.91429) and Western_Forests (18.94000) have average mean heights less than 20cm
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
meanBrowsingScore <- sap_spe_browse$`mean(BrowsingScore)`
avg_browse_spe <- arrange(sap_spe_browse, desc(meanBrowsingScore))
#highest:Northern_Peninsula_Forests (4.571429) lowest:StraitOfBelleIsleBarrens (1.000000)
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))
AverageBrowsingIntensity <- fir_reg_browse$`mean(BrowsingScore)`
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "AverageBrowsingIntensity",main = "AvgBrowsingIntensityInDiffEcoregions", col = "forestgreen")
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "AverageBrowsingScore",main = "AvgSpruceBrowsingIntensityInDiffEcoregions", col = "forestgreen")
#The balsam fir browsing is much more even across the different ecoregions than the spruce browsing. The spruce browsing, however, has a higher maximum average browsing score with the Northern_Peninsula_Forests, having a score of 4.57.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally()
#Q20 a.) I don't think the dataset is evenly distributed, there is only 1 tree from the StraitOfBelleIsleBarrens which also happens to have the lowest average browsing score. clearly that area needs more study given how much it stands out. The North_Shore_Forests are also highly overrepresented, with 8.
#Q20 b.) It's important to recognize bias as scientific studies are meant to help understand objective reality. By recognizing biases, one can take the research with a grain of salt, and not treat it as objective.
moose_2020b<- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse<- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean(BrowsingScore), mean(moose_sap$MooseDensity)) %>%
  print()
#Q23 a.) Their hypothesis is supported, as with lower densities, Willows and Alders are heavily favored, while at higher densities, though Willows and Alders are still the highest, they are all at more comparable levels of browsing.
#Q23 b.) The moose favour the willow the most, and browse the Black_Spruce the least
#Q23 c.) The Black_Ash is not shown on the figure, likely because the data only had one black_ash, so there wasn't a big enough sample size to warrant including
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2<- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge<- left_join(moose_coll2, moose_2020, by = 'Ecoregion', relationship = "many-to-many")
plot.default(coll_merge$MooseDensity, coll_merge$collisions2020)
#Q26 The graph generally trends to higher collisions with higher density, besides the outlier of the Avalon_Forests region, with a much higher number of collisions than any other datapoint at such a low density.
coll_merge_per_capita<- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop)
#Q29 The human population tends to decrease with the collisions per capita. This trend makes sense, because less people around with usually mean more wilderness, especially in Newfoundland, and therefore would have a bigger moose population, increasing the chances of a collision significantly.