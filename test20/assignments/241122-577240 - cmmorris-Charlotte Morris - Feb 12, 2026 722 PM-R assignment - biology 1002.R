install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
View(moosedata)
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type = "l", 
     xlab = "year", 
     ylab = "Moose density (moose per km squared)", 
     main = "Moose density over time in western forests")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0 )
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
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowsing))
#12b) Highest browsing = first row
#Lowest browsing = last row 
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
#13b) Ecoregion below have average sampling heights < 20 cm 
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
average_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
#14b) Highest browsing = first row
#Lowest browsing = last row 
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion", ylab = "Average Browsing Intensity", 
        main = "Balsam Fir browsing Intensity by Ecoregion", 
        col = "forestgreen", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion", ylab = "Average Browsing Intensity", 
        main = "Black Spruce browsing Intensity by Ecoregion", 
        col = "blue", cex.names = 0.6)
#17c) Balsam fir graph shows a higher Average Browsing Intensity then the black spruce graph.
#This could suggest that the moose would perfer browsing the balsm fir over the black spruce.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#19) The number of saplings vary among different Ecoregions 
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#20a) The same number of tree saplings were not counted for each species
#some species were sampled more than others.
#The SaplingStudy set is not evenly distributed because some Ecoregions and tree species,
#have more sampled saplings than others. This implies some regions or species may be 
#over represented, while some others may be under represented.
#20b) Sampling bias in this circumstance matters because it can cause a false conclusion of results for,
#moose browsing behavior. If some Ecoregions or tree saplings have more sampling done on them, false 
#results may appear that moose perfer that area over the other when it is really just a result of 
#uneven sampling. 
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity))
print(sum_spe_browse)
#23a) Yes there is evidence that supports the hypothesis, at low moose densities browsing is selective.
#at high moose densities browsing scores are less selective, they consume most sapling species.
#23b) Moose seem to favour Willow sapling species the most, Willow has the highest browsing score.
#they seem to browse the Black Spruce the least, has the lowest browsing score.
#23c) Black Ash is not shown on the figure because moose do not browse these saplings.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions",
     main = "Moose Density vs Moose-Vehicle Collisions")
#26b) I see a relationship between the moose density and the number of moose-vehicle collisions.
#When moose density is high, there is a trend of higher moose-vehicle collisions.
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
#27 the long range barrens have the highestest number of moose collisions per person because
#there was 14 collsions in 2020 with a population of only 4000.
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions Per Capita",
     main = "Moose Collisions per Capita vs Human population")
#29 the trends i see are negative relationships between human population size and moose collisions per capita.
#Less populated regions are showing signs of higher per person collision rates. This would make sense with 
#my knowledge of human and moose population in Newfoundland, rural areas tend to have more moose habitat and highways
#which are constantly being drove on. This will cause higher rates of collision per person. 
