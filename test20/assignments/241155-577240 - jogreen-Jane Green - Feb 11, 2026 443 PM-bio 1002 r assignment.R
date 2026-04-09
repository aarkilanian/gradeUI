#part 1

library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
year_max <-max(moose_sel$Year)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moosedata2$Year, moosedata2$MooseDensity,type="l",
     xlab = "year",
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time line graph")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moosedata2, MooseDensity > "2.0")
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#part 2

saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(mean(BrowsingScore)) %>% 
  print()
avg_browse_reg <- desc(sap_reg_browse$`mean(BrowsingScore)`)

#the highest score was in Western_Forests and was 4.5 and the lowest score was in StraitOfBelleIsleBarrens and was 1.0

sap_reg_height <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(mean(Height)) %>% 
  print()

#the ecoregions with an average height less than 20 cm are Northern_Peninsula_Forests and Western_Forests

sap_spe_browse <- sap_clean %>% 
  group_by(Species) %>% 
  summarize(mean(BrowsingScore)) %>% 
  print()
avg_browse_spe<- desc(sap_spe_browse$`mean(BrowsingScore)`)

#the highest score was Willow and was 4.31 and the lowest score was in Black_Spruce and was 2.33

fir_reg_browse <- sap_clean %>% 
  filter(Species=="Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean(BrowsingScore)) %>% 
  print()
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Browsing Score in Different Regions", col = "forestgreen",cex.names = 0.6)
spruce_reg_browse <- sap_clean %>% 
  filter(Species=="Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean(BrowsingScore)) %>% 
  print()
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Browsing Score in Different Regions", col = "forestgreen",cex.names = 0.6)

#in some regions there is no black spruce, while there's some balsam fir in every region on the graph

sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_reg_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#in ecoregions, the average is 5 but Maritime_Barrens and StraitOfBelleIsleBarrens are underrepresented slightly and North_Shore_Forests and Northern_Peninsula_Forests are over represented slightly. in species most fall between a range of 7 to 9 but Black_Ash are greatly underrepresented and Balsam_Fir are greatly over represented

#recognizing bias is important in regards of reading and understanding data. it can distort our information and by recognizing it, we can form conclusions that actually reflect real biological information

#part 3

moose_sap <- left_join(moose_2020, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>% 
  group_by(Species, Ecoregion) %>% 
  summarize(mean(BrowsingScore), mean(MooseDensity)) %>% 
  print()

#yes, it does support the hypothesis. when moose density is low, browsing is more concentrated, while when moose density is high, browsing is more generalized. 

#willow is the most favored with the highest browsing scores and black spruce is the least favored with the lowest browsing scores

#black ash isn't shown in the figure, which is likely to do with being so under represented

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", .cols = "study_sites")
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "moose-vehicle collisions",
     main = "Moose Density compared to moose-vehicle collisions scatterplot")

#there's a positive correlation between moose-vehicle collisions and moose density. there seems to be one out liar at 1.0 density and 110 moose-vehicle collisions

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions per Person",
     main = "Scatterplot of Collisions per Person vs Human Population")

#it's a negative relationship because as population increases, moose collisions per person decreases which makes sense because there will be more collisions per person in less populated rural areas, as they're more common and there's less people to spread them out around