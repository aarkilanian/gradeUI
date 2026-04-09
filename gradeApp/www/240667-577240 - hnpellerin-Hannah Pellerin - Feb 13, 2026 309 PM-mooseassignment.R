MooseData <- read.csv("MoosePopulation.csv")
Moose_clean <- na.omit(MooseData)
Moose_sel<- select(Moose_clean,Ecoregion,Year,Area,Estimated_Moose_Pop)
Year_min<- Moose_sel$Year
Moose_max<- Moose_sel$Estimated_Moose_Pop
Moosedata2<- mutate(Moose_sel,MooseDensity=Estimated_Moose_Pop/Area)
plot(Moosedata2$MooseDensity,
     xlab="Year",
     ylab="Moose per sq km",
     main="Moose density in Newfoundland ecoregions over time")
Moose_west<- filter(Moosedata2,Ecoregion=="Western_Forests")
plot(type="l",Moose_west$MooseDensity,
     xlab="Year",
     ylab="Moose Density in Western Forest",
     main="Moose density in Western Forest over time")
Moose2020<- Moosedata2%>% filter(Year == "2020")
Moose2020_high<- Moose2020 %>% filter(MooseDensity>"2.0")
arrange(Moose2020_high,desc(MooseDensity))
Moose_final<- Moosedata2 %>% 
  filter(Year == "2020") %>% 
  filter(MooseDensity>"2.0") %>% 
  arrange(desc(MooseDensity)) %>% 
  print()
SapData<- read.csv("SaplingStudy.csv")
sap_clean<- na.omit(SapData)
sap_reg_browse<- sap_clean %>% group_by(sap_clean$Ecoregion) %>% 
  summarize(mean(BrowsingScore)) %>% 
  print()
avg_browse_reg<- sap_clean %>% arrange(desc(BrowsingScore))
#Maritime_Barrens had lowest average browsing score, North_shore_forests had highest average browsing score
sap_height_reg<- sap_clean %>% group_by(sap_clean$Ecoregion) %>% 
  summarize(mean(Height)) %>% 
  print()
sap_reg_height_low <- sap_clean %>% filter(Height < "20") %>% 
  print()
#Ecoregions with average heights less than 20 cm include North shore forests, North peninsula forests, Central forests, and Western Forests
sap_spe_browse<- sap_clean %>% group_by(Species) %>% 
  summarise(mean(BrowsingScore)) %>% 
  print()
ave_browse_spe<- sap_clean %>% arrange(desc(BrowsingScore)) %>% 
  print()
#The willow and Alder share the highest browsing score (5.0) and Black spruce has the lowest (0.0)
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarise(avr_browsingscore = mean(BrowsingScore))
barplot(fir_reg_browse$avr_browsingscore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average browsing intensity", main = "Average browsing intensity across Ecoregions", col = "hotpink", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarise(Avg_Browsingscore = mean(BrowsingScore))
barplot(spruce_reg_browse$Avg_Browsingscore,names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Intensity", main = "Average browsing intensity across Ecoregions", col = "pink", cex.names = 0.6)
#Fir browsing is higher on average across ecoregions, with balsam browsing reaching 0 or almost 0 in both avalon forests and long range barrens.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>% 
  tally() %>% 
  print()
sap_spe_tally <- sap_clean %>% 
  group_by(Species) %>% 
  tally() %>% 
  print()
#I think that the sapling study could have been more evenly distributed, there was not an equal amount of plant species taken from each ecoregion, especially in the case of the North shore and Northern peninsula forests, which had a lot more samples taken compared to other ecoregions, and the Strait of belle isle barrens, which only had one sample taken.
#Its important to recognize bias in ecological datasets so that there is no scewed data when representing the ecoregions, if one forest has 100 samples taken from it and another only has 10, when comparing them, the results will not be accurate to represent the real life ecoregions.
Moose_2020b<- Moose_clean %>% filter(Year == "2020")
moose_sap <- left_join(Moose_2020b,sap_clean,by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- sap_clean %>% group_by(Species, Ecoregion) %>%
  summarise(mean(BrowsingScore, MooseDensity) %>% 
  print()
#Yes this graph supports the researchers hypothesis, as seen in the graph, when there is a lower moose density there is a a clear preference to Willow and Alder Browsing, and when there is a higher density the gap between browsing is much lesser.
#Based on the graph of Browsing Intensity Across Moose Density by Species, Moose browse the most on Willow, and browse the least on Black spruce.
#Black Ash is not shown on the graph done by the researchers, this is because the Browsing score of black ash lines up with the browsing score of Willow, and only one can be shown on the graph.
collisions2020 <- c(56,60,14,36,48,10,40,110,6)
human_pop <- c(18000,1200,4000,75100,24000,3500,32000,270000,2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll<- data.frame(collisions2020,human_pop,study_sites)
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
coll_merge <- left_join(Moose2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
plot(Moose2020$MooseDensity, collisions2020, xlab = "Moose Density", ylab = "Collisions")
#Based off of the graph, the higher the moose density is the more collisions will occur, this is pretty much the same across the graph with 1 outlier being a very high amount of collions (100 collisions) at a moose density of 1.0. 
coll_merge_per_capita <- coll_merge %>%  mutate(coll_per_capita = collisions2020/human_pop)
plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita, xlab = "human population", ylab = "collisions per capita")
#The graph displays the number of collisions per capita as very small all throughout, with almost no collisions after the human pop passes 100000. This has only a couple of outliers with one being a higher number of collisions (0.05) at a human pop of between 0 and 50000 and a small amount of collisions (0.00) surfacing past a human pop of 250000. This makes sense considering moose live in forests, and the more populated it is in newfoundland, the less forest there is, meaning there is less moose to hit in more populated areas.
