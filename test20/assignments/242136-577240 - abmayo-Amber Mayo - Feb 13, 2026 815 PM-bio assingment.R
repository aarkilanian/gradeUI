install.packages("dplyr")
library("dplyr")
View(moosedata)
moose_clean<-na.omit(moosedata)
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_clean$Year)
year_min<-min(moose_clean$Year)
moose_max<-max(moose_clean$Estimated_Moose_Pop)
moosedata2<-mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
moose_west<-filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year",
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundlands western forests ecoregions over time")
moose_2020<-filter(moosedata2, Year == "2020")
moose_2020_high<-filter(moose_2020, MooseDensity == "<2.0")
arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
sap_clean<-na.omit(saplings)
sap_reg_browse<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(AverageBrowsing=mean(BrowsingScore))%>%
  print()
avg_browse_reg<-arrange(sap_reg_browse, AverageBrowsing)
View(avg_browse_reg)
#StraitOfBelleIsleBarrens is the lowest average browsing score, and the Northern_Peninsula_Forests is the highest score.
sap_reg_height<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarize(AverageHeight=mean(Height))%>%
  print()
sap_reg_height_low<-sap_reg_height%>%
  filter(AverageHeight<20)%>%
  print()
#Northern_Peninsula_Forests and Western_Forests both have heights that are less than 20cm.
sap_spe_browse<-sap_clean%>%
  group_by(Species)%>%
  summarise(AverageBrowsingSpe=mean(BrowsingScore))%>%
  print()
avg_browse_spe<-arrange(sap_spe_browse, AverageBrowsingSpe)
View(avg_browse_spe)
#Black_Spruce is the lowest browsing score, and Black_Ash is the highest.
fir_reg_browse<-sap_clean%>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(AverageBalsamBrowsing=mean(BrowsingScore))%>%
  print()
barplot(fir_reg_browse$AverageBalsamBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "ecoregions", ylab = "browsing score", main = "balsam fir browsing intensity in different ecoregions", col = "pink")
spruce_reg_browse<-sap_clean%>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(Averagesprucebrowsing=mean(BrowsingScore))%>%
  print()
barplot(spruce_reg_browse$Averagesprucebrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab ="ecoregions", ylab = "browsing score", main = "black spruce browsing intensity in different ecoregions", col = "hotpink")
#There's significantly less less black spruce browsing than balsam fir browsing happening in the maritime barrens and eastern hyper oceanic barrens.
sap_reg_tally<-sap_clean%>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()
sap_spe_tally<-sap_clean%>%  
  group_by(Species)%>%
  tally()%>%
  print()
#No, the SaplingStudy dataset is not evenly distributed.
#It is important to recognize bias in ecological datasets due to the fact that bias may distort results which can therefore lead to incorrect conclusions (over/underrepresentation).
moose_2020b<-moose_clean%>%
  filter(Year == 2020)%>%
  mutate(MooseDensity=Estimated_Moose_Pop)%>%
  print()
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
sum_spe_browse<-moose_sap%>%
  group_by(Species)%>%
  group_by(Ecoregion)%>%
  summarise(averagemoosebrowsing=mean(MooseDensity))%>%
  print()
#a Yes, there is evidence that supports the hypothesis. Browsing scores are varied more at low moose density, showing preference. At higher density, they are more clustered, suggesting generalist feeding.
#b The moose appear to favour Willow and Alder the most, and Black Spruce the least.
#c Black Ash is not shown due to the lack of browsing.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", .cols = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
length(coll_merge$collisions2020)
plot(coll_merge$collisions2020,coll_merge$Estimated_Moose_Pop,
     xlab = "Number of Moose-Vehicle Collisions (2020)",
     ylab = "Moose Density",
     main = "Moose Density vs Moose-Vehicle Collisions")
#One trend I've noticed is that when the number of collisions increases, so does the moose density. One outlier is that there is a very high density (~40000) around (~40).
coll_merge_per_capita<-coll_merge%>%
  mutate(coll_per_capita=collisions2020/human_pop)%>%
  print()
plot(coll_merge_per_capita$coll_per_capita,coll_merge_per_capita$human_pop,
     xlab = "Number of collisions per capita", 
     ylab = "Human population",
     main = "Number of collisions per capita vs Human population")
#The trend that I've noticed is that as the human population increases, the number of moose-vehicle collisions per capita decreases. Based on my knowledge, this makes sense due to the fact that moose are mostly found in rural/forested areas which usually surround smaller communities.