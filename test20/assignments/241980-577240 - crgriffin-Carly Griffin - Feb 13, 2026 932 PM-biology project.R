library("dplyr")
MoosePopulation <- read.csv("~/Downloads/MoosePopulation.csv")
MoosePopulation<-na.omit("~/Downloads/MoosePopulation.csv")
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min<-min(moose_sel$Year)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type="l",
     ylab="Moose Density per sq km",
     main="Moose density change over time in western forests")
moose_2020<-filter(moosedata2,moosedata2$Year==2020)
moose_2020_high<-filter(moose_2020, Ecoregion>2.0)
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
SaplingStudy <- read_csv("~/Downloads/SaplingStudy.csv")
sap_clean<-na.omit("~/Downloads/SaplingStudy.csv")
sap_reg_browse<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(MeanBrowsingScore=mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
avg_browsing<-aggregate(BrowsingScore~Ecoregion, data = sap_clean, FUN = mean)
#Northern_Peninsula_Forests has the highest average browsing score,StraitOfBelleIsleBarrens has the lowest average browsing score
sap_reg_height<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean_height = mean(Height, na.rm = TRUE))
print(sap_reg_height)
sap_reg_height_low<-sap_reg_height%>%
  filter(mean_height<20)%>%
  print()
#Northern_Peninsula_Forests and Western_Forests are the only ecoregions that have average heights less than 20
sap_spe_browse<-sap_clean%>%
  group_by(Species)%>%
  summarise(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))
avg_browse_spe<-sap_spe_browse%>%
  arrange(desc(mean_BrowsingScore))
#Northern_Peninsula_Forests has the highest browsing score,StraitOfBelleIsleBarrens has the lowest average browsing score
fir_reg_browse<-sap_clean%>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(mean_BrowsingScore = mean(BrowsingScore))
barplot(fir_reg_browse$mean_BrowsingScore,
        names.arg=fir_reg_browse$Ecoregion,
        xlab="Ecoregion",
        ylab="mean_BrowsingScore",
        col = "forestgreen",
        cex.names = 0.6)
fir_reg_browse<-sap_clean%>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(mean_BrowsingScore = mean(BrowsingScore))
barplot(spruce_reg_browse$mean_BrowsingScore,
        names.arg=spruce_reg_browse$Ecoregion,
        xlab="Ecoregion",
        ylab="mean_BrowsingScore",
        col = "purple",
        cex.names = 0.6)
#Black Spruce has has a mush lower brosing scores, meaning that moose might like Balsam Fir more than Black Spruce.
sap_spe_tally<-sap_clean%>%
  group_by(Species)%>%
  tally()
#the SaplingStudy dataset is not evenly distributed, the  StraitOfBelleIsleBarrens and Maritime_Barrens are underepresented in the dataset. North_Shore_Forests and Northern_Peninsula_Forests are overrepresented.
#it important to recognize bias in ecological datasets because it helps to make sure that any conclusions drawn from the data set are accurete. 
moose_2020b<-moose_clean%>%
  filter(Year==2020)%>%
  moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse<-moose_sap%>%
  group_by(Species, Ecoregion)%>%
  summarize(mean_BrowsingScore=mean(BrowsingScore,na.rm=TRUE),
            mean_moose_densityy=mean(moose_densityy,na.rm=TRUE))
#question 23 part a- yes, there is evidence that supports the researchers’ hypothesis that moose have a prefrence toward low density and moose do shift to more generalist browsing at higher density.
#question 23 part b- moose favor willow sapplings, they browse balssam the least.
#question 23 part- c all species in the chat and the figure are represented 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2<-moose_coll%>%
  rename(Ecoregion=study_sites)
coll_merge<-moose_coll%>%
  print()
coll_merge<-moose_coll2%>%
  left_join(moose_2020, by="Ecoregion")
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab="Moose Density",
     ylab="Collisions in 2020",
     main="Moose Density vs. Collisions in 2020")
#a trend is as moose density increases so do collisions, the value with 110 collisions and a moose density of 0.9619048 is an outlier.
coll_merge_per_capita<-coll_merge%>%
  mutate(coll_per_capita=collisions2020/human_pop)
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab="Human Population",
     ylab="Collisions per Capita",
     main="Scatterplot of Collisions per Capita va. Human Population")
#as human population increases collisions per capita decrease. this makes sense because in places with moore people you don't see as many moose, less moose in an area means less moose to have colisiions with. 

     
     
     

     
     
