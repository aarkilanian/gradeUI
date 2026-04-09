#Biology1002_Ceili_Bennett
#q1
install.packages("dplyr")
library(dplyr)
#q1
moosedata <- read.csv("MoosePopulation 2.csv")
#q3
moose_clean <- na.omit(moosedata)
#q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#q5
moose_max<-max("Estimated_Moose_Pop")
#q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#q7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#q8 a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#q8 b)
plot(moosedata2$Year, moosedata2$MooseDensity, type="l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#q9 a)
moose_2020 <- moosedata2%>%
  filter(Year=="2020")
#q9 b)
Moose_2020_high<-moose_2020
  filter(MooseDensity==2.0 moose/km²)
#q9 c)
arrange(Moose_2020_high,desc(MooseDensity))
#q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#q11
install.packages("dplyr")
library(dplyr)
saplings<-read.csv("SaplingStudy 3.csv")
sap_clean<-na.omit(saplings)
#q12 a)
sap_reg_browse <- sap_clean%>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))
print(sap_clean)
print(sap_reg_browse)
#q12 b
avg_browse_reg<- arrange(sap_reg_browse,desc(mean_BrowsingScore))
avg_browse_reg<-rename(sap_reg_browse,BrowsingScore=mean_BrowsingScore)
#q13 a)
sap_reg_height<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarize(mean(Height))
print(sap_reg_height)
#q13 b)
sap_reg_height_low<-print(sap_reg_height>20)
print(sap_reg_height_low)
#There are two ecoregions that have average hights less than 20cm they are the Northern_Peninsula and also the Western_Forests
#q14 a)
sap_spe_browse<-sap_clean%>%
  group_by(Species)%>%
  summarise(mean_BrowsingScore=mean(BrowsingScore))
print(sap_spe_browse)
#14 b)
avg_browse_spe<- arrange(sap_spe_browse,desc(mean_BrowsingScore))
#The species that has the highest browsing score is Black_Ash and the lowest browsing score is Black_Spruce
#15
fir_reg_browse<-sap_clean%>%
  filter(Species =="Balsam_Fir")%>%
  group_by(Ecoregion)%>%
summarize(mean_browsingscore=mean(BrowsingScore))
#16
barplot(fir_reg_browse$mean_browsingscore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "mean(BrowsingScore)", main = "Average Balsom fir browsing by Ecoregion", col = "purple",cex.names = 0.6)
#17 a)
spruce_reg_browse<-sap_clean%>%
filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>%
summarise(mean_browsingscore=mean(BrowsingScore))
#b)
barplot(spruce_reg_browse$mean_browsingscore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "mean(BrowsingScore)", main = "Average Black Spruce browsing by Ecoregion", col = "purple",cex.names = 0.6)
#c)
#The North shore forest graph has the highest overall browsing score for both black spruce and balsom fur and some of the Balsom fur have very low Average browsing scoresbut with regards to the black spruce some have zero browsing scores
#q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#q19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#q20 a)
#The sapling study data setis not evenly distributed becuasesome ecoregions and tree species have many more saplings sampled than others which means they are overrepresented while others are underreprecented
#b)Recognizing biasin ecological datasets is important because uneven sampling can lead to incorrect conclutions about species abundance or browsing pressure. Bias can make some patterns appear stronger or weaker than they actually are in nature
#q21a)
moose_2020b <- moose_clean%>%
  filter(Year=="2020")%>%
  mutate(MooseDensity=Estimated_Moose_Pop)
#b)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#22
sum_spe_browse<-moose_sap%>%group_by(Species,Ecoregion)%>%
  summarise(mean_Browse =mean(BrowsingScore)%>%summarise(mean_density =mean(MooseDensity))%>%
print()
#23
library(ggplot2)
ggplot(sum_spe_browse, aes(x = mean_density, y = mean_Browse, color= Species)) + geom_point(size = 3)+theme_minimal() + labs(title = "Browsing Intensity Across Moose Density by Species",x = "Average Moose Density",y = "Average Browsing Score")
#a)
#There is partial evidence supporting theresesrchers hypothesis. At a lower moose density, browsing intensity differs more among tree species,suggesting selective feeding,while at higher moose densities browsing becomes more simmilar across species, this indicates more genralist feeding
#b
#The Moose appear to favor Willow the most,as it has the highest average browsing score.Black Spruce show the lowest browsing intensity, suggestingit is less perfered by moose
#c
#White spruce is notshown in the figure becuase it does not occur in all ecoregions or did not have enough observations after filtering averagingto appear in the plot
#q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#q25 a)
moose_coll2<-moose_coll%>%
  rename_with(Ecoregion = study_sites)
moose_coll2<-moose_coll%>%
  rename_with(~"Ecoregion", .cols ="study_sites")
#b)
coll_merge <- left_join(moose_coll2,moose_2020, by ="Ecoregion")
#26 a)
plot(coll_merge$MooseDensity,coll_merge$collisions2020)
#b)
#Ecoregions with higher moose density tend to have many more collisions but having saod that the pattern is not always perfect becuase there may be difrances where collisions are high with low density this could be becuase of higher trafic on the roads
#q27
coll_per_capita<-coll_merge%>%
  mutate(coll_per_capita=collisions2020/human_pop)
coll_merge_per_capita=coll_per_capita
#q28
plot(coll_per_capita$human_pop)
#q29
#The collisions per capita tend to be higher in smaller human populations and lower in larger populations. This makes sense with regards to Newfoundland becuase rural areas have lots of moose and some higer speed road but less people so the collisions per person are higher.
