install.packages("dplyr")
library("dplyr")
View(moosedata)
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_clean$Year)
year_min<-min(moose_clean$Year)
moose_max<-max(moose_clean$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundlands western forests ecoregions over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high<- filter(moose_2020, MooseDensity == "<2.0")
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
veiw(avg_browse_reg)
#the lowest avg browsing score is StraitOfBelleIsleBarrens and the highest score was Northern_Peninsula_Forests

sap_reg_height<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarize(AverageHeight=mean(Height))%>%
  print()
sap_reg_height_low<-sap_reg_height%>%
  filter(AverageHeight<20)%>%
  print()
#Northern_Peninsula_Forests and Western_Forests have heights less then20cm

sap_spe_browse<-sap_clean%>%
  group_by(Species)%>%
  summarise(AverageBrowsingSpe=mean(BrowsingScore))%>%
  print()
avg_browse_spe<-arrange(sap_spe_browse, AverageBrowsingSpe)
veiw(avg_browse_spe)
#the lowest browsing score is Black_Spruce and the highest is Black_Ash

fir_reg_browse<-sap_clean%>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(AverageBalsmBrowsing=mean(BrowsingScore))%>%
  print()
barplot(fir_reg_browse$AverageBalsmBrowsing, names.arg = fir_reg_browse$Ecoregion,xlab = "ecoregions", ylab = "browsing score", main = "balsam fir browsing intensity in different ecoreions", col = "blue")

spruce_reg_browse<-sap_clean%>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(Averagesprusebrowsing=mean(BrowsingScore))%>%
  print()
barplot(spruce_reg_browse$Averagesprusebrowsing, names.arg = spruce_reg_browse$Ecoregion,xlab = "ecoregions", ylab = "browsing score", main = "black spruce browsing intensity in different ecoreions", col = "pink")
#there are a lot less black srpuce browsing happening in the maritime barrens and eastern hyper oceanic barrens then balsam fir browsing

sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

sap_spe_tally<-sap_clean%>%
  group_by(Species)%>%
  tally()%>%
  print()

#no the sapling sturdy id not evenly distributed, one example of why this is becasue there are five data samples from central forests and only one from the barrens of bell island
#it is important to recognize bias when analzying data because if you recognizes them and still contiune reaseach it would be unethical statists and not leave to accurte conculsion to the eperiment

moose_2020b<-moose_clean%>%
  filter(Year == 2020)%>%
  mutate(MooseDensity=Estimated_Moose_Pop)%>%
  print()
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

sum_spe_browse<-moose_sap%>%
  group_by(Species)%>%
  group_by(Ecoregion)%>%
  summarise(averagemoosebrosing=mean(MooseDensity))%>%
  print()
#a yes, at lower populations the moose show more of a strong perfernce, perffering willow and alder the most, then at higher population there perfernces is still visible but less pronauced 
#b moose browse willow the most while also browsing alder a lot too. the least browsed saplings inculed black spruce and black ash
#c the sapling that is not shown on the figure is blackash, due to the reason that there was little to no braising on these species 

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", .cols = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
length(coll_merge$collisions2020)

plot(coll_merge$collisions2020,coll_merge$Estimated_Moose_Pop,
     xlab="Number of Moose-Vehicle Collisions (2020)",
     ylab="Moose Density",
     main = "Moose Density vs. Moose-Vehicle Collisions")
#the trend that i noticed is that the higher the population is the more collison occured, and there are two outliars that  see

coll_merge_per_capita<-coll_merge%>%
  mutate(coll_per_capita=collisions2020/human_pop)%>%
  print()
  
plot(coll_merge_per_capita$coll_per_capita,coll_merge_per_capita$human_pop,
     xlab = "#of collions per capita",
     ylab = "human population",
     main = "number of colliosions per capita vs human population")
#the trend that i notice is that the higher the population the more collisions per capita   

