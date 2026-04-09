#1
install.packages("dplyr")
library(dplyr)

#2
#imported using import function 

#3
moose_clean <- na.omit(Moosedata)

#4
Moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop )

#5,a
min(Moose_sel$Year) 
#year is 1904 oldest

#5,b
Year_min <-min(Moose_sel$Year)
max(Moose_sel$Estimated_Moose_Pop) 
#Answer is 41250

#6
Moosedata2 <- mutate(Moose_sel, MooseDensity= Estimated_Moose_Pop/Area )

#7
plot(Moosedata2$Year, Moosedata2$MooseDensity, xlab = "Year", ylab = 
"Moose per Sq Kilometer", main = "Moose density in newfoundland ecoregions over time ") 

#8,a
Moose_west <- filter(Moosedata2, Ecoregion=="Western_Forests" )

#8,b
plot(Moosedata2$Year, Moosedata2$MooseDensity,pch= 17, type = "l",
xlab ="Year", ylab="Moose per sq Kilometer", 
main= "Moose density in newfoundland ecoregions over time" )

#9,a
Moose2020 <- filter(Moosedata2, Year=="2020")

#9,b
moose_2020_High <- filter(Moose2020, MooseDensity > 2)

#9,c
arrange(moose_2020_High, desc(MooseDensity))

#10
moosefinal <- Moosedata2 %>% filter(Year==2020) %>% filter(MooseDensity >2) %>% arrange(desc(MooseDensity)) %>% print()

#11,a/b
Sapling Study file was imported using import function
Sap.clean <-na.omit(Sapplings)

#12,a
Sap.clean %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_Browsing=mean(BrowsingScore)) 
print()
#12,b
#Northern_Peninsula_Forest had the highest browsing score of 4.38,
#StraitOfBellleIsleBarrens had the lowest BrowsingScore which was 1.

#13,a
Sap.clean %>%
  group_by(Ecoregion) %>% 
  summarize(mean_Height=mean(Height)) 

#13,b
#Western_forests and Northern_Peninsula_Forests have heights less than 20cm.

#14,a
Sap.clean %>% 
  group_by(Species) %>% 
  summarize(mean_Browsing=mean(BrowsingScore)) 
print()

#14,b
#Black_Spruce has the lowest browsing score and Black_Ash has the highest browsing score.

#15
 Reg.fir.Browse <- Sapplings%>% 
  filter(Species=="Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_browsing=mean(BrowsingScore)) %>%  
print()

#16
barplot(Reg.fir.Browse$mean_browsing,names.arg = Reg.fir.Browse$Ecoregion,
        xlab = "Ecoregion", 
        ylab= "Mean Browsing Score",
        Main= "Balsam Fir browsing intensity by ecoregion", 
        col="forestgreen", cex.names=0.6)

#17,a
BlackSpruce <- Sapplings %>% 
  filter(Species=="Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_browsing=mean(BrowsingScore)) %>% 
  print()

#17,b
barplot(BlackSpruce$mean_browsing,
        names.arg = BlackSpruce$Ecoregion,
        xlab="Ecoregion",
        ylab = "Mean Browsing Score",
        main = "Black Spruce browsing intensity by Ecoregion",
        col = "forestgreen",
        cex.names=0.6)

#17,c       
#Balsim Fir has higher Browsing Scores than Black Spruce in the majority of ecoregions,
#Balsim Fir also has a more consistient range of browsing as compared to that of Black Spruce

#18 
sap_reg_tally <- Sapplings %>% 
      group_by(Ecoregion) %>% 
      tally() %>% 
      print()

#19
sap_spe_tally <-Sapplings %>% 
  group_by(Species) %>% 
  tally() %>% 
  print()

#20,a
#The SaplingStudy dataset is not evenly distributed. Black_ash has only 1 entry
#compared to BalsimFir which has 11 enteries. This is uneven distribution.

#20,b
#It is important to recognize bias within datasets as this allows us to make approperate adjustments to data
#in order to get an accurate understanding of whatever data it is we are working with. Sample sizing and even distrbution 
#of parameters can then be done to ensure an accurate and proportional dataset.

PART 3
#21,a
Moosedata_2020b <- Moosedata %>% 
  filter(Year==2020) %>% 
  mutate(MooseDensity=Estimated_Moose_Pop/ Area)
view(Moosedata_2020b)

#21,b
moose_sap <- left_join(Moosedata_2020b, Sapplings, by ="Ecoregion", relationship = "many-to-many")

#22
sum_spe_browse <- moose_sap %>% 
  group_by(Species, Ecoregion) %>% 
  summarize(mean_browsing= mean(BrowsingScore),
            mean_density= mean(MooseDensity)) %>% 
  print()
#23
library(ggplot2)
ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#23,a
#cannot get this plot to load. Even when importing 
#the code used in the assignment using copy and paste.

#Yes there is evidence that supports this hypothesis. At low densitys
#there is a much wider browsing score range, however, at high densitys
#moose tend to have a more clustered browsing score which would average
#out to a more generalist behavior as compared to lower densitys.

#23,b
#the moose favour the willow the most with consistently high scores 
#across all densitys, while black spruce is the least browsed with 
#scores averaging out to nearly zero.

#23,c
#Black ash was not shown on the figure due to the fact that it only 
#had a singular entry in the dataset. This is not enough of a sample
#size to be able to accuratly portray this data.

#24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
"Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
"Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

#25,a/b
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- data.frame(study_sites,human_pop,collisions2020)
moose_coll2 <- moose_coll2 %>% 
  rename(Ecoregion = study_sites)
coll_merge <- left_join(Moose2020, moose_coll2, by = ("Ecoregion")

  
                     

                        