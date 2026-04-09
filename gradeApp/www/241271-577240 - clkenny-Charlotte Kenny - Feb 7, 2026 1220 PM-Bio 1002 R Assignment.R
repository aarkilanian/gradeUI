#Biology1002_CharlotteKenny
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just dplyr:
install.packages("dplyr")
library(dplyr) 
Moosedata <- read.csv("MoosePopulation.csv")
Moosedata <- na.omit(Moosedata)
View(Moosedata)
moose_clean <- na.omit(Moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
Year_<-min(1904)
Moose_max<-max(41250)
Moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(Moosedata2$Year, Moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(Moosedata2, Ecoregion == "Western_Forests")
plot(type="l",Moosedata2$Year, Moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_2020<-Moosedata2 %>%  filter(Year==2020)
moose_2020_high<-Moosedata2 %>%  filter(MooseDensity>=2.0)
moose_2020_high_byD<-arrange(Moosedata2, desc(MooseDensity))
moosefinal <- Moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_reg_browse)
avg_browse_reg<-sap_reg_browse %>% arrange(desc(AverageBrowsing))
#Highest_Average_Browsing=Northern_Peninsula_Forests_
#Lowest_Average_Browsing=StraitOfBelleIsleBarrens
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>% 
  summarize(AverageHeight = mean(Height))
print(sap_reg_height)
sap_reg_height_low<-sap_clean %>%  filter(Height<=20)
print(sap_reg_height_low)
#The_following_ecoregions_have_average_heights_<20cm:North_Shore_Forests,Northern_Peninsula_Forests,Central_Forests,Western_Forests
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_spe_browse)
avg_browse_spe<-sap_spe_browse %>% arrange(desc(AverageBrowsing))
#Black_Ash_had_the_highest_browsing_score
#Black_Sprucehad_the_lowest_browsing_score
fir_reg_browse <- sap_clean %>%
  filter(Species=="Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(fir_reg_browse$AverageBrowsing,names.arg = fir_reg_browse$Ecoregion,, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Browsing Intensity of Balsam Fir in Different Ecoregions ",col= "purple",cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species=="Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(spruce_reg_browse$AverageBrowsing,names.arg = spruce_reg_browse$Ecoregion,, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Browsing Intensity of Black Spruce in Different Ecoregions ",col= "blue",cex.names = 0.6)
#Balsam Fir has a significantly higher browsing intensity than Black Spruce in Avalon forests, Central Forest, Eastern Hyper Ocean Barrens and Martime Barrens. They have close to equal intensity in Northern Penisula forests. Western forests are observed for Black Spruce but not for Balsam Fir. 
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
# The Sapling Study dataset is not evenly distributed: some ecoregions (North_Shore_Forest) and species, (Balsam_Fir) are overrepresented, while StraitOfBelleIsleBarrens and Black_Ash are underrepresented.
# Recognizing bias in ecological datasets is important because uneven sampling can lead to incorrect conclusions about species abundance, habitat health, or environmental trends.
moose_2020b<-moose_clean %>%  filter(Year==2020)
moose_2020b <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area) %>% print()
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse<- moose_sap %>%
  group_by(Species, Ecoregion) %>% 
  summarize(
    mean_BrowsingScore=mean(BrowsingScore),
    mean_MooseDensity=mean(MooseDensity))%>% print()
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#Yes, the figure suggests moose show more selective browsing at low density and broaden their diet at higher density, supporting the hypothesis of a shift toward more generalist feeding as competition increases.
#Moose appear to favour Willow and Balsam_Fir the most (higher browsing scores), while Black_Spruce and White_Birch tend to be browsed the least overall.
#Black_Ash is not shown in the figure because there is likely too little data to calculate a reliable average browsing score.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
rename_with(moose_coll2=moose_coll)
moose_coll2<-moose_coll
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', relationship = "many-to-many")
moose_coll2<-moose_coll %>% 
  rename(Ecoregion=study_sites)
#They don't have the same column, that's why they do not work. Moose 2020 uses ecoregion and moose_coll uses study_sites
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab="Moose Density (Moose/km^2)",ylab="Collisions (2020)",main="Relationship between Moose Population Density and Moose-Vehicle Collisions 2020",pch=19)
#There is a positive trend indicating that moose-vehicle collisions increase as moose density rises 
#A noticible outlier appears around a density of 1 moose/km^2 with an unusually high number of collisions compared to the other points 
coll_per_capita <- mutate(moose_coll2, coll_per_capita = collisions2020 / human_pop)
plot(coll_merge$human_pop, coll_merge$coll_per_capita, xlab="Human Population",ylab="Collisions per Capita",main="Relationship between Human Population and Collisions per Capita",pch=19)
# There is no clear trend between human population size and collisions per capita, and one point stands out as an outlier. 
# This makes sense because collisions are more influenced by moose density and road conditions in rural areas, where there are fewer people but more driving through moose habitat.