#Biology1002_Gracie_Fudge
install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
Year_<-(1904)
moose_max<-(1960)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(type = "l",moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_2020 <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_2020_high <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_2020_high_byD <-arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
sap_clean<-na.omit(SaplingStudy)
sap_reg_browse<-sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(averagebrowsing=mean(BrowsingScore)) %>% 
  arrange(desc(averagebrowsing))
sap_reg_browse
avg_browse_reg<-sap_reg_browse %>% arrange(desc(averagebrowsing))
#The highest average browsing score is Northern Peniunsula forests 
#The lowest average browsing score is Strait of Belle Isle barrens
sap_reg_height<- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageHeight=mean(Height))
print(sap_reg_height)
sap_reg_height_low<-sap_clean %>% filter(Height<=20) %>%
  arrange(desc(Height)) %>%
  print()
#Central, Western, North Shore and Northern Penuinsula 
sap_spe_browse<-SaplingStudy %>% 
  group_by(Species) %>% 
  summarize(meanBrowseScore= mean(BrowsingScore)) %>% 
  print()
#The species with the highest browsing is Alder
#The species with the lwoest browsing is Willow
fir_reg_browse<-SaplingStudy %>% 
filter(Species=="Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarize(meanBrowsingScore=mean(BrowsingScore)) %>% 
print()
barplot(fir_reg_browse$meanBrowsingScore,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion", ylab = "BrowsingScore",
        main = "Browsing Scores on Balsam Fir's", col = "pink", cex.names = 0.6)
spruce_reg_browse<-SaplingStudy %>% 
  filter(Species=="Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(meanBrowsingScore=mean(BrowsingScore)) %>% 
  print()
barplot(spruce_reg_browse$meanBrowsingScore,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion", ylab = "BrowsingScore",
        main = "Browsing Scores on Black Spruce", col = "blue", cex.names = 0.6)
#The browsing scores on Balsam Fir's have a higher score in the North Shore, Northern Peninsula Forests, Maritime Barrens, Eastern Hyper Oceanic Barrens and the Avalon Forests than The Black spruce browsing scores. 
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_reg_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#The Sapling Study is not evenly distributed. Eco regions that are overrepresented is North Shore Forests and Northern Peninsula Forests in both tree species. Eco regions that are underrepresented is Eastern Hyper Oceanic Barrens in Black Spruce species and Long Range Barrens in Balsam Fir's. 
#It is important to recognize bias in ecological datasets because it can distort patterns and lead to incorrect conclusions about Eco regions. Identifying bias improves the accuracy and reliability of ecological decisions. 
moose_2020b<- moose_clean %>% 
  filter(Year==2020) %>% 
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse<- moose_sap %>% 
  group_by(Species,Ecoregion) %>% 
  summarize(AvgBrowsing=mean(BrowsingScore),
            AvgDensity=mean(MooseDensity)) %>% 
  print()
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# Moose shows a strong preference at low density because with a lower moose density, browsing becomes higher creating less competition between moose. 
#The sapling species that moose favor the most is Black Spruce, White Birch and Willow. The sapling species that they browse the least would be Alder.
#The sapling species that is not shown on the figure is Black Ash. This could be because the geographical location is different. 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2<- moose_coll %>% 
rename(Ecoregion=study_sites)
coll_merge <- left_join(moose_2020b, moose_coll2 , by = 'Ecoregion')
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density", 
     ylab = "Moose collisions", 
     main = "Moose collisions per Density")
#The higher the moose density increases the moose collisions. An outlier shown presents where 100 moose collisions happened at a moose density of 1.0 which does not follow the trends of the scatter plot. 
coll_merge_per_capita<- coll_merge %>% 
  mutate(coll_per_capita=collisions2020/human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Pop", 
     ylab = "Collisions per Person ", 
     main = "Collisions per capita verse Human Pop")
#The trends show that with lower human population the collision per person increases. This does not make sense because there's less people so the collision rate should be lower.









