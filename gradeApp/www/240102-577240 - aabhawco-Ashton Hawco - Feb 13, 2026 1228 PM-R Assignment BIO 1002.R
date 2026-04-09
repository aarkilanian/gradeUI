install.packages("dplyr")
moosedata <- read.csv("MoosePopulation.csv")
library(dplyr)
View("moosedata")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
Year_min  <- min(moose_sel$Year)
Moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year,moose_west$MooseDensity,type ="l",xlab ="Year",ylab ="Moose per sq km",main="Moose density in Newfoundland western Forests over time") 
moose_2020 <- filter(moosedata2,Year=="2020")
moose_2020_high <- filter(moose_2020,MooseDensity >2)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
# Load Data: 
sapling <- read.csv("Saplingstudy.csv")
sap_clean <- na.omit(sapling)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarise(meanBrowsingScore = mean(BrowsingScore)) %>% print()
avg_browse_reg <- arrange(sap_reg_browse,desc(meanBrowsingScore))
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarise(meanHeight=mean(Height))%>%print()
sap_reg_height_low <- filter(sap_reg_height,meanHeight > 20)
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarise(mean(BrowsingScore))%>% print()
avg_browse_spe <- sap_spe_browse %>% arrange(desc(`mean(BrowsingScore)`))
fir_reg_browse <- sap_clean %>% filter(Species=="Balsam_Fir")%>%group_by(Ecoregion)%>% summarise(mean(BrowsingScore))
barplot(fir_reg_browse$`mean(BrowsingScore)`,names.arg=fir_reg_browse$Ecoregion,xlab="Ecoregion",ylab = "Browsing Score",main="Balsam Fir browsing intensity by ecoregion",col="forestgreen",cex.names=0.6)
spruce_reg_browse <- sap_clean %>% filter(Species=="Black_Spruce")%>%group_by(Ecoregion)%>%summarise(mean(BrowsingScore))
barplot(spruce_reg_browse$`mean(BrowsingScore)`,names.arg = spruce_reg_browse$Ecoregion,xlab = "Ecoregion",ylab="Browsing Score",main = "Black Spruce browsing intensity by ecoregion",col = "forestgreen",cex.names = 0.6)
#Black Spruce have a lower browsing score in the EasternHyperOceanicBarrens and MaritimeBarrens ecoregions,but a higher browsing score in the AvalonForests and NorthernPeninsulaForests Ecoregions 
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
moose_2020b <- mutate(moose_sel,MooseDensity= Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>% group_by(Species,Ecoregion)%>% summarise(AvgBrowsing=mean(BrowsingScore))%>%tally()%>%print()
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# a - Yes, there is evidence that supports the researchers hypothesis.Moose show a strong preference to black spruce at low density but they show less prefrence to a specific species at higher density
# b - Moose favour the Willow species the most and they browse Black Spruce the least.
# c - Black Ash is not shown on the graph.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites) %>% 
  rename(Ecoregion = study_sites)
MooseMerged <- moose_2020 %>% 
left_join(moose_coll, by = 'Ecoregion', relationship = "many-to-many")  
plot(MooseMerged$MooseDensity,MooseMerged$collisions2020)
# the number of collisons increases as the density increases.There is an outlier, at the density 1.0, the number of collisons increases dramaticly. 
coll_per_capita <- MooseMerged%>%mutate(coll_per_capita=collisions2020/human_pop)
plot(coll_per_capita$human_pop)
# the number of collisons remains less than 50000 for all index values except for 4.