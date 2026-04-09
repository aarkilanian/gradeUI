library (dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
Year_min <- min(moose_sel$Year)
Moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, moosedensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
xlab = "Year",
ylab = "moose per sq km",
main = "moose density in newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$moosedensity, type = "l" , xlab = "year" , ylab = "moose per sq km" , main = "moose density in newfoundland ecoregion over time")
moose_2020 <- filter (moosedata2, Year == '2020')
moose_2020_high <- filter (moose_2020, moosedensity > 2)
moose_2020_high_byD <- arrange(moose_2020_high, desc(moosedensity)
moosefinal <- moosedata2 %>% filter(Year == "2020") %>% filter(moosedensity > 2.0) %>% arrange(desc(moosedensity)) %>% print()                             
# load data :
sap <- read.csv ("SaplingStudy.csv")
sap_clean <- na.omit (sap)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarise(mean(browsingscore)) %>% print()
avg_browse_reg <- arrange(Sap_Reg_Browse,desc(meanbrowsingscore))
Sap_reg_Height <- sap_clean %>% group_by(Ecoregion) %>% summarize(meanHeight=mean(Height))%>% print ()
Sap_reg_Height_low <- filter(Sap_reg_Height,meanHeight > 20)
Sap_spe_Browse <- sap_clean %>% group_by(Species) %>% summarise(mean(BrowsingScore))%>% print()
avg_browse_spe <- Sap_spe_Browse %>% arrange(desc(`mean(BrowsingScore)`))
fir_reg_browse <- sap_clean %>% filter(Species=="Balsam_Fir")%>% group_by(Ecoregion)%>% summarise(mean(BrowsingScore))
barplot(fir_reg_browse$`mean(BrowsingScore)`,names.arg=fir_reg_browse$Ecoregion,xlab="Ecoregion",ylab= "Browsing Score", main= "Balsam Fir browsing intensity be ecoregion",col= "forestgreen", cex.names=0.6)
spruce_reg_browse <- sap_clean %>% filter(Species=="Black_Spruce")%>% group_by(Ecoregion)%>% summarise(mean(BrowsingScore))      
barplot(spruce_reg_browse$`mean(BrowsingScore)`,names.arg = spruce_reg_browse$Ecoregion,xlab = "Ecoregion",ylab = "Browsing Score",main = "Black Spruce browsing intensity be ecoregion",col="forestgreen", cex.names =0.60)
#Across ecoregions in newfoundland, black spruce is generally browsed less frequently and less intensively than balsam fir, as balsam fir is more patable and preferred by herbivores such as a moose. this pattern tends to be consistent across forested ecoregions, with balsam experiencing higher browsing pressure overall.
sap_reg_tally<- sap_clean %>% group_by(Ecoregion)%>% tally()%>% print()
sap_spe_tally<- sap_clean %>% group_by(Species)%>% tally()%>% print()
#the sapling study data set is reasonably evenly distributed across ecoregions and species, there is a little bit of underrepresentation in long range barrens 
#recognizing sampling bias is important because uneven data can distort ecological conclusions. over represented groups may appear more influential, leading to misleading interpretations of browsing patterns. 
str(sap_clean)
summary(sap_clean)
moose2020b <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion)%>%
  summarise(AvgBrowsing = mean(BrowsingScore))%>%
  tally()%>% print()
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#yes the figure supports the hypothesis. at low moose densities, browsing appears more selective, while at higher densities browsing intensity increases across more species indicating more generalist feeding
#moose favor willow the most. it is consistently showing the highest browsing scores, they browse black spruce the least, which shows very low browsing across densities 
#black ash is not shown on the graph 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)%>%
  rename(Ecoregion=study_sites)
MooseMerged <- moose_2020 %>% 
left_join(moose_coll, by = "Ecoregion")
          
plot(MooseMerged$moosedensity,
     MooseMerged$collisions2020)
#the plot suggests that where theres higher moose density there are more collisions thought there is some variability
coll_per_capita <- MooseMerged%>%mutate(coll_per_capita=collisions2020/human_pop)

plot(coll_per_capita$human_pop)
#most values are low with one outlier that is much higher, suggesting no strong overall trend. this makes sense because moose vehical collisions often depend on local factors rather than just human population size in Newfoundland.