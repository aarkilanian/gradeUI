install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$year)
moose_max <-max(moose_sel$Estimated_Moose_Pop)
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
     main = "Moose density in Newfoundland ecoregions over time")
moosedata2020<-moosedata2%>%
  filter(Year==2020)
moosedensity2020<-filter(moosedata2, MooseDensity > 2.0)
arrange(moosedensity2020, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
sap_clean<-read.csv("SaplingStudy.csv")
na.omit(sap_clean)
sap_clean_ecoregion <- sap_clean%>% group_by(Ecoregion,)%>%
  summarize(avgbrowsing=mean(BrowsingScore))%>%
  print()
sap_reg_height <- arrange(sap_clean_ecoregion <- sap_clean%>% group_by(Ecoregion,)%>%
          summarize(avgbrowsing=mean(Height))%>%
          print()
sap_reg_height_low <-#        
Severesites<-sap_clean_ecoregion%>%
filter(averagetreeheight<20)%>%
print()
sap_reg_height_low <- SevereSites
#northern & western forests are less than 20
sap_spe_browse<-sap_clean%>%
  group_by(Species)%>%
  summarize(averagebrowsing=mean(BrowsingScore))%>%
  print()
arrange(sap_clean_ecoregion,desc(BrowsingScore))
#northern has highest browsing score and the straits has the lowest 
balsam_fir<-sap_clean%>%
  filter(Species=="Balsam_Fir")
group_by(Ecoregion)%>%
  summarize(averagebrowsing=mean(BrowsingScore))%>%
  print()
barplot(balsam_fir$BrowsingScore,
names.arg = balsam_fir$Ecoregion, 
xlab = "ecoregion",
ylab = "browsingscore",
main = "Browse scores on balsam fir",
col = "lightgreen", #pick any colour you want
cex.names = 0.6) # Reduces x-axis label size for readability
blackspruce<-sap_clean%>%
  filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarize(averagebrowsing=mean(BrowsingScore))%>%
  print()
barplot(blackspruce$averagebrowsing,
        names.arg = blackspruce$Ecoregion, 
        xlab = "ecoregion",
        ylab = "browsingscore",
        main = "Browse scores on balsam fir",
        col = "lightblue", #pick any colour you want
        cex.names = 0.6) # Reduces x-axis label size for readability
sap_reg_tally<- sap_clean%>%
  group_by(Ecoregion)%>%
  tally() %>%
  print()
sap_reg_tally<- sap_clean%>%
  group_by(Species)%>%
  tally() %>%
  print()
#not evenly distributed because theres more trees in the northern part than others
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(sap_clean, moose_2020b, by = "Ecoregion")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    mean_browsing = mean(BrowsingScore, na.rm = TRUE),
    mean_density = mean(MooseDensity, na.rm = TRUE),
    .groups = "drop")
print(sap_spe_browse)
#a) Yes, the evidence supports the hypothesis. At low moose densities (0.5–1.0), there is a wide spread in browsing scores showing clear preference (Willow is high, while Black Spruce is at zero), but as density increases (2.0+), the scores for all species converge toward the top of the scale (3–5), indicating more general browsing. 
#b) yes, willow is favoured, with high browsing intensity. In contrast Black Spruce is the least browsed and seems untouched until moose densiy is very high
#c)Black Ash is listed in the legend, but doesn't show on the graph, this could have possibly happened because that specific 2020 data subset didn't have any data for it, or the areas where it grows didn't have any moose density number to go with them. It resulted in NA as the value which the computer cannot plot 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
MooseCollisions <- data.frame(collisions2020, human_pop, study_sites)
> print(moose_coll)
moose_coll2 <- moose_coll2 %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Collisions")
#Broadly speaking, there’s a clear trend: as moose density goes up, so do the accidents, which you can see from how the points cluster and climb toward the right side of the chart. But there’s one huge outlier sitting at over 100 collisions with only a moderate density (around 1.0). That’s almost certainly the Avalon Forests—it’s a spot where the sheer amount of people and traffic drives the accident numbers up, rather than just the number of moose in the woods.
coll_merge <-coll_merge%>% mutate(CollisionsPerCapita = collisions2020/human_pop)
plot(coll_merge$human_pop, coll_merge$CollisionsPerCapita, xlab = "Human Population", ylab = "Collisions per Person", main = "Collisions per Capita vs Populationn Size", pch = 19, col = "pink")
#the trend is a strong negative one: your individual risk of hitting a moose is actually highest in places where there are the fewest people.
#It tracks for Newfoundland because in the rural spots, the "moose-to-human" ratio is way higher. You've got fewer people spread out over a lot of land, so your odds of crossing paths with a moose while driving are just statistically much higher than they’d be in a crowded city.
#But then you look at super crowded spots like the Avalon, and the risk for each person actually goes down just because there are so many people living there. Even if there are a lot of crashes, they're spread out over such a huge population that your individual odds of hitting a moose stay pretty low.