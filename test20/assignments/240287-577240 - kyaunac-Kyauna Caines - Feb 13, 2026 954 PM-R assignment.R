#Q1
install.packages("dplyr")
library(dplyr)
#Q2
moosedata <- read.csv("MoosePopulation.csv")
View()
#Q3
moose_clean <- na.omit(moosedata)
#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Q5
min(moosedata$Year)
max(moosedata$Year)
#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Q8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Q9
moosedata2020<-moosedata2%>%
  filter(Year == 2020)
MooseDensity2020<-filter(moosedata2020,MooseDensity>2.0)
arrange(MooseDensity2020,desc(MooseDensity))
#Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Q11
Sap_clean<-read.csv("SaplingStudy.csv")
na.omit(Sap_clean)
#Q12
sap_cleanecoregion<-Sap_clean%>%
  group_by(Ecoregion)%>%
  summarize(averagebrowsing=mean(BrowsingScore))%>%
  print()
arrange(sap_cleanecoregion,desc(Sap_clean))
#Q13
sap_cleanecoregion<-Sap_clean%>%
  group_by(Ecoregion)%>%
  summarize(averagetreeheight=mean(Height))%>%
  print()

SevereSites<-sap_cleanecoregion%>%
  filter(averagetreeheight<20)%>%
  print()
#Q13b
sap_reg_height_low <- SevereSites
#northern & western forests are less than 20
#Q14
sap_spe_browse<-Sap_clean%>%
  group_by(Species)%>%
  summarize(averagebrowsing=mean(BrowsingScore))%>%
  print()
arrange(sap_cleanecoregion,desc(BrowsingScore))
#northern has highest browsing score and the straits has the lowest
#Q15
balsam_fir<-Sap_clean%>%
  filter(Species=="Balsam_Fir")
  group_by(Ecoregion)%>%
  summarize(averagebrowsing=mean(BrowsingScore))%>%
  print()
  #Q16
barplot(balsam_fir$BrowsingScore,
names.arg = balsam_fir$Ecoregion,
xlab = "ecoregion", 
ylab = "browsingscore",
main = "Browse scores on balsam fir",
col = "purple", #pick any colour you want
cex.names = 0.6) # Reduces x-axis label size for readability
#Q17
blackspruce<-Sap_clean %>%
  filter(Species=="Black_Spruce") %>%
  group_by(Ecoregion)%>%
  summarize(averagebrowsing=mean(BrowsingScore)) %>%
  print()

barplot(blackspruce$averagebrowsing,
        names.arg = blackspruce$Ecoregion,
        xlab = "ecoregion", 
        ylab = "browsingscore",
        main = "Browse scores on balsam fir",
        col = "blue", #pick any colour you want
        cex.names = 0.6) # Reduces x-axis label size for readability
#Q18
sap_reg_tally<- Sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Q19
sap_reg_tally<- Sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Q20
# not evenly distributed because theres more trees in the northern part than others

#Q21
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Q22
BrowsingBySpeciesDensity <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(averagebrowsing = mean(BrowsingScore),
  averagedensity = mean(MooseDensity))

#Q23
a) The evidence supports the hypothesis. At low moose densities (0.5–1.0), browsing scores vary widely among species, indicating clear selectivity—Willow shows high browsing intensity, while Black Spruce remains at zero. As moose density increases (2.0 and above), browsing scores for all species cluster near the upper end of the scale (3–5), suggesting a shift toward more generalized feeding.
b) Moose show the strongest preference for Willow, as it consistently receives the highest browsing scores across all density levels. In contrast, Black Spruce is browsed the least, with scores remaining at zero or very low until moose density becomes very high.
c) Black Ash appears in the legend but not in the figure itself. This likely occurred because no data points were available for this species in the 2020 subset, or because the ecoregions where it occurs lacked corresponding moose density data. As a result, the values were missing (NA) and could not be displayed on the plot.

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25
moose_coll <-moose_coll %>%
rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")

#Q26
plot(moose_coll$MooseDensity, moose_coll$collisions2020, xlab = "Moose Density", ylab = "Collisions")

#Q27
moose_coll <- moose_coll %>%
  mutate(CollisionsPerCapita = collisions2020/human_pop)

#Q28
plot(coll_merge$human_pop, coll_merge$CollisionsPerCapita, 
xlab = "Human Population",
ylab = "Collisions per Person",
main = "Collisions per Capita vs Populationn Size",
pch = 19, col = "purple")


#Q29
The trend indicates a clear negative relationship: the likelihood of an individual being involved in a collision is greatest in areas with the lowest population density. This pattern is especially evident in Newfoundland, where rural regions have a significantly higher moose-to-human ratio. In contrast, in more densely populated areas such as the Avalon Peninsula, the risk per person decreases because the larger population spreads that risk across more residents.


