# Title: My R script
# Author: Abigail Strangemore
# Date: 09-02-2026

# Load libraries needed
install.packages("dplyr")
library("dplyr")

# Set working directory
setwd("/cloud/project")

# Load data
moosedata <- read.csv("MoosePopulation.csv")
saplings <- read.csv("SaplingStudy.csv")

# Analyze data
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min<-(1904)
moose_max<-(41250)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)
moose_2020 <- filter(moosedata2, Year ==2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
ap_clean <- na.omit(saplings)
sap_reg_browse <- ap_clean%>%
  group_by(Ecoregion)%>%
  summarize(mean(BrowsingScore))
  print(sap_reg_browse)
avg_browse_reg <- arrange(sap_reg_browse, desc(`mean(BrowsingScore)`))
#The region with the highest average browisng score is Northern_Peninsula_Forests. The area with the lowest average browsing score is StraitOfBelleIsleBarrens.
sap_reg_height <- ap_clean%>%
group_by(Ecoregion)%>%
summarize(mean(Height))
print(sap_reg_height) 
sap_reg_height_low <- sap_reg_height%>%
  filter(`mean(Height)`<20)
print(sap_reg_height_low)
#The regions with average heights lower than 20cm are Northern_Peninsula_Forests and Western_Forests
sap_spe_browse <- ap_clean%>%
  group_by(Species)%>%
  summarize(mean(BrowsingScore))
print(sap_spe_browse)
avg_browse_spe<- arrange(sap_spe_browse, desc(`mean(BrowsingScore)`))
#the species with the highest browing score is Black_Ash the species with the lowest browsing score is Black_Spruce
fir_reg_browse<- ap_clean%>%
  filter(Species=="Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarize(mean(BrowsingScore))
spruce_reg_browse<- ap_clean%>%
  filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarize(mean(BrowsingScore))
#The black Spruce data in comparison to the Basalm fir data is much more extreme, its averages are very high in the wastern forests and lower in the avalon forests and long range barrens, wheras the Balsam fir averages are high in the avalon forests, lower in the long range barrens and high in the northern peninsula forests.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<-ap_clean%>%
  group_by(Species)%>%
  tally()
#No, the SaplingStudy Dataset is not evenly distributed. there are many outliers inn this data. Black_spruce is under represented but willows are overrepresented among others.
#it is important to recognize bias in ecological data sets in order too ensure accuracy and so that it can be used in comparison to past and future data.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  )
print(sum_spe_browse)
#Yes, moose show more strong preference at low density and show more generalist browsing at high density.
#Moose mostly favour the species willow and alder, moose favour the species black spruce the least.
#Black ash is not shown on the figure, this is most likely due to there being no data for this species
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2<-moose_coll%>%
  rename(Ecoregion = study_sites)
coll_merge<-moose_2020%>%
  left_join(moose_coll2)
coll_per_capita<-moose_coll%>%
  mutate(col_per_capita = collisions2020 / human_pop)
coll_merge_per_capita<-coll_per_capita


# Plot data
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "year", ylab = "moose per sq km", main = "Moose Density in Western Forsets Over Time")
barplot(fir_reg_browse$`mean(BrowsingScore)`,names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "`mean(BrowsingScore)`",
        main = "Balsam Fir Average Browisng By Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
barplot(spruce_reg_browse$`mean(BrowsingScore)`,names.arg = spruce_reg_browse$Ecoregion,
        ylab = "`mean(BrowsingScore)`",
        xlab = "Ecoregion",
        main = "Black_Spruce Average Browsing By Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
    xlab= "MooseDensity",
    ylab="Collisions",
    main= "how moose density realtes to moose vehicle collisions", 
    col="blue",
    cex.names = 0.6)
#Moose vehicle collisions typically increase with moose density with the exception of one extremely high value of collisions at moose density 1.0
plot(coll_per_capita$human_pop, moose_coll2$coll_per_capita,
     xlab="Human Popultion",
     ylab="Collisions Per Person",
     main ="Collisions Per Capita Per Population Size",
     col="purple",
     pch=19)
#there is no clear trend shown between moose vehicle collisions and human population density, this makes sense for newfoundland beacause moose density's vary across different areas making certain locations more prone to moose vehicle collisions. this would not depend on human population. 
