install.packages("dplyr")
library("dplyr")
MoosePopulation <- read.csv("MoosePopulation.csv")
View(MoosePopulation)
moose_clean <- na.omit(MoosePopulation)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
min(moose_sel$Year)
year_min <- 1904
max(moose_sel$Estimated_Moose_Pop)
moose_max <- 41250
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time",type = "l")
 moose_2020 <- filter(moosedata2, Year == 2020)
 moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
 moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
 moosefinal <- moosedata2 %>%
   filter(Year == 2020) %>%
   filter(MooseDensity > 2.0) %>%
   arrange(desc(MooseDensity)) %>%
   print()
 #part2
 saplings <- read_csv("SaplingStudy (1).csv")
 View(saplings)
 sap_clean <- na.omit(saplings)
 sap_reg_browse<- sap_clean%>%
   group_by(Ecoregion)%>%
   summarise(AverageBrowsing= mean(BrowsingScore))%>%
   print()
 
 avg_browse_reg <- sap_clean%>%
   group_by(Ecoregion) %>%
   summarise(AverageBrowsing= mean(BrowsingScore))%>%
   arrange(desc(AverageBrowsing))%>%
   print()
 #Northern_Peninsula_Forests have the most moose browsing
 #StraitOfBelleIsleBarrens have the least moose browsing
 
 sap_reg_height <- sap_clean %>%
   group_by(Ecoregion) %>%
   summarise(AverageBrowsing= mean(BrowsingScore)) %>%
   print()

 sap_reg_height_low <- sap_clean %>%
   group_by(Ecoregion) %>%
   summarise(mean_height = mean(Height)) %>%
   filter(mean_height < 20) %>%
   print()
#Ecoregions with average tree heights less than 20cm are considered severly browsed
#Northern_Peninsula_Forests and Western_Forests
 
avg_browse_reg <- sap_clean %>%
  group_by(Species) %>%
  summarise(AverageBrowsing= mean(BrowsingScore)) %>%
  arrange(desc(AverageBrowsing)) %>%
  print()

#Black_Ash has the highest browsing score of 5
#Black_Spruce has the lowest browsing score of 2.33

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(mean_BrowsingScore =mean(BrowsingScore))%>%
  print()

barplot(fir_reg_browse$mean_BrowsingScore,names.arg = fir_reg_browse$Ecoregion, xlab ="Ecoregion", ylab = "Mean_Browsing", main = "Mean Browsing of Balsam Fir in Each Ecoregion", col = 'navy', cex.names = 0.6  )
spruce_reg_browse <-sap_clean %>%
  filter(Species == "Black_Spruce")%>%
group_by(Ecoregion)%>%
  summarise(mean_browsing =mean(BrowsingScore))%>%
  print()
barplot(spruce_reg_browse$mean_browsing,names.arg = spruce_reg_browse$Ecoregion,xlab = "Ecoregion", ylab = "Mean_Browsing", main = "Mean Browsing of Black Spruce in Each Ecoregion", col = "red", cex.names = 0.6 )
#There are no black spruce in the Northern_Peninsula_Forests, while there are balsam fir in the Northern_Peninsula_Forests with a browsing score of 4.25. There are more balsam fir in the Avalon_Forests than there are black spruce, with a 2 browsing score for balsam fir and 0.5 score for black spruce.
sap_reg_tally<- sap_clean%>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species)%>%
  tally()%>%
  print()
#No, the same number of tree saplings were not counted for each species.
#No, I think the SaplingStudy dataset is not evenly disturbed across all Ecoregions.
#Strait_of_Bell_Isle is an underrepresented ecoregion at 1 tree, North_Shore_Forests is an overreprented at 8 trees
#Recognizing bias in ecological datasets is important because bias can skew results unfairly and lead to inaccurately represent data. Ultimately leading to consumers of the research being misleaded and informed about incorrect informations.

moose_2020b <- moose_clean%>%
  filter(Year == "2020")%>%
  mutate(MooseDensity=Estimated_Moose_Pop/ Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion",relationship = "many-to-many")

sum_spe_browse <- moose_sap%>%
  group_by(Species, Ecoregion)%>%
  summarise(mean_BrowsingScore= mean(BrowsingScore), mean_MooseDensity =mean(MooseDensity))%>%
  print()
library(ggplot2)

ggplot(sum_spe_browse, aes(x = mean_BrowsingScore, y = mean_MooseDensity, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#Yes, there is evidence supporting the researchers hypothesis. At Average Moose Density 0, only Willow is browsed highly with a score of 4 meaning there is a strong preference.
#Willow is the most favoured sapling specie. Black_Spruce is the least browsed.
#Black_Ash is not shown on the figure since it likely had no browsing activity recorded because it might have not been found in the study plots
collisions2020<- c(56, 60, 14, 36, 48, 10, 40, 110,6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanBarrens", "Maritime_Barrens", "Avalon_Forests", "StraightOfBelleIsleBarrens")

moose_coll<- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion= study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")

plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "collisions 2020", main = "Moose density vs Collisions")

coll_per_capita <- mutate(moose_coll2, coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita <- coll_per_capita
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Moose Collisions per Capita",
     ylab = "Human Population",
     main = "Moose Collisions per Capita vs Human Population")
# The trend of the graph shows that as human population increases, moose collision decrease. This makes sense,In poopulated areas there is a lower chance of seeing moose therefore less collisions.
