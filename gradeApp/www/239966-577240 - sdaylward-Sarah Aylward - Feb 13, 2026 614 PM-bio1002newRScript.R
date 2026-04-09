# Title: R script 
# Name: Sarah Aylward 202510613
# Date: Feb 12 2026

# Load libraries needed -----------------
MoosePopulation
SaplingStudy


# Moose Populations in NL -----------

#Q1
library(dplyr)

#Q2
moosedata <- read.csv("MoosePopulation.csv")

setwd("C:/Users/50130203/Downloads/bullshit")
#Q3
moose_clean <- na.omit(moosedata)
View(moose_clean)

#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Q8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, 
     moose_west$MooseDensity,
     type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Q9
moose_2020 <- filter(moosedata2, MooseDensity == Estimated_Moose_Pop / Area)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byd <- arrange(moose_2020_high, desc(MooseDensity))

#Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


#2 --- Tree Sapling Study


#Q11
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)            

#Q12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# The Northern Peninsula and North Shore forests have the highest browsing scores, while the Strait of Belle Isle and the Maritime Barrens have the lowest average browsing scores.

#Q13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(Height)) %>%
  print()

sap_reg_height_low <- filter(sap_clean, Ecoregion > 20)%>%
print()

#Q14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#Q15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#Q16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "ecoregion", ylab = "average browsing intensity", las=2, main = "BalsamFir", col = "forestgreen", cex.names = 0.6)

#Q17        
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, las = 2, xlab = "ecoregion", ylab = "average browsing intensity", main = "Spruce", col = "forestgreen", cex.names = 0.6)

#In the Avalon Forest, as well as the Oceanic and Maritime Barrens, there is a moderate amount of browsing that occurs, however in the spruce there is little to none. As well, both the Western and Peninsula Forests have a strong average browsing intensity in both the Spruce and Balsam species. 

#Q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Q19
sap_reg_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Q20

#I believe the sapling study is not equally distributed entirely. This is because while many regions and/or species may be analyzed in similar fashions or amounts, some are over represented, such as the North Shore Forests region. In addition, others are very under represented such as the Black Ash Species, which is only located in one region, or the Maritime Barrens region.

#It is important to recognize bias in ecological data sets for many reasons, such as it can lead to incorrect or miscalculated results and/or conclusions, which can negatively affect their accuracy. As well, on a larger scale, developing and distributing incorrect ecological data can lead to incorrect or misleading ecological decisions, policies, research, etc.


#3----- Creating and Joining Datasets

#Q21

moose_2020b <- moose_clean %>%
  filter(Year == '2020' ) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Q22
browse_spp_dens <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
    summarize(AvgDensity = mean(MooseDensity), 
          AvgBrowsing = mean(BrowsingScore))
print(browse_spp_dens)

#Q23
Based on the image provided, there is evidence that suppoorts the reserachers hypothesis. As moose populations are more condensed, it appears that the browsing intensity increases, while as they are more dispersed the browsing intensity decreases. 
Moose appear to favour sapling species such as White Birch and Black Spruce, while they show little interest in Willow trees.
The sapling species Black Ash is not shown on the figure because there is a low population of this species and/or they are not recorded in abundance in these regions. 

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites) 

#Q25
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020b, moose_coll2, by ='Ecoregion')

#Q26 
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "vehicle collision", 
     ylab = "moose density", 
     main = "moose desnity vs vehicle collsion")


#Q27
coll_merge_per_capita <- coll_merge  %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

#Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "human pop",
     ylab = "collions per capita",
     main = "collisions per capita vs human population")
 
#Q29
####As demonstrated in the scatterplot, it is apparent that with more people in a given area, it is more likely for a collision to occur. As well, it is more likely for a collision to occur when both the human and moose population are highr.