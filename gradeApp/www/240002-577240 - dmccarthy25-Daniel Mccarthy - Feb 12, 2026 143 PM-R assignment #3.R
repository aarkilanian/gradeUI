# title: R script for Biology1001 R assignment
# author: Daniel McCarthy
# Date: 30-01-2026
# set working directory: setwd("/Users/danie/OneDrive/Documents/Biology1001Rassignment")libr
# Load Data: Read.csv("Moosepopulation(1).csv")
# Analyze data: year_min(1904) moose_max(41250)
#question 1
Library(dplyr)
#Question 2
Read.csv("Moosepopulation(1).csv")
#Question 3
moose_clean <- na.omit(MooseData)
#question 4
Moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#question 5
Year_min <- min(Moose_sel$Year)
Moose_max <- max(Moose_sel$Estimated_Moose_Pop)
#question 6
moosedata2 <- mutate(Moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#question 7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
#question 8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland Western Forests over time")
#question 9
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > 2)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#question 10 
moosefinal <- moosedata2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()
#question 11
# Load Data: Read.csv("Saplingstudy.csv")
sap_clean <- na.omit(Saplings)
#question 12
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarise(meanBrowsingScore = mean(BrowsingScore)) %>% print()
avg_browse_reg <- arrange(sap_reg_browse, desc(meanBrowsingScore))
#Northern Pennuinsula Forests has the Highest average browsing score, Straight of bell isle barrens had the lowest Average Browsing Score.
#question 13
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarise(meanHeight = mean(Height)) %>% print()
Sap_reg_height_low <- filter(sap_reg_height, meanHeight > 20)
# Northern Peninsula Forests and Western Forests have Average tree height Scores less than 20
# question 14
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarise(meanBrowsingScore = mean(BrowsingScore)) %>% print()
avg_browse_spe <- arrange(sap_spe_browse, desc(meanBrowsingScore))
#Black ash has the highest Browsing Score and Black Spruce Has the Lowest Average Browsing Score.
#question 15
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarise(mean(BrowsingScore))
#question 16
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "average browsing intensity", main = "Browsing intensity variation of Balsam Fir by Ecoregion", col = "forestgreen", cex.names = 0.6)
#question 17
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarise(mean(BrowsingScore))
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average browsing intensity", main = " Browsing intensity variation of Black spruce trees by Ecoregion", col = "blue", cex.names = 0.6)
# Balsam Fir browsing occurs in Central Forests, Long Range Barren's and North Shore Forests while Black spruce browsing dosen't, meanwhile Black spruce Browsing occurs in Eastern hyper oceanic barrens, Maritime Barrens, and Northern Peninsula Forests while Balsam Fir dosen't, aswell both Balsam Fir and Black spruce browsing occurs in Avalon forests but Balsam Fir occur's at a higher intensity
# question 18
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()
# question 19
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()
#question 20
#I think for the most part it is evenly distibuted, other than Black_Ash being underpresented, same as the straitofbelleisleBarrens or Balsam_Fir being overpresented but other than that its relatively evenly distributed.
# Because if we dont it can lead to use having false assumptions on what the Ecological dataset is on
#question 21
moose_2020b <- moose_clean %>% filter(Year == "2020") %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
#question 22
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarise(mean_value1 = mean(BrowsingScore, na.rm = TRUE), Mean_value2 = mean(MooseDensity, na.rm = TRUE)) %>% print()
#question 23
# Yes this supports researchers hypthesis as there is more of a selection of browsing with higher density's, as at low density's its more specific but when it gets to higher populations the selection of browsing becomes more general
#Moose favour the Willow the most as it has a higher average browsing intensity at low moose density, and they browse white burch the least as it has the lowest average browsing intensitys across most Moose densitys
# Black ash isnt shown on the figure because its only found in one of the Ecoregions
#question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll<- data.frame(collisions2020, human_pop, study_sites)
#question 25
moose_coll <- rename(moose_coll, Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll, by = "Ecoregion", relationship = "many-to-many")
#question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "MooseDensity", ylab = "Collisions", main = "Moose density's relation to number of moose vehicle collisions")
# as moose Density increases, number of Moose, vehicle collisions increases, there is an outlier as the highest amount of Vehicle collisions occur at a MooseDensity of 1.0
#question 27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
#question 28
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop, xlab = "Moose collisions per capita", ylab = "Human Population", main = "Moose collisions per capita realtionship with human populations")
#question 29
# The higher the population the lower moose collisions per capita, this makes sense as Moose tend to live away from areas with higher human population or areas that are more industrialized


