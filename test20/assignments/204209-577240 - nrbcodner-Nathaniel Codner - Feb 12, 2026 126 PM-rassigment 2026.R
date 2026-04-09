#Nathaniel Codner
#bio 1002 R Assigment 
#Febuary 10th 2026
#set WD
setwd("/Users/nathanielcodner")
install.packages("dplyr")
#Q1
library(dplyr)
#Q2
moosedata<- read.csv("MoosePopulation.csv")
#Q3
View(moosedata)
moose_clean<-na.omit(moosedata)
#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Q5 a)
year_min<-min(moose_sel$Year)
#Q5 b) 
year_max<-max(moose_sel$Year)
#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Q7 
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time",
     type="l")
#Q8 a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Q8 b) 
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests region over time",
     type = "l")
#Q9 a)
moose_2020<-filter(moosedata2, Year==2020)
#Q9 b)
moose_2020_high<- filter(moose_2020,MooseDensity>2.0)
#Q9 c)
arrange(moose_2020_high, desc(MooseDensity))
#Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Q11 
saplings<-read.csv("SaplingStudy.csv")
#Q12
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsingScore = mean(BrowsingScore)) %>%
  print()
#Q12 b) 
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(MeanBrowsingScore)) %>%
  print()
#highest browsing score is "Northern_Peninsula_Forests"
#lowest Browsing score is "StraitOfBelleIsleBarrens"
#Q13 a)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(MeanHeight = mean(Height)) %>%
  print()
#Q13 b)
ap_reg_height_low <- sap_reg_height %>%
  filter(MeanHeight < 20) %>%
  print()
# the Ecoregions with avrage tree heights lowewr then 20cm is "Western_Forests" and "Northern_peninsula_Forests"
#Q14 a)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(MeanBrowsingScore = mean(BrowsingScore)) %>%
  print()
#Q14 b) 
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(MeanBrowsingScore)) %>%
  print()
# the species with highest browsing score is "black_Ash"
# the species with lowest browsing score is "Black_Spruce"
#Q15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsingScore = mean(BrowsingScore)) %>%
  print()
#Q16 
barplot(fir_reg_browse$MeanBrowsingScore, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Mean Browsing intensity", 
        main = "Average Balsam Fir Browsing Intensity by Ecoregion", 
        col = "limegreen", 
        cex.names = 0.6)
#Q17 a)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsingScore = mean(BrowsingScore)) %>%
  print()
#Q17 b)
barplot(spruce_reg_browse$MeanBrowsingScore, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Avrage browsing intensity", 
        main = "Average Black Spruce Browsing Intensity by Ecoregion", 
        col = "red", 
        cex.names = 0.6)
#Q17 c) 
# the browsing difference between the black spruce and balsam fir vary by region. The balsam fir outdoes the black spruce in almost every region except the long range barons where the black spruce outscores the balsam firs by 2 to 1 also in the western forests there is no balsam fir data to compare so the black spruce wins there as well.
#Q18 
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#Q19 
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#Q20 a)
# yes the ecological data set is unevenly distributed the regions have varying numbers of samples taken from different regions. for example north_shore_forests have eight samples taken vs StraitOfBelleIsleBarrens which only have one sample taken. Similar to the species taken the balsam fir has 11 samples taken compared to the black spruce which only had 9 or black ash which only had 1. this shows that there is inconsistency in the sampling data from the data set.  
#Q20 b) 
#it is important to recognize bias in data collection to ensure you can properly interpret the results received from the data set. if one species of tree has a higher number of saplings taken in a region compared to another and the data reflects that it would be a representation of sampling type vs actual ecological patterns. 
#Q21 a) 
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
#Q21 b)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#Q22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(MeanBrowsingScore = mean(BrowsingScore),
            MeanMooseDensity = mean(MooseDensity)) %>%
  print()
#Q23 a)
# no the dataset does not support the hypothesis it shows areas having high and low browing score not directly dependent on moose density for example in the Avalon_forests compared to central forests the density is .96 compared to 2.01 respectively meanwhile the browsing score is nearly identical at 4.0 and 4.5 respectively 
#Q23 b) 
# the species moose seem to browse the most are black ash with a browsing score of 5.0 and the least browsed is the black spruce with a score of 2.33. this makes sense because the black ash is in only one area that has a high density in that area so it would have the highest browsing score. 
#Q23 c) 
# the only sapling not listed in the plot is the black ash this could be because the plot is being overlapped by the willow tree plot at the same point on the graph.  
#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Q25 a)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
#Q25 b)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
#Q26 a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (per sq/km)",
     ylab = "Number of Moose Vehicle Collisions (2020)",
     main = " The relationship between Moose Density and Vehicle Collisions in 2020",
     pch = 16,    
     col = "lightblue")
#Q26 b)
#we see a common trend that the higher the density the more moose collisions there are in that area except one plot which has the highest number of crashes over 100 in a moose density area of only 1 per sq/km this point would be the outliar in this graph
#Q27 
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita)) %>%
  select(Ecoregion, collisions2020, human_pop, coll_per_capita) %>%
  print()
#Q28 
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "The relationship between Human Population and Moose Collisions per Capita",
     pch = 16,
     col = "violet")
#Q29 
#the trends I see are that there are more collisions in less densely populated areas I think that this makes sense because moose tend to be in areas where there aren't alot of people. The less people the higher density of moose population youre likely to see and higher chance of vehicle collisions. 





