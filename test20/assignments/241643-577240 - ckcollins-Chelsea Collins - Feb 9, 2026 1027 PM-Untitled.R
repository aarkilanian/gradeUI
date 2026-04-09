# Title: R Assignment 
# Author: Chelsea Collins
# Date: 09-02-2026

#part 1
#question 1
library(dyplr)

#question 2 
moosedata <- read.csv("MoosePopulation.csv")

#question 3
View(moosedata)
moose_clean <- na.omit(moosedata)

#question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#question 5
#a
year_min <- min(moose_sel$Year)
#b
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#question 7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#question 8
#a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#b
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in the Western Forests Ecoregion over time",
     type = "l")

#question 9
#a
moose_2020 <- filter(moosedata2, Year == "2020")
#b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#part 2
#question 11
#a
saplings <- read.csv("SaplingStudy.csv")
#b
sap_clean <- na.omit(saplings)

#question 12
#a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#question 12b
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
#highest average browsing score - northern peninsula forests
#lowest average vbrowsing score - strait of belle isle barrens  

#question 13
#a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
#b
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)
#the forests with with average heights that are less than 20 cm are the northern peninsula forests and western forests 

#question 14
#a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#b
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# Highest browsing score - black ash 
#lowest browsing score - black spruce 

#question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print() 

#question 16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregions", 
        ylab = "Average Browsing Intensity", 
        main = "The Average Browsing Intensity of Balsam Fir in Different Ecoregions", 
        col = "green", cex.names = 0.6) 

#question 17
#a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print() 
#b
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregions", 
        ylab = "Average Browsing Intensity", 
        main = "The Average Browsing Intensity of Black Spruce in Different Ecoregions", 
        col = "yellow", cex.names = 0.6) 
#c
#balsam fir has higher browsing in regions like north shore peninsula, northern peninsula forests and central forests. So does black spruce but it's average browsing is still lower than balsam firs. 

#question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#question 19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#question 20
#a
#the saplingstudy dataset is not evenly distributed. Certain tree species and ecoregions are overrepresented and some are underrepresented.
#b
#it's important to recognize bias in ecological datasets because if it is not recognized it can lead you to make inaccurate conclusions about the data.  

#part 3
#question 21
#a
moose_2020b<- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>% 
  print()
#b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', 
                       relationship = "many-to-many")

#question 22
sum_spe_browse<- moose_sap %>%
  group_by(Ecoregion) %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore), 
            AverageMooseDensity = mean(MooseDensity)) %>% 
  print()

#question 23
#a
#this figure does support theresearchers hypothesis. For low moose density there are high browsing scores for certain species of tree but fopr hiugh moose density there is an increasing browsing score across many of the tree species. This shows that there is a shift towards generalist browsing at high moose densities. 
#b
#after looking at the figure moose seem to browse black spruce the least and willow the most.  
#c
#in the figure, the species not shown is black ash. This could be because the species was not frequently found enough at the study site to collect usable data. 

#question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#question 25
#a 
moose_coll2 <- rename_with(moose_coll, ~ "Ecoregion", "study_sites")
#b 
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', 
                        relationship = "many-to-many")

#a
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collsions (2020)",
     main = "Moose Density in relation to Moose-Vehicle Collisions")
#b
#when moose density increases the number of moose-vehicle collisions also increase.
#there are outliers as in some areas because when moose density is only moderate the number of moose-vehicle collisions are very high.

#question 27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

#questions 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population",
     ylab = "Moose Collisions per Capita",
     main = "Moose Collisions per Capita in relation to Human Population")

#question 29 
#regions with smaller human ppulations have higher moose collisions per capita which means the two varibales must have a negative relationship. 
#it does make sense because I know that towns that are around the bay tend to have smaller populations and they have a lot more moose sightings so there is a high likelyness that there will be more moose-vehicle collisions in this area. 