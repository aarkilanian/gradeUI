# Title: R script BIOL 1002
# Author: Claire Hodder
setwd("dir")
install.packages("dplyr")
# Question 1:
library(dplyr)
# Question 2:
moosedata <- read.csv("~/Downloads/MoosePopulation.csv")
View(moosedata)
# Question 3:
moose_clean <- na.omit(moosedata)
# Question 4:
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# Question 5:
# a. 1904
year_min <- min(moose_sel$Year)
# b. 41250
moose_max <- max(moose_sel$Estimated_Moose_Pop)
# Question 6:
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
# Question 7:
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
# Question 8:
# a.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# b.
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland Western Forest Ecogregion")
# Question 9:
# a.
moose_2020 <- filter(moosedata2, Year == "2020")
# b.
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
# c.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
# Question 10:
moosefinal <- moosedata2 %>% filter (Year == 2020) %>% filter (MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()
# Part 2
#question 11:
# a.
saplings <- read.csv("~/Downloads/SaplingStudy.csv")
View(saplings)
#question 11
#b
sap_clean <- na.omit(saplings)
# Question 12:
# a.
sap_reg_browse <- sap_clean %>%
 group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Question 12
# b.
avg_browse_reg <- arrange(sap_reg_browse,desc(AverageBrowsing))
# Northern Peninsula Forests has the highest average browsing score.
#strait of Belle Isle Barrens has the lowest average browing score 
# Question 13 
#a
sap_reg_height <- sap_clean %>%
 group_by(Ecoregion) %>% 
 summarize(AverageHeight = mean(Height)) %>%
 print()  
# b
sap_reg_height_low <- filter(sap_reg_height, AverageHeight <20)
# The Western Forests and Northen Peninsula Forests have average heights less then 20cm 
# Question 14 
#a 
sap_spe_browse <- sap_clean %>%
 group_by(Species) %>%
 summarise(AverageBrowsing = mean(BrowsingScore)) %>%
 print()
# Question 14
# b
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# Black ash has the highest browsing score.
# Black spruce has the lowest browsing score.
# Question 15 
fir_reg_browse <- sap_clean %>%
 filter(Species == "Balsam_Fir") %>%
 group_by(Ecoregion)  %>%
 summarize(AverageBrowsing = mean(BrowsingScore))  %>%
 print()  
# Question 16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregions",
        ylab = "Average Browsing Intensity",
        main = "The Average Browsing Intensity of Balsam Fir in Different Ecoregions", 
        col = "forestgreen", cex.names = 0.6)
# Question 17 
#a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce")  %>%
  group_by(Ecoregion)  %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#b
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregions",
        ylab = "Average Browsing Inensity",
        main = "The Average Browsing Intensity of Black Spruce in Different Ecoregions",
        col = "yellow", cex.names = 0.6)
#c
# balsam fir has a higher browsing in regions like north shore peninsula, northern penisula forests and western forests. Black spuce does aswell but its average browsing is still lower than balsam firs
# Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
# Question 19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# Question 20
#a
# The saplingstudy dataset isnt evenly distributed, certain tree species and ecoregions are overepresented and some are underepresented.
#b
# it is important to recognize bias in ecological datasets because if it is not recognized it can lead you to make wrongful conclusions about the data.

# Part 3
# Question 21
# a
moose_2020b<- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print() 
# b
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion",
                       relationship = "many-to-many")
# Question 22
sum_spe_browse<- moose_sap %>%
  group_by(Ecoregion) %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore),
            AverageMooseDensity = mean(MooseDensity)) %>%
  print()
# Question 23 
# a
# yes this figure supports the researchers hypothesis because, For low moose density there are high browsing scores for certain species of tree but for high moose density the browsing score increases across many of the tree species,there is a shift towards generalists browsing at high moose densities 
# b
# after looking at this fugure, it seems that moose browse black spruce the least and willow the most. 
# c
# the figure, The species now shown is black ash. this species was not frequently found enough at the site of studies to collect usable data
# Question 24 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# Question 25
# a
moose_coll2 <- rename_with(moose_coll, ~ "Ecoregion", .cols = study_sites)
# b
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion", relationship = "many-to-many")
# Question 26
# a 
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
    xlab = "Moose Density",
    ylab = "Number of Moose-Vehicle Collisons (2020)",
    main = "Moose Density in relation to Moose-Vehicle Collisons")
# b.
# when the density of moose increases so does the number of Moose-vehicle collisons.
# there are some outliers in certain areas, because when moose denisty is moderate the number of moose-vehicle collisons are super elavated 

# Question 27
coll_merge_per_capita <- mutate(coll_merge,coll_per_capita = collisions2020 / human_pop)

# Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisons per Capita",
     main = "Moose Collisons per Capita in relation to Human population")
# Question 29
# regions that have smaller human populations have higher moose collisons per capita, which means the two variables compared have a negative relationship with eachother
# it makes sense because my knowledge of how town around the bay have smaller populations, and have alot more moose sightings occur, so the likelyness is high that there will be more moose collisons in this area.
