# My R script for assignment 1
# Author: Jacob Hawco
# Date: 05-02-2026

# Q1 Load Libraries needed:
library(dplyr)

# Q2 Import .csv's:
moosedata <- read.csv("C:/Users/Jacob/Downloads/MoosePopulation.csv")

#Q3 Viewing Data:
View(moosedata)
moose_cleaned <- na.omit(moosedata)

#Q4 selecting data of interest:
Moose_sel <- select(moose_cleaned, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5 Find oldest observation in data set, highest estimated moose population:
#a.)
Year_min <- min(Moose_sel$Year)

#b.)
Moose_max <- max(Moose_sel$Estimated_Moose_Pop)

#Q6 Moose density by region:
moosedata2 <- mutate(Moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Q7 Visulaizing data:
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose/km^2",
     main = "Moose density in Newfoundland ecoregions over time")

#Q8 Western Forests dataset:
#a.)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#b.)
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Time(years)",
     ylab = "Moose/km^2",
     main = "Moose density in Newfoundland's Western Forests")

#Q9 Moose in 2020
#a.)
moose2020 <- filter(moosedata, Year == "2020")
moose2020 <- mutate(moosedata, MooseDensity == Estimated_Moose_Pop / Area)

#b.)
moosedata <- mutate(moosedata, MooseDensity = Estimated_Moose_Pop / Area)
moose2020_high <- filter(moosedata, MooseDensity > 2.0)

#c.)
moose_2020_high_byD <-arrange(moose2020_high, desc(MooseDensity))

#Q10 pipes
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Q11 tree saplings
saplings <-read.csv("C:/Users/Jacob/Downloads/SaplingStudy.csv")
sap_clean <-na.omit(saplings)

#Q12
sap_reg_browse <- group_by(sap_clean, Ecoregion) %>% 
  summarise(AverageBrowsing = mean(BrowsingScore))

avg_browse_reg <-arrange(sap_reg_browse, desc(AverageBrowsing))

#Q13
sap_reg_height <- group_by(sap_clean, Ecoregion) %>% 
  summarise(Height = mean(Height))
#b.)
avg_reg_height_low <- filter(sap_reg_height, Height < 20) %>% 
  print()
# the Northern Peninsula Forests & the Western Forests

#Q14
sap_spe_browse <- group_by(sap_clean, Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#b.)
avg_browse_spe <-arrange(sap_spe_browse, desc(AverageBrowsing)) %>% 
  print()
#highest = "Black Ash"
#Lowest = "Black Spruce"

#Q15 
BalsamFir <- filter(sap_clean, Species == "Balsam_Fir") 
fir_reg_browse <-group_by(BalsamFir, Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>% 
  print()

#Q16
barplot(fir_reg_browse$AverageBrowsing, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregions", 
        ylab = "Average Browing", 
        main = "Average browsing of Balsam Fir in different Ecoregions", 
        col = "lavender",
        cex.names = 0.6)

#Q17
#a.)
BlackSpruce <- filter(sap_clean, Species == "Black_Spruce")
spruce_reg_browse <- group_by(BlackSpruce, Ecoregion) %>% 
  summarise(AverageBrowsing = mean(BrowsingScore)) %>% 
  print()
#b.)
barplot(spruce_reg_browse$AverageBrowsing, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregions", 
        ylab = "Average Browing", 
        main = "Average browsing of Black Spruce in different Ecoregions", 
        col = "lavender",
        cex.names = 0.6)
#c.)
# The Black Spruce had the highest browsing in the Northern Peninsula Forests
# and the Northern Shore Forests. The Balsam fir also had the highest
# browsing on the Northern Shore Forests as well.  

#Q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# no there were different numbers of tree saplings counted in each region 

#Q19 
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#Q20
# The Balsam Fir is over represented in species sampled, and the Black Ash is 
# underrepresented only having one sample. The North Shore Forest and the
# Northern Shore Peninsula ecoregions are over represented, whereas 
# the strait of belle isle is under represented only having one.

#Q21 
Moose_2020btemp <- filter(moose_cleaned, Year == 2020)

Moose_2020b <- mutate(Moose_2020btemp, MooseDensity = Estimated_Moose_Pop / Area)
#b.)
moose_sap <- left_join(Moose_2020b, sap_clean,
                       by = 'Ecoregion', relationship = "many-to-many")
#Q22
# Calculate the average browsing score and moose density
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsingScore = mean(BrowsingScore, na.rm = TRUE),
    AvgMooseDensity = mean(MooseDensity, na.rm = TRUE))
  print(sum_spe_browse)
#Q23
#A.) there is evidence to support this hypothesis, when density is low they 
  # eat more of two types of stickers,
  #B.) willow and alder have the highest browsing scores compared to other species.
  #C.) Black ash isn't shown in the graph this is likely because there is only one 
  #data point in for black ash.
  
#Q24
  collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
  human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
  study_sites <- c("North_Shore_Forests",
                   "Northern_Peninsula_Forests",
                   "Long_Range_Barrens",
                   "Central_Forests",
                   "Western_Forests",
                   "EasternHyperOceanicBarrens",
                   "Maritime_Barrens",
                   "Avalon_Forests",
                   "StraitOfBelleIsleBarrens")
  
  moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25
#a.)
  moose_coll2 <- moose_coll %>%
    rename(Ecoregion = study_sites)
#b.)
  coll_merge <- moose_coll2 %>%
    left_join(moose2020, by = "Ecoregion")
#Q26
  coll_mergeedit <- mutate(coll_merge, MooseDensity = Estimated_Moose_Pop / Area)
plot(coll_mergeedit$MooseDensity,
       coll_mergeedit$collisions2020,
       xlab = "Moose Density",
       ylab = "Number of Moose-Vehicle Collisions (2020)",
       main = "Moose Density vs. Moose-Vehicle Collisions (2020)",
       pch = 19)
#Q27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
#Q28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions Per Capita",
     main = "Moose Collisions Per Capita vs. Human Population",
     pch = 19)
abline(lm(coll_per_capita ~ human_pop,
          data = coll_merge_per_capita),
       lwd = 2)
#Q29
#using the abline function, i generated a trend line to more obviously show the 
#trend between increased population having lower collisions per capita. this makes
#sense as moose tend to avoid high traffic busy areas. they are skidish so it makes
#sense that collisions are higher in less populated areas.