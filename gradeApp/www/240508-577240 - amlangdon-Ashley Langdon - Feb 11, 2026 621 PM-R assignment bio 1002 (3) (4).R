# Title: R script BIOL 1002
# Author: Ashley Langdon
# Date: 11-02-2026
install.packages("dplyr")
# Part 1 
# Question 1
library(dplyr)
# Question 2
moosedata <-read.csv("MoosePopulation.csv")
# Question 3
View(moosedata)
moose_clean <- na.omit(moosedata)
# Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# Question 5
year_min <- min(moose_sel$Year)
# a. 1904
moose_max <- max(moose_sel$Estimated_Moose_Pop)
# b. 41250
# Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
# Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
# Question 8 
# a. 
moose_west <- filter(moosedata2, Year == "2020")
# b. 
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose denisty in Newfoundland Western Fores Ecogregion")
# Question 9 
#a. 
moose_2020 <- filter(moosedata2, Year == "2020")
#b. 
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Question 10
moosefinal <- moosedata2 %>% filter (Year == 2020) %>% filter (MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()

# Part 2 
# Question 11
# a. 
saplings <- read.csv("SaplingStudy (1).csv")
View(saplings)
# b. 
sap_clean <- na.omit(saplings)
# Question 12 
# a. 
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# b. 
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
#Northern_Peninsula_Forests has the highest average browsing score.
#StraitOfBelleIsleBarrens has the lowest average browsing score. 
# Question 13
# a. 
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
# b. 
sap_reg_height_low <- filter(sap_reg_height, AverageHeight <20)
#Western_Forests and Northen_Peninsula_Forests have average heights less than 20cm 
# Question 14 
# a. 
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# b. 
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# Black_Ash has the highest Average Browsing score.
# Black_Spruce has the lowest Average Browsing score.
# Question 15 
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Question 16 
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregions",
        ylab = "Average Browsing Intensity",
        main = "The Average Browsing Intensity of Balsam Fir in Different Ecoregions",
        col = "forestgreen", cex.names = 0.6)
# Question 17
# a. 
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# b. 
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregions",
        ylab = "Average Browsing Inensity",
        main = "The Average Browsing Intensity of Black Spruce in Different Ecoregions",
        col = "yellow", cex.names = 0.6)
# c. 
# The Balsam Fir has a higher browsing in regions like: North_Shore_Peninsula, Northen_Peninsula_Forests, and Western_Forests. The Black Spruce also does, but its average browsing is still lower than the Balsam Firs.
# Question 18 
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
# Question 19 
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print() %>%
# Question 20
# a. 
# The saplingstudy data set isn't evenly distributed, certain tree species and ecoregions are overepersented and some are underrepresented.
# b. 
# It is important to recognize bias in ecological data sets because if it is not recognized it can lead you to make wrongful conclusions about the data. 

# Part 3 
# Question 21 
# a. 
moose_2020b<- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
# b. 
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
# a. 
# Yes, the figure supports the researchers hypothesis. The hypothesis is correct because for low moose density there are high browsing scores for certain species of the tree but for the high moose density the browsing score increases across majority of the tree species. There is a shift towards generalists browsing at high moose densities.
# b. 
# After looking at the following figure, it seems that the moose browse black spruce had the least amount and the willow had the most.
# c. 
# In the figure, the species is shown as black ash. Within this study this species was not as frequently found as the other species meaning there was not enough species at the site of the study to collect usable data.
# Question 24 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Martime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collision2020, human_pop, study_sites)
# Question 25
# a. 
moose_coll2 <- rename_with(moose_coll, ~ "Ecoregion",.cols = study_sites)
# b. 
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoreion", relationship = "many-to-many")
# Question 26 
# a. 
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density",
     ylab = "Number of Moose-Vechile Collision (2020)",
     main = "Moose Density in relation to Moose-Vechile Collisions")
# b. 
# As the density of the moose increases so do the number of Moose-Vehicle collisions
# In this data there are some out liars, it can been seen when the moose density is moderate, although the number is moderate the moose-vehicle collisions are super high in comparison. 
# Question 27
coll_merge_per_capita <- mutate(coll_merge,coll_per_capita = collisions2020 / human_pop)
# Question 28 
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisons",
     main = "Moose Collisons per Capita in relation to Human population")
# Question 29 
# In this data, the regions that have smaller human populations also have high moose collisions per capita, this data tells us that these two variables have a negative relationship when compared to each other.
#In conclusion, this data makes sense because within smaller communities there is less activity happening. Therefore, moose are more likely to be in these lower populated places making it normal for more moose sightings, this means that there are higher chances of moose-vehicle collisoons to occur.  