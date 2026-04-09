# R Assignment - Moose Ecology Study
# Casey Sullivan - BIOL 1002
# Questions left: 20, 23, 26, 29

# Question 1

install.packages("dplyr")
library(dplyr)

# Question 2

moosedata <- read.csv("MoosePopulation.csv")
View(moosedata)

# QUestion 3

moose_clean <- na.omit(moosedata)

# QUestion 4

moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5

year_min <- min(moose_sel$Year)

moose_max <- max(moose_sel$Estimated_Moose_Pop)

# QUestion 6

moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7

plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# QUestion 8

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density over time in western forest regions")

# Question 9

moose_2020 <- filter(moosedata2, Year == 2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# QUestion 10

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# QUestion 11

saplings <- read.csv("SaplingStudy.csv")

sap_clean <- na.omit(saplings)

# QUestion 12

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print() 

avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing)) %>%
  print()

# The region with the highest mean browsing score was the Northern Peninsula with a score of 4.57. 
# Following that, the lowest mean browsing score was in the strait of belle isle barrens region. Which only had a browsing score of 1


# Question 13 

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageTreeHeight = mean(Height)) %>%
  print() 

sap_reg_height_low <- filter(sap_reg_height, AverageTreeHeight < 20)

# The forest regions which have average heights lower than 20cm are the northern peninsula and the western forests. 
# This means it is likely that these two regions suffer from the most moose browse

# Question 14

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print() 

avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing)) %>%
  print()

# The species with the highest browsing score was the Black Ash with a score of 5.
# THe species which has the lowest browsing score is the Black spruce, with a score of 2.33.

# Question 15 

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print() 

# QUestion 16

barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Moose Browsing Score", main = "Average moose browsing score on Balsam Fir Trees", col = "hotpink", cex.names = 0.6)

# Question 17

spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print() 

barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Moose Browsing Score", main = "Average moose browsing score on Black Spruce Trees", col = "blue", cex.names = 0.6)

# While the highest browsing scores are the north shore forest and the northern peninsula forests for both of the species. The black spruce also contains a high browsing score for western forests. Which are not present in the Balsam fir data.
# Additionally the Eastern hyper oceanic barrens and the maritime barrens both have a browsing score of 0 for the black spruce species. Whereas for the balsam fir data, they both have a browsing score of 2.

# QUestion 18

sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

# Question 19 

sap_reg_tall <- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

# Question 20 
# a) The sapling study data set is not fully distributed. Eco regions that are under represented are Strait Of Belle Isle Barrens eco region with 1 and Black Ash Species is underrepresented with 1. 
# Eco regions that are over populated are North shore forests with 8 and northern peninsula forests with 7. Over represented specie is Balsam Fir with 11.
# b) Its important to reconzie bias in ecological datasets becasue it can show bias in research that effect the accuracy in results and need to be addressed to help conservation attempts.

# Question 21

moose_2020b <- filter(moose_clean, Year == "2020")
  
moose_2020b <- mutate(moose_2020b, MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

# Question 22


sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore), AverageMooseDensity = mean(MooseDensity)) %>%
  print() 

 # Question 23 

install.packages("ggplot2")
library(ggplot2)

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AverageMooseDensity, y = AverageBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

# Yes, at low densities, moose show the strongest browsing preferences for the alder and willow. The data points for the other tree species are all much lower. While at a higher moose density, all species have much higher browsing score and points clustered closer together.
# The sapling species favored the most is the willow. Willows has the highest browsing score at both high and low densities. The Alder saplings is favored at low density, but less when the density is higher.
# The species not shown is the Black Ash. It only has a single data point which overlaps with the willow point a at 5.0 browsing score and 2.5 moose density.

# Question 24 

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25 

moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")

# Question 26

plot(coll_merge$collisions2020, coll_merge$MooseDensity, 
     xlab = "Moose Collisions in 2020", 
     ylab = "Moose per sq km", 
     main = "Number of Moose collisions in 2020 by region compared to moose density")
#Collisions occur more when moose have a higher density.
#There is an outlier point on the right side of the graph which represents a region with a very high number of collisions even though the moose density was not high.

# QUestion 27

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# Question 28

plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human population", 
     ylab = "Moose collisions per capita", 
     main = "The number of moose collisions per captia by population size")

# QUestion 29
  
#The highest moose collisions per capita occurs in places where the population is lower. It makes sense since urban regions with a higher population are likely to have less forest to support moose. Rural regions will contain more forest and have a higher density of moose. Making the roads more dangerous.

