# Title: R Assignment 3
# Author: Carly Stuckless
# Date: 21-01-2026

#Set working directory -----------------------------------
setwd("/Users/susmacbook/Downloads/R Assignment Files/") 

# Part I -------------------------------------------------

# Question 1 
library(dplyr)

# Question 2 
moosedata <- read.csv("MoosePopulation.csv")

# Question 3
View(moosedata)

moose_clean <- na.omit(moosedata)

# Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5a
year_min <- min(moose_sel$Year)
# Question 5b
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# Question 8b
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in the Western Forests Ecoregion over time",
     type = "l")

# Question 9a
moose_2020 <- filter(moosedata2, Year == "2020")
# Question 9b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
# Question 9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Part II ------------------------------------------

# Question 11a
saplings <- read.csv("SaplingStudy.csv")
# Question 11b
sap_clean <- na.omit(saplings)

# Question 12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Question 12b
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
# Northern Peninsula Forests has the highest average browsing score. 
# Strait of Belle isle Barrens has the lowest average browsing score.

# Question 13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
# Question 13b
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)
# Northern Peninsula Forests and Western Forests have average heights less than 20cm. 

# Question 14a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Question 14b
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# Black ash has the highest browsing score.
# Black spruce has the lowest browsing score.

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
        col = "pink", cex.names = 0.6) 

# Question 17a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print() 
# Question 17b
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregions", 
        ylab = "Average Browsing Intensity", 
        main = "The Average Browsing Intensity of Black Spruce in Different Ecoregions", 
        col = "orange", cex.names = 0.6) 
# Question 17c
# Balsam Fir has a much higher browsing in regions such as the North Shore Peninsula, Central Forests and the Northern Peninsula Forests and that is the same for the Black Spruce. Although, Black Spruce's average browsing seems to be mostly lower than that of Balsam Fir. 

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

# Question 20a
# I do not think the SaplingStudy dataset is evenly distributed. Ecoregions like Northern Peninsula Forests and North Shore Forests and tree species such as Black Spruce and Balsam Fir are overrepresented whereas ecoregions like Strait of Belle Isle Barrens and Maritime Barrens and tree species such as willow are underrepresented. 
# Question 20b
# It is important to recognize bias in ecological datasets because it can make the data unevenly distributed, leading to incorrect conclusions about species, populations, or ecosystems. Recognizing bias helps us to interpret results more accurately. 

# Part III -----------------------------------------------------

# Question 21a
moose_2020b<- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>% 
  print()
# Question 21b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', 
                       relationship = "many-to-many")

# Question 22
sum_spe_browse<- moose_sap %>%
  group_by(Ecoregion) %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore), 
            AverageMooseDensity = mean(MooseDensity)) %>% 
  print()

# Question 23a
# Yes, the figure supports the researchers hypothesis. At low moose density, there are higher browsing scores for certain tree species whereas at higher moose densities the browsing score increases across multiple species, suggesting a shift towards more generalist browsing at higher moose densities.  
# Question 23b
# In the figure, moose seem to favor Willow the most and Black Spruce seems to be browsed the least. 
# Question 23c
# Black Ash is the species not shown on the figure. This must be because the species was either too rare or absent in the sampled sites to calculate an average browsing score. 

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)

study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens",
                 "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25a 
moose_coll2 <- rename_with(moose_coll, ~ "Ecoregion", "study_sites")
# Question 25b 
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', 
                        relationship = "many-to-many")

# Question 26a
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collsions (2020)",
     main = "Moose Density VS Moose-Vehicle Collisions")
# Question 26b
# In the plot you can see a positive relationship between moose density and moose-vehicle collisions because when moose density increases the number of moose-vehicle collisions also seemed to increase.
# There are also a few possible outliers as in some areas, the number of moose-vehicle collisions are extremely high even though the moose density is only moderate. 

# Question 27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

# Questions 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population",
     ylab = "Moose Collisions per Capita",
     main = "Moose Collisions per Capita VS Human Population")

# Question 29 
# There seems to be a negative relationship between moose collisions per capita and human population as regions with a smaller human population tend to have higher moose collisions per capita. 
# This does make sense as the more rural areas of Newfoundland tend to have smaller populations than in town, but the moose sightings are much higher in those areas, increasing their risk of colllsion. 



