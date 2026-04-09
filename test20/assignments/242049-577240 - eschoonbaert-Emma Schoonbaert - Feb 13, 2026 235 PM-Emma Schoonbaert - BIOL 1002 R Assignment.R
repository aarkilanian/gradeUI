# Author: Emma Schoonbaert
# Title: BIOL 1002 R-Studio Assignment
# Date: February 13 2026

# Question 1 ------------------------
library(dplyr)

# Question 2 ------------------------
moosedata <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv")

# Question 3 ------------------------
View(moosedata)
na.omit(moosedata)
moose_clean <- na.omit(moosedata)

# Question 4 ------------------------
select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5 A ------------------------
year_min <- min(moose_sel$Year)
print(year_min)

# Question 5 B ------------------------
moose_max <- max(moose_sel$Estimated_Moose_Pop)
print(moose_max)

# Question 6 ------------------------
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7 ------------------------
plot(moosedata2$Year, moosedata2$MooseDensity, type ="l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose Density in Newfoundland Ecoregions Over Time")

# Question 8 A ------------------------
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8 B ------------------------
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose Density in Western Forests Ecoregion Over Time")

# Question 9 A ------------------------
moose_2020 <- filter(moosedata2, Year == "2020")

# Question 9 B ------------------------
moose_2020_high <- filter(moose_2020, MooseDensity > "2.0")

# Question 9 C ------------------------
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10 ------------------------
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Question 11 A ------------------------
saplings <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")

# Question 11 B ------------------------
View(saplings)
na.omit(saplings)
sap_clean <- na.omit(saplings)

# Question 12 A ------------------------
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing=mean(BrowsingScore)) %>%
  print()

# Question 12 B ------------------------
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))

# The region with the highest average browsing score is Northern Peninsula Forests (4.57). The region with the lowest average browsing score is Strait of Belle Isle Barrens (1.00).

# Question 13 A ------------------------
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight=mean(Height)) %>%
  print()

# Question 13 B ------------------------
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < "20")
print(sap_reg_height_low)

# The ecoregions with average heights less than 20cm are Northern Peninsula Forests (19.9cm) and Western Forests (18.9cm)

# Question 14 A ------------------------
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing=mean(BrowsingScore)) %>%
  print()

# Question 14 B ------------------------
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))

# The species that has the highest browsing score is the Black Ash (5.00). The species that has the lowest browsing score is the Black Spruce (2.33)

# Question 15 ------------------------
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsingScore=mean(BrowsingScore)) %>%
  print()

# Question 16 ------------------------

barplot(
  fir_reg_browse$AverageBrowsingScore, 
  names.arg = fir_reg_browse$Ecoregion, 
  xlab = "Ecoregion", 
  ylab = "Average Browsing Intensity", 
  main = "Balsam Fir Average Browsing Intensity By Ecoregion", 
  col = "lavenderblush")

# Question 17 A ------------------------
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsingScore=mean(BrowsingScore)) %>%
  print()

# Question 17 B ------------------------

barplot(
  spruce_reg_browse$AverageBrowsingScore, 
  names.arg = spruce_reg_browse$Ecoregion, 
  xlab = "Ecoregion", 
  ylab = "Average Browsing Intensity",
  main = "Black Spruce Average Browsing Intensity By Ecoregion", 
  col = "powderblue")

# Question 17 C ------------------------

# The browsing intensity for Black Spruce is much lower than that of Balsam Fir in Eastern Hyper Oceanic Barrens as well as Maritime Barrens.

# Question 18 ------------------------
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

# Question 19 ------------------------
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

# Question 20  A ------------------------

# The Sapling Study dataset is not evenly distributed. In terms of species, Black Ash tree saplings (1n) were underrepresented compared to all other tree sapling species. Balsam Fir tree saplings (11n) were overrepresented. As for ecoregions, Strait of Bell Isle Barrens (1n) and Maritime Barrens (3n) are underepresented whereas North Shore Forests (8n) and Northern Peninsula Forests (7n) are overrepresented. 

# Question 20  B ------------------------

# It is important to recognize bias in ecological datasets so that conclusions drawn account for these outliers. For example, identifying underrepresentation and overrepresentations in values to prevent potentially deceptive interpretation. 

# Question 21 A ------------------------
moose_2020b <- moose_clean %>%  
  filter(Year == "2020") %>% 
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Question 21 B ------------------------
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

# Question 22 ------------------------
sum_spe_browse <- moose_sap %>%
  group_by(Ecoregion, Species) %>%
  summarise(AverageBrowsing=mean(BrowsingScore), 
             AverageMooseDensity=mean(MooseDensity)) %>% 
              print()   
            
# Question 23 ------------------------
install.packages("ggplot2")

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AverageMooseDensity, y = AverageBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

# a) Yes, the evidence does support the researcher's hypothesis. The graph indicates at high moose density, all sapling species are being browsed relatively equally. However, at low moose density the species are being browsed selectively, indicating that the moose eat based on preference rather than for survival.  

# b) The sapling species that moose favour the most are the Black Spruce and Willow. They browse the Alder species the least. 

# c) The sapling species not shown on the figure is the Black Ash. This is because the Black Ash species was underrepresented (1n), as seen in dataset sap_spe_tally. 

# Question 24 ------------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25 A ------------------------
moose_coll2 <- moose_coll %>% 
  rename(Ecoregion = study_sites)

# Question 25 B ------------------------
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', relationship = "many-to-many")

# Question 26 A ------------------------
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose density per sq km", 
     ylab = "Number of collisions", 
     main = "Newfoundland Collisions and Moose Density in 2020")

# Question 26 B ------------------------

# Trends seen in the scatterplot indicates as moose density increases, number of collisions increases. When the moose density is 1.0, there is an outlier in which the number of collisions is greater than 100. 

# Question 27 ------------------------
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

# Question 28 ------------------------
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population", 
     ylab = "Collisions per Person", 
     main = "Collisions per Person for Human Population Size in Newfoundland")

# Question 29 ------------------------

# Trends seen in the scatterplot indicate at lower human populations, the number of collisions per person is higher. This trend makes sense because moose accidents commonly happen in more rural areas, places with lower human population. Therefore, the number of collisions per person would be higher in locations with lower human population, as opposed to high population cities where moose density is low. 
