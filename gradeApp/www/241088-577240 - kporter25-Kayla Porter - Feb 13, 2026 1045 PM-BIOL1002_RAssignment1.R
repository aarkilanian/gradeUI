# Title: My R script for R assignment 1

# Author: Kayla Porter

# Date: 13-02-2026

# Load libraries needed ---------------------------
library(dplyr)

# Set working directory ---------------------------
setwd("/Users/kaylaporter")

# Load data ---------------------------
moosedata <- read.csv("~/Downloads/MoosePopulation (1).csv")
saplings <- read.csv("~/Downloads/SaplingStudy.csv")
# Analyze data ---------------------------

# PART I
# Question 3
na.omit(moosedata)
mooseclean <- na.omit(moosedata)

# Question 4
select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5
min(1904)
year_min <- min(1904)
max(41250)
moose_max <- max(41250)

# Question 6
mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 8
filter(moosedata2, Ecoregion == "Western_Forests")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 9 
filter(moosedata2, Year == "2020")
moose_2020 <- filter(moosedata2, Year == "2020")
filter(moose_2020, MooseDensity > 2.0)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10
moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity))
  print(moosefinal)

# PART II
# Question 11 
na.omit(saplings)
sap_clean <- na.omit(saplings)

# Question 12
sap_reg_browse <- group_by(sap_clean, Ecoregion)%>%
summarize(sap_clean, mean(BrowsingScore))
arrange(sap_clean, desc(BrowsingScore))
avg_browse_reg <- arrange(sap_clean, desc(BrowsingScore))
# Highest score: North Shore Forests
# Lower score: Maritime Barrens

# Question 13
sap_reg_height <- group_by(sap_clean, Ecoregion)%>%
summarize(sap_clean, mean(Height))%>%
print(sap_reg_height)%>%
sap_reg_height_low <- filter(sap_clean, Height < 20)
# Ecoregions hight < 20cm: North Shore Forests, Northern Peninsula Forests, 
# Central Forests, Western Forests

# Question 14
sap_spe_browse <- group_by(sap_reg_height_low, Species)%>%
summarize(sap_reg_height_low, mean(BrowsingScore))
avg_spe_browse <- arrange(sap_reg_height_low, desc(BrowsingScore))
# Highest score: Willow, Lowest: Alder

# Question 15
sap_clean%>%
filter(Species == "Balsam_Fir")%>%
group_by(Ecoregion, mean(BrowsingScore))
fir_reg_browse <- sap_clean%>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion, mean(BrowsingScore))

# Question 17a
sap_clean%>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion, mean(BrowsingScore))
spruce_reg_browse <- sap_clean%>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion, mean(BrowsingScore))

# Question 18 
sap_clean %>%
group_by(Ecoregion) %>%
tally() %>% 
print()
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

# Question 19
sap_clean %>%
group_by(Species) %>%
tally() %>% 
print()
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

# Question 20
# While it may seem that the data is unevenly distributed across the ecoregions, 
# we also have to take in account for the sheer size of these regions. Some of 
# them span across nearly a quarter or more of the island itself, whereas others
# may only be a small section of the island. With this in mind, I personally 
# think that the ecoregions and their trees are distributed evenly across the 
# dataset. 
# It's important to recognize bias in ecological datasets because unidentified 
# ecological species lead to incorrect conclusions. These conclusions therefore 
# go on to produce ineffective or harmful consequences.  

# PART III
# Question 21 
moose_clean%>%
filter(Year == "2020")%>%
mutate(moose_2020, MooseDensity)
moose_2020b <- mutate(moose_2020, MooseDensity)
moose_sap <- left_join(moose_2020b, sap_clean, 
by = 'Ecoregion', 
relationship = "many-to-many")

# Question 22 
sum_spe_browse <- group_by(moose_2020b, Species, Ecoregion)%>%
summarize(moose_2020b, mean(BrowsingScore), mean(MooseDensity))
  tally() %>% 
  print()

# Question 23
#a, Yes, there is some evidence to support the researchers' hypothesis. At low 
# density there is a wider range of browsing scores which indicates stronger 
# preferences. At higher densities, the browsing scores tend to gather in one
# area which suggests a more generalist browsing. 
#b, Moose appear to favor willow saplings the most, as they have the highest
# browsing scores at higher densities. Black ash is the sapling they favor the
# least as they do not even appear on the graph.
#c, Black ash is not shown on the graph which may be due to their low browsing
# intensity. 
  
# Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", 
              "Long_Range_Barrens","Central_Forests",
              "Western_Forests","EasternHyperOceanicBarrens",
              "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
  moose_coll <- data.frame(collisions2020, human_pop, study_sites) 
  
# Question 25
moose_coll2 <- moose_coll%>%
rename(Ecoregion = study_sites)
left_join(moose_2020, moose_coll2, 
          by = 'Ecoregion', 
          relationship = "many-to-many")

# Question 27 
coll_merge_per_capita <- mutate(moose_2020, coll_per_capita = 
                                  collisions2020 / human_pop)
mutate(moose_2020, coll_per_capita = collisions2020 / human_pop)
   
# Question 29 
# There is a negative relationship between human population and moose 
#collisions, meaning that in areas with smaller human populations, the moose
# collision rate is high. In contrast to higher populations with smaller 
# collisions. 

# Plot data ---------------------------

# Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8b
plot(moose_west$Year, moose_west$Western_Forests, 
     type = "l", 
     xlab = "Year", 
     ylab = "Moose per sq km",
     main = "Moose density in newfoundland Western Forests over Time")

# Question 16
barplot(fir_reg_browse$BrowsingScore, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Intensity", 
        main = "Average browsing Intensity of Balsam Fir by Region", 
        col = "forestgreen", 
        cex.names = 0.6) 

# Question 17b 
barplot(spruce_reg_browse$BrowsingScore, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Intensity", 
        main = "Average browsing Intensity of Black Spruce by Region", 
        col = "forestgreen", 
        cex.names = 0.6) 
# Black spruce are much less frequent in Maritime Barrens compared to that 
#of Balsam Fir. Both trees are very common in North Shore Forests but overall, 
#Black Spruce are much less common in the Ecoregions than that of Balsam Fir. 

# Question 26
MooseDensity <- c(2.6666667, 2.5880952, 2.5000000, 2.0121951, 0.9619048, 
0.9600939, 0.6022727, 0.5000000, 0.0100000)
  plot(collisions2020, MooseDensity, pch = 17, 
     xlab = "Number of moose-vehicle collisions in 2020", 
     ylab = "Moose Density",
     main = "Moose density relative to the number of moose-vehicle collisions")
# The more the density of moose, relatively means the more vehicle 
# collisions that happen. An outlier is with a relativly small density (0.5), 
# the more the moose-vehicle collisions. This may be due to the rarity of the 
# moose meaning drivers are not as cautious as they would be with more moose. 
  
# Question 28
coll_per_capita <- c(0.0031111111, 0.0050000000, 0.0035000000, 0.0004793609, 
                     0.0020000000, 0.0028571429, 0.0012500000, 0.0004074074, 
                     0.0026086957)
  plot(human_pop, coll_per_capita, pch = 17,
     xlab = "Human Population", 
     ylab = "Collisions Per Capita",
     main = "Moose Collisions Per Human Population") 
  
  
  
  
  
  
