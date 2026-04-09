# Title: My R Script for Bio 1002
# Author: Nathalie Quinit
# Date: 10-02-2026

# Set working directory
setwd("/Users/natha/OneDrive/Documents/Bio1002__RAssignment")


## Part 1: Moose Population in Newfoundland

# install dplyr
install.packages("tidyverse")

#Q1
# Load library
library(dplyr)

#Q2
# Load data
moosedata <- read.csv("MoosePopulation.csv")

#Q3 
## Removing "NA" or the missing values in the dataset
moose_clean<-na.omit(moosedata)

#Q4
## Simplify dataset to only include the columns of interest
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)


#Q5a.
## The oldest observation
Year<- c(2020,1992,1980,1960,1940,1904)

year_min<-min(Year)

#Q5b.
## The highest Estimated_Moose_Pop recorded
Estimated_Moose_Pop<- c(21740,32000,5300,41250,17000,2200,20450,4040,5,14000,21000,1540,27600,11650,1400,14500,5320,7400,15700,400,13200,600,8300,200,10600,18000,1300,17210,14500,10300,200,3500,9600,11200,230,4)

moose_max<-max(Estimated_Moose_Pop)

#Q6

##Create new column called MooseDensity
moosedata2 <- mutate(moose_sel,MooseDensity = Estimated_Moose_Pop/Area)

#Q7a.

##Make a line graph
plot(moosedata2$Year,moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")


#Q8a. 
##How moose Population changed over time in Western Forest Ecoregion ## 

moose_west<- filter(moosedata2, Ecoregion=="Western_Forests")

nrow(moose_west)
head(moose_west)


#Q8b. 

##Make a line graph
plot(moose_west$Year,moose_west$MooseDensity, type = "l",
     xlab= "Year",
     ylab= "Moose per sq km",
     main= "Moose density in Western Forest Ecoregion over time")

#Q9a. 

## Use filter function
moose_2020 <- filter(moosedata2, Year=="2020")

#Q9b.

##filter the MooseDensity to be >2.0
moose_2020_high <- filter (moosedata2, MooseDensity >"2.0")

#Q9c.

##Use arrange function
moose_2020_high_byD <- arrange(moosedata2, desc(MooseDensity))

#Q10

##Use pipes
moosefinal <- moosedata2 %>%
  filter(Year=="2020") %>%
  filter (MooseDensity > "2.0") %>%
  arrange(desc(MooseDensity))%>%
  print()

## Part 2: Tree Sapling Study ##


#Q11a.
##Load the data

saplings <- read.csv("SaplingStudy.csv")

#Q11b.

##remove NA
sap_clean <-na.omit(saplings)

#Q12a.
## New database using pipes##

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(MooseBrowsingScore = mean(BrowsingScore))
print(sap_reg_browse)

#Q12b.

##Rearrange dataset to decreasing
avg_browse_reg <- arrange(sap_reg_browse, desc(MooseBrowsingScore))

# The region with highest avg browsing score is Norther_Peninsula_Forests
# The region with lowest avg browsing score is StraitOfBelleIsleBarrens

#Q13a.

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Height = mean(Height))

print(sap_reg_height)

#Q13b. 

sap_reg_height_low <- sap_reg_height %>%
  filter(Height < "20")

print(sap_reg_height_low)

## Both Northern_Peninsula_Forests and Western_Forests have average heights less than 20cm ##

#Q14a.

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(BrowsingScore=mean(BrowsingScore))
print(sap_spe_browse)

#Q14b.

avg_browse_spe <- arrange(sap_spe_browse, desc(BrowsingScore))

##Black_Ash has the highest and Black_Spruce has the lowest browsing score

#Q15

fig_reg_browse <- filter(sap_clean, Species == "Balsam_Fir")

fig_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MooseBrowsingScore = mean(BrowsingScore))


#Q16

barplot(fig_reg_browse$MooseBrowsingScore,
        names.arg = fig_reg_browse$Ecoregion,
        xlab= "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

#Q17a.

spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MooseBrowsingScore = mean(BrowsingScore))

#b.

barplot(spruce_reg_browse$MooseBrowsingScore,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Black Spruce Browsing Intensity by Ecoregion",
        col = "black",
        cex.names = 0.6)

#c. 

## Black Spruce generally experience lower browsing intensity than Balsam Fir across most regions
## However, both species show similar patterns with their lowest - barren regions- and their 
##highest - northern and western forests - browsed ecoregions.

#Q18

## Count the number of trees in each region
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally ()

print(sap_reg_tally)

#Q19

##Count the number of trees for each species

sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()
print(sap_spe_tally)

#Q20a.

##Base on the tally results, the SaplingStudy dataset is not evenly distributed
## For ecoregions, North_Shore_Regions and Northern_Peninsula are over represented compared to
## StraitOfBelleIsleBarrens and Maritime_Barrens.
## For species, Balsam Fir and Black Spruce are overrepresented compared to Black Ash.

#b.

# It is crucial to recognize bias in ecological datasets because it can lead to incorrect conclusions about ecosystem patterns
#If certain regions or species are underrepresented, we might understimate the moose browing patterns in those areas,
#which can lead to wrong assumptions about the consistency of the patterns. 


## Part 3: Creating and Joining Datasets


#Q21 

moose_2020b<- moose_clean %>%
  filter(Year=="2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop/Area)

#b.

moose_sap<- left_join(moose_2020b,sap_clean, by = "Ecoregion", relationship = "many-to-many")

#Q22

sum_spe_browse<- moose_sap %>%
  group_by(Ecoregion, Species) %>%
  summarize(BrowsingScore = mean(BrowsingScore),
            MooseDensity = mean(MooseDensity))%>%
  print()

#Q23

library(ggplot2)

ggplot(sum_spe_browse, aes(x = MooseDensity, y = BrowsingScore, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#a. Yes - at low densities, the browsing score vary widely by species, but at high densities, all species show similar high browsing scores.

#b. Balsam Fir and Willow are most favored with the highest browsing scores while Black Spruce and Adler are least favored.

#c. Black Ash is not shown because only one individual was sampled; there is no relieable average that could be calculated.

#Q24

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25a.

moose_coll2<- rename(moose_coll, Ecoregion = study_sites)

#b.

coll_merge<- left_join(moose_2020, moose_coll2, by = "Ecoregion")

#Q26a.

plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose per sq km)",
     ylab = "Number of Moose-Vehicle Collisions (2020)",
     main = "Relationship Between Moose Density and Vehicle Collisions",
     pch = 19,
     col = "brown")

#b. The plots shows a general positive trend but one site shows and unusually high collisions compared to its moose density.
#This suggest that other factors like road density or traffic volume might have an influence on the collision rates.


#Q27

coll_merge_per_capita<- coll_merge %>% 
  mutate(coll_per_capita = collisions2020/ human_pop)
head(coll_merge_per_capita)


#Q28

plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose-Vehicle Collision per Capita",
     main = "Moose-Vehicle Collisions per Person by Human Population",
     pch = 19,
     col = "red")

#Q29
## There is a negative trend because as human population increase, collision per capita decrease.
## This make sense because we know that remote regions with small populations - like Western Forests - have higher moose density compared to
## populated areas because moose tend to avoid urban regions, lowering the collision rates per capita.