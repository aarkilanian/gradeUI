# Title: BIOL 1002 R Assignment
# Author: Satya Meminger
# Date: 02-09-2026

# Question 1: Load Libraries needed 
install.packages("dplyr")
library(dplyr)

#Set working directory 
setwd("~/Desktop")

# Question 2: Import Dataset 
moosedata <- read.csv("~/Desktop/BIOL1002_Rassignment/MoosePopulation.csv")

# Question 3: 
moose_clean <- na.omit(moosedata)

# Question 4:
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5:
# Have to make vectors prior to finding min and max values
Year <- c(1904, 1940, 1960, 1980, 1992, 2020)
Estimated_Moose_pop <- c(4, 5, 200, 400, 600, 1300, 1400, 1540, 2200, 2300, 3500, 4040, 5300, 5320, 7400, 8300, 9600, 10300, 10600, 11200, 11650, 13200, 14000, 14500, 15700, 17000, 17210, 18000, 20450, 21000, 21740, 27600, 32000, 41250)
#a) 
min(Year)
1904
year_min <- min(Year)
#b)
max(Estimated_Moose_pop)
41250
moose_max <- max(Estimated_Moose_pop)

# Question 6:
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7:
#a) 
View(moosedata2)
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", 
ylab = "Moose per sq km", 
main = "Moose density in Newfoundland ecoregions over time")

# Question 8:
#a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#b)
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "year", 
ylab = "Moose per sq km", main = "Moose density in Western Forsests over time")

# Question 9:
#a) 
View(moosedata2)
moose_2020 <- filter(moosedata2, Year == "2020")

#b)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

#c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10:
moosefinal <- moosedata2 %>%
 	filter(Year == 2020) %>%
 	filter(MooseDensity > 2.0) %>%
 	arrange(desc(MooseDensity)) %>%
 	print()
  
#Sapling Study ---------------------------------------------
  
# Question 11: 
#a)
saplings <- read.csv("~/Desktop/BIOL1002_Rassignment/SaplingStudy.csv")

#b)
sap_clean <- na.omit(saplings)

# Question 12:
#a)
sap_reg_browse <- group_by(sap_clean, Ecoregion) %>%
	summarise(mean(BrowsingScore)) %>%
	print()
	
#b) 
avg_browse_reg <- group_by(sap_clean, Ecoregion) %>%
summarise(MeanScore = mean(BrowsingScore)) %>%
arrange(desc(MeanScore))
# StraitofBelleIsleBarrens had the lowest average browsing score of 1.000000, while Northern_Peninsula_Forests had the highest average browsing score of 4.571429

# Question 13:
#a)
View(sap_clean)
sap_reg_height <- group_by(sap_clean, Ecoregion) %>% summarize(mean(Height))

#b)
sap_reg_height_low <- sap_reg_height %>%
filter(.data[["mean(Height)"]] < 20) %>%
select(Ecoregion) %>%
print()
# Northern_Peninsula_Forests and Western_Forests have average heights less than 20cm. 

#Question 14:
#a) 
sap_spe_browse <- group_by(sap_clean, Species) %>%
summarize(mean(BrowsingScore)) %>%
print()

#b) 
avg_browse_spe <- sap_spe_browse %>%
arrange(desc(`mean(BrowsingScore)`))

#Question 15:
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarise(mean(BrowsingScore))

#Question 16:
# Increasing margins to fit better
par(mar = c(12, 5, 4, 2))  # bottom, left, top, right
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, col = "forestgreen", cex.names = 0.5, las = 2, ylab = "Mean Browsing Score", main = "Mean Browsing Scores of Balsam Fir Trees Based on Ecoregion") 
#las 2 to make labels vertical
# Have to add x-axis label manually and lower it using mtext feature  
mtext("Ecoregion", side = 1, line = 8)  

#Question 17
#a)
spruce_reg_browse <- sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarise(mean(BrowsingScore)) %>%
print()

#b)
#Increasing margins to fit better 
par(mar = c(12, 5, 4, 2))
# bottom, left, top, right
barplot(spruce_reg_browse$`mean(BrowsingScore)`,
names.arg = spruce_reg_browse$Ecoregion, col = "darkblue", cex.names = 0.5, las = 2, ylab = "Mean Browsing Score", main = "Mean Browsing Scores of Spruce Trees Based on Ecoregion")
# las = 2 = making labels vertical instead of horizontal
# Adding in x-axis label manually with mtext feature to remove overlapping of label with ecoregion labels
mtext("Ecoregion", side = 1, line = 8)  
# side=1 is x-axis, line moves it down that many times 

#c) 
# The browsing scores of the Spruce Trees are more variabel in numbers compared to the Balsam Fir Trees. The browsing scores are higher for Spruce Trees in the western and northern ecoregions and lower, if not 0.0 in the eastern and maritime ecoregions. balsam fir tree browsing scores are similar to that of the spruce trees in the sense that they are higher in the central and northern ecoregions, however they differ by being above 0.5 in the eastern ecoregions as opposed to 0. 

# Question 18:
sap_reg_tally<- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>% 
print()

# Question 19: 
sap_spe_tally <- sap_clean %>%
group_by(Species) %>%
tally() %>%
print()

# Question 20:
#a) I do not believe that the SaplingStudy dataset is evenly distributed. The types of saplings and how many were studied per ecoregion are not consistent. Some ecoregions measure some trees but it is not consistent across all ecoregions with all saplings. 

#b) It is important to recognice bias in ecological dataset because it cause inaccurate data, leading to potentially harmful research conclusions. It is important to be consistent and unbiased in research to avoid inaccurate results/conclusions. 

# Question 21:
#a) 
moose_2020b <- mutate(moose_clean, MooseDensity = Estimated_Moose_Pop / Area)

#b)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

# Question 22:
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarise(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE), mean_MooseDensity = mean(MooseDensity, na.rm = TRUE)) %>%
print()
#mean_'value' = is to ensure that the labels do not create error messages when running code

# Question 23:
#a) 
#According to the figure, moose do show preferences for specific saplings at low density and shift to more generalist browsing patterns at higher density. At low density, moose seem to gravitate towards willow saplings and as the average moose density increases, the more generalized the saplings become. 

#b) 
#Moose favour the willow and alder saplings the most. The least browsed sapling speciesis the black spruce species. 

#c)
#The black ash species is not shown on the figure because there is not enough data of the saplings browsing score to provide sufficient data. 
 
# Question 24:
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25:
#a)
moose_coll2 <- moose_coll %>%
rename(Ecoregion = study_sites)
#b)
coll_merge <- left_join(moose_2020, moose_coll2)

# Question 26
#a) 
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Collisions From 2020", main = "Moose Density In Relation To Moose-Vehicle Collisons")

#b)
#The graph shows that, as the number of collisions increased, the moose density was variable. There were almost just as many moose when the collision rate was low as there were when the collision rates were at their highest. There is no real trend. The moose density was at it's highest around 40-60 collisions. 

# Question 27:
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = 
collisions2020 / human_pop)

# Question 28:
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions Per Capita")

# Question 29:
# Based on the graph, it is shown that, as the human population increases, the collisions per capita decreases. The would make sense since it can be assumed that moose would avoid highly human-populated areas and gravitate towards more uninhabited lands that are best for feeding. The highest collision rates are closer to where there are less humans and more forests. 










  