# Title: BIOL 1002 Quantitative Methods Assignment
# Author: John Cooper ID:202412640
# Date: 12-02-2026

#Install dplyr package

#Part I: Moose Populations in Newfoundland

#Question 1: Load libraries needed 
library(dplyr)

#Set working directory 
setwd("~/Downloads/BIOL1002_Rassignment")

#Question 2: Read data
moosedata <- read.csv("MoosePopulation.csv")

#Question 3: View and Clean data
View(moosedata)

moose_clean <- na.omit(moosedata)

#Question 4: Simplify data
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5a: Find oldest observation 
year_min <- min(moose_sel$Year)
year_min
#Question 5b: Find highest Moose population recorded
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max

#Question 6: Standardize data 
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7: Visualize data
plot(moosedata2$Year, moosedata2$MooseDensity,
xlab = "Year",
ylab = "Moose per sq km",
main = "Moose density in Newfoundland ecoregions over time")

#Question 8a: Filter data to only include Western Forest
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Question 8b: Plot a line graph for the Western Forest moose density
plot(moose_west$Year, moose_west$MooseDensity,
type = "l",
xlab = "Year",
ylab = "Moose per sq km",
main = "Moose density in Western Forests over time")

#Question 9a: Filter for the year 2020
moose_2020 <- filter(moosedata2, Year == 2020)
#Question 9b: Filter for moose density greater than 2.0
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#Question 9c: Arrange to sort moose density in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD

#Question 10: Repeat Question 10 using pipes and save final result 
moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()

#Part II: Tree Sapling Study

#Question 11a: Read data
saplings <- read.csv("SaplingStudy.csv")
#Question 11b: Clean data
sap_clean <- na.omit(saplings)

#Question 12a: Mean Browsing Score by Ecoregion
sap_reg_browse <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()
#Question 12b: Rearrange data in decreasing order 
avg_browse_reg <- sap_reg_browse %>%
arrange(desc(AverageBrowsing)) %>%
print()
#Highest average browsing average: Northern Peninsula Forests
#Lowest average browsing average: Strait Of Belle Isle Barrens

#Question 13a: Mean tree height across ecoregions
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(AverageHeight = mean(Height)) %>%
print()
#Question 13b: Filter for average height under 20cm
sap_reg_height_low <- sap_reg_height %>%
filter(AverageHeight < 20) %>%
print()
#Ecoregions with average heights under 20cm: Northern Peninsula Forests and Western Forests

#Question 14a: Mean browsing score by species
sap_spe_browse <- sap_clean %>%
group_by(Species) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()
#Question 14b:Rearrange data in decreasing order
avg_browse_spe <- sap_spe_browse %>%
arrange(desc(AverageBrowsing)) %>%
print()
#Highest average browsing score: Black Ash
#Lowest average browsing score: Black Spruce

#Question 15: Filter Balsam Fir and sort by ecoregion
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()

#Question 16: Bar graph for Balsam Fir
barplot(fir_reg_browse$AverageBrowsing,
names.arg = fir_reg_browse$Ecoregion,
xlab = "Ecoregion",
ylab = "Average Browsing Score",
main = "Average Balsam Fir Browsing by Ecoregion",
col = "forestgreen",
cex.names = 0.6)

#Question 17a: Black Spruce summary table
spruce_reg_browse <- sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()
#Question 17b: Black Spruce bar graph
barplot(spruce_reg_browse$AverageBrowsing,
names.arg = spruce_reg_browse$Ecoregion,
xlab = "Ecoregion",
ylab = "Average Browsing Score",
main = "Average Black Spruce Browsing by Ecoregion",
col = "darkgreen",
cex.names = 0.6)
#Question 17c: Black Spruce vs. Balsam Fir
#Both Black Spruce and Balsam Fir show highest browsing in North Shore Forests and Northern Peninsula Forests.
#But Balsam Fir shows higher browsing intensity across most ecoregions,
#while Black Spruce shows very low or zero browsing in some regions, suggesting most moose probably prefer Balsam Fir.

#Question 18: Trees counted in each ecoregion
sap_reg_tally <- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>%
print()

#Question 19: Trees counted in each species
sap_spe_tally <- sap_clean %>%
group_by(Species) %>%
tally() %>%
print()

#Question 20a: Is the SaplingStudy dataset evenly distributed?
#The SaplingStudy dataset is not evenly distributed across ecoregions or species.
#Some regions like North Shore Forests and species like Balsam Fir are overrepresented,
#while others like Strait Of Belle Isle Barrens and Black Ash are underrepresented. 
#Question 20b: Why is it important to recognize bias in ecological datasets?
#Recognizing bias is important because uneven sampling can distort conclusions about browsing intensity.
#Overrepresented regions or species may exaggerate trends, while underrepresented groups may not
#accurately reflect true patterns.

#Part III: Creating and Joining Datasets

#Question 21a: Filter 2020 and create MooseDensity
moose_2020b <- moose_clean %>%
filter(Year == 2020) %>%
mutate(MooseDensity = Estimated_Moose_Pop / Area)
#Question 21b: Join moose density with sapling data
moose_sap <- left_join(moose_2020b, sap_clean,
by = "Ecoregion",
relationship = "many-to-many")

#Question 22: Average browsing + average density for each species in each ecoregion
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(
AvgBrowsing = mean(BrowsingScore),
AvgDensity = mean(MooseDensity)
) %>%
print()

#Question 23: Visualize Plot

#install ggplot2 package to view ggplot

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#Question 23a: Do moose show strong preferences at low density and shift to general browsing at high density?
#The figure supports the researchers’ hypothesis.
#At lower moose densities, browsing intensity varies across species, suggesting selective feeding.
#At higher densities, browsing scores become high across species,
#indicating reduced selectivity due to increased competition.
#Question 23b: Which saplings do moose favour the most and the least?
#Moose appear to favour Willow and Balsam Fir, as they consistently show high browsing scores.
#Black Spruce appears to be browsed the least, particularly at lower moose densities.
#Question 23c: Which sapling is not shown on the figure and why?
#Black Ash is not shown in the figure.
#This could be because there were too few observations for this species
#in the merged 2020 dataset to calculate meaningful averages.

#Question 24: Create moose-vehicle collisions dataset
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25a: Rename column and join 
moose_coll2 <- moose_coll %>%
rename(Ecoregion = study_sites)
#Question 25b: Join datasets into new dataset
coll_merge <- left_join(moose_coll2, moose_2020b,
by = "Ecoregion")

#Question 26a: How does moose density related to moose-vehicle collisions 
plot(coll_merge$MooseDensity,
coll_merge$collisions2020,
xlab = "Moose Density",
ylab = "Moose-Vehicle Collisions",
main = "Moose Density vs Collisions (2020)")
#Question 26b: What trends can you see from the plot? (and outliers)
#There appears to be a positive relationship between moose density and the number of collisions,
#as regions with higher moose density generally show more vehicle collisions.
#However, one region shows very high collisions at moderate density,
#suggesting that human population size may also influence collision frequency.

#Question 27: Highest moose-vehicles collisions per capita
coll_merge_per_capita <- coll_merge %>%
mutate(coll_per_capita = collisions2020 / human_pop)

#Question 28: Create plot to show collisions per capita vs human population
plot(coll_merge_per_capita$human_pop,
coll_merge_per_capita$coll_per_capita,
xlab = "Human Population",
ylab = "Collisions per Person",
main = "Collisions per Capita vs Human Population")

#Question 29: Visible trends in the plot
#There appears to be a negative relationship between human population size and collisions per capita.
#Regions with smaller human populations tend to have higher collisions per person,
#while highly populated regions show lower per-capita collision rates.
#This makes ecological sense, as rural areas likely overlap more with moose habitat
#and have less infrastructure to reduce moose-vehicle interactions.