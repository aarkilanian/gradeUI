#Title: zc_BIOL1002_RAssignment
#Author: Zachary Chaytor
#Date: 2026-02-12

source("~/R_folder/BIOL_1002_R_assignment/ZachChaytor_BIOL1002_RAssignment.R")

#Trying to install dplyr-----------------------
install.packages("dplyr")

#Loading dplyr
library(dplyr)

#Doing a quick save
source("~/R_folder/BIOL_1002_R_assignment/ZachChaytor_BIOL1002_RAssignment.R")

#Question 2: Importing and naming moosedata
moosedata <- read.csv("~/R_folder/BIOL_1002_R_assignment/MoosePopulation.csv")
View(moosedata)
#Uploaded the data using import feature.

#Question 3: Looking at moosedata, trying to omit NA
View(moosedata)
moose_clean <- na.omit(moosedata)
na.omit(moosedata)
#Saved data as moose_clean

#Question 4: Simplifying dataset
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)

#Question 5a: oldest obs. in dataset
year_min <- min(moose_sel$Year)
#Question 5b: highest moose population
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#Question 6: Make new column "MooseDensity" call it moosedata2
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)

#Question 7: Making line graph of Moosedensity over year
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
#Looks odd but I am unsure if this is whats wanted or if there is a way to fix

#Question 8: Moose populations changes in western forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)

#Question 8b: Turning moose_west to a line graph to show change over time
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland's Western Forests ecoregion over time")
#Right code, had to click zoom in plot tab to reset margins

#Question 9a: moose_2020 filter
moose_2020 <- filter(moosedata2, Year == 2020)
View(moose_2020)

#Question 9b: Looking for density over 2
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
View(moose_2020_high)

#Question 9c: arranging in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
View(moose_2020_high_byD)

#Question 11a: making saplings dataset with import
saplings <- read.csv("~/R_folder/BIOL_1002_R_assignment/SaplingStudy.csv")
View(saplings)

#Question 11b: removing NA from data
sap_clean <- na.omit(saplings)
View(sap_clean)

#Question 12a: mean browsing by ecoregion
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarise(mean_BrowsingScore = mean(BrowsingScore)) %>% print()
View(sap_reg_browse)
# The Northern peninsula forests has the highest avg browsing score
# The strait of Belle Isle Barrens has the lowest avg browsing scores

#Question 12b: arrange descending
avg_browse_reg <- sap_reg_browse %>% arrange(desc(mean_BrowsingScore))
View(avg_browse_reg)

#Question 13a: mean height by ecoregion
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarise(mean_Height = mean(Height)) %>% print()

#Question 13b: filter low height
sap_reg_height_low <- sap_reg_height %>% filter(mean_Height < 20) %>% print()
#The northern peninsula forests and western forests ecoregions have average tree heights below 20 cm.

#Question 14a: mean browsing by species
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarise(mean_BrowsingScore = mean(BrowsingScore)) %>% print()

#Question 14b: arrange descending
avg_browse_spe <- sap_spe_browse %>% arrange(desc(mean_BrowsingScore))
View(avg_browse_spe)
#The black_ash species has the highest avg browsing score
#The black_spruce species has the lowest avg browsing score

#Question 15: Balsam Fir browsing by ecoregion
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarise(mean_BrowsingScore = mean(BrowsingScore)) %>% print()
View(fir_reg_browse)

#Question 16: Balsam Fir barplot
barplot(fir_reg_browse$mean_BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average browsing intensity", main = "Average Balsam Fir browsing intensity by ecoregion", col = "forestgreen", cex.names = 0.5, ylim = c(0, 5))
#One of my fav. colors is forestgreen, need full screen to view all labels, adjusted ylim.

#Question 17a: Black Spruce browsing by ecoregion
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarise(mean_BrowsingScore = mean(BrowsingScore)) %>% print()

#Question 17b: Black Spruce barplot
barplot(spruce_reg_browse$mean_BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average browsing intensity", main = "Average Black Spruce browsing intensity by ecoregion", col = "aquamarine", cex.names = 0.5)
#Black Spruce generally shows lower average browsing intensity than Balsam Fir.

#Question 18: tally by ecoregion
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()

#Question 19: tally by species
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()
#The SaplingStudy dataset is uneven across species. Black Ash underrepresented.

#Question 21: Moose 2020 subset and density
moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
View(moose_2020b)

#Question 21b: join with saplings
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Question 22: summary of browsing and moose density
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarise(mean_BrowsingScore = mean(BrowsingScore), mean_MooseDensity = mean(MooseDensity)) %>% print()

#Question 23b: observation
View(sum_spe_browse)
#Moose favor willow and alder the most. Black ash and black spruce are browsed the least.

#Question 24: Moose collisions and human population
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests","Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25b: rename column
moose_coll2 <- moose_coll %>% rename_with(~ "Ecoregion", study_sites)
names(moose_coll2)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion", relationship = "many-to-many")

#Question 26a: scatterplot of collisions
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density (moose per sq km)", ylab = "Number of Moose-Vehicle Collisions ", main = "Moose Density vs Moose-Vehicle Collisions in 2020", pch = 16, col = "violetred")
#Used zoom to fix error of margins

#Question 27: collisions per capita
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)

#Question 28: scatterplot per capita
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Moose-Vehicle Collisions per Person", main = "Collisions per Capita vs Human Population", pch = 17, col = "turquoise3")
#Scatterplot suggests general trend: lower human populations have higher collisions per person.

#Question 29:---------------------------------------------------------------
#The scatterplot trends indicate that ecoregions with smaller human populations generally have higher moose-vehicle collisions per person, whereas areas with larger populations show lower per-capita collision rates. This trend makes sense because in sparsely populated regions, each person is more likely to encounter moose, while in densely populated areas, the same number of collisions is distributed across more people.
source("~/R_folder/BIOL_1002_R_assignment/ZachChaytor_BIOL1002_RAssignment.R")
