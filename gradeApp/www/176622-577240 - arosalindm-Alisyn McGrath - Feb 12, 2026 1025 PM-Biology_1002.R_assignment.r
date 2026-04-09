# Title: Biology 1002 R assignment
# Name: Alisyn McGrath
# Date: 12-02-2026
# Question 1: Load dplyr functions
library(dplyr)
# Question 2: Import MoosePopulation dataset, name it moosedata
moosedata <- read.csv("MoosePopulation (2).csv")
# Question 3: Look through data using view function
View(moosedata)
#Write line of code to remove rows with missing values
moose_clean <- na.omit(moosedata)
# Question 4: Simplify dataset to only include columns of interest
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# Question 5A: Find the oldest year in the dataset
year_min <- min(moose_sel$Year)
year_min
#1904
# Question 5B: What is the highest population recorded?
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
#41250
# Question 6: Standardize data by calculating moose densitry for each ecoregion
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)
# Question 7: Make graph to show density changes over year
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
# Question 8A: Use filter function to only include western forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# Question 8B: Line graph showing change in moose density over time in western region
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Western Forests over time")
# Question 9A: Filter the year 2020
moose_2020 <- filter(moosedata2, Year == 2020)
# Question 9B: Filter to only show regions greater than 2.0
moose_2020_high <- filter(moosedata2, MooseDensity > 2.0)
# Question 9C: Arrange to sort density column in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
# Question 10: Use pipes on question 9 a,b and c
moosefinal <- moosedata2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()
# Question 11A: Load sapling dataset
saplings <- read.csv("SaplingStudy (1).csv")
# Question 11B: Remove NA's from dataset
sap_clean <- na.omit(saplings)
# Question 12A: How does moose browsing pressure vary across different ecoregions
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
#It varies slightly between the different ecoregions, the lowest being 1 and highest being 4.57.
# Question 12B: Which ecoregions have the highest and lowest amount of moose browsing?
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
avg_browse_reg
# Northern_Peninsula_Forests had the highest and StraitOfBelleIsleBarrens had the lowest average browsing scores
# Question 13A: How does avg tree height vary across different ecoregions?
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height)) %>% print()
#The average tree height variess quite a bit with the tallest being 32.4 and shortest being 18.9.
# Question 13B: Which Ecoregions have average heights less than 20cm?
sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20) %>% print()
sap_reg_height_low
# Northern_Peninsula_Forests and Western_Forests ecoregions have average heights less than 20 cm.
# Question 14A: How does avg browsing score vary across different tree sapling species?
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
#Average browsing varies slightly with the highest being 5 and lowest being 2.33.
# Question 14B: Which species have the highest and lowest browsing?
avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing))
avg_browse_spe
#Black_Ash has the highest and Black_Spruce has the lowest browsing score.
# Question 15: How Balsam Fir browsing intensity varies by ecoregion.
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
# Question 16: Balsam Fir avg browsing intensity bar graph.
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Avg Browsing Intensity", main = "Balsam Fir browsing intensity by ecoregion", col = "lightblue", cex.names = 0.6)
# Question 17A: How Black spruce browsing intensity varies by ecoregion.
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
# Question 17B: Black Spruce avg browsing intensity bar graph
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Avg Browsing Intensity", main = "Black Spruce Browsing Intensity by Ecoregion", col = "coral", cex.names = 0.6)
# Question 17C: How does Black Spruce browsing compare to Balsam Fir browsing across ecoregions?
# Both Black Spruce and Balsam Fir have highest browsing in North_Shore_Forests and Northern_Peninsula_Forests. Balsam Fir has a higher browsing intensity than Black Spruce in most ecoregions, except for in Long_Range_Barrens where Black Spruce is 1.0 higher.
# Question 18: Were the same number of tree saplings counted in each Ecoregion?
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()
#no, although majority counted was 5, the amount does vary among others.
# Question 19: Were the same number of tree saplings counted in each Species?
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()
#no, only two species were the same number, all other species vary.
# Question 20A: Was the SaplingStudy dataset evenly distributed?
#I feel the data was not evenly distributed. For Ecoregions its clear that StraitOfBelleIsleBarrens was underrepresented with only 1 counted, comparing that to North_Shore_Forests that had 8. For tree species i feel the same, Balsam_Fir had 11 counted which i feel it was over represented, compared to Black_Ash that only had 1.
# Question 20B: Why is it important to recognize bias in ecological datasets?
#Its important to recognize bias because if we don't, it could cause discrepancies in the data we're trying to represent. Having more representation in one area over another can also cause us to miss things that may show up in the area that is being under represented.
# Question 21A: Use original moose dataset create new column called MooseDensity.
moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
# Question 21B: Join datasets matching rows by the common ecoregion column.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
# Question 22: Calculate avg browsing score and avg moose density for each species within each ecoregion.
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore), AverageMooseDensity = mean(MooseDensity)) %>% print()
# Question 23A: Is there evidence that supports the researchers' hypothesis?
#yes there is evidence to support their hypothesis. When the density of moose is lower, theres more sporatic browsing, some species of tree higher than others, all over the place. When the moose density is higher, the browsing score is more uniform in the higher averages, favoring similar species.
# Question 23B: Which sapling species do moose favour the most? Which do they browse the least?
#The Moose favour the Willow species the most as it has the highest avg browsing score at the highest moose density, the least browsed would be Black Spruce as its browsing score is lower than every other species throughout the moose density.
# Question 23C: Which sapling species is not shown on the figure and why?
#Black Ash is not shown on the figure because there was only 1 counted, there was not enough data to fairly represent.
# Question 24: Create dataset using vectors for moose vehicle collisions.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 27000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll
# Question 25A: Correct datatset to be able to join them.
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
# Question 25B: Join dataset.
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
coll_merge
# Question 26A: Create scatterplot of moose density and collisions2020.
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Moose Vehicle Collisions 2020", main = "Moose Density vs Collisions in 2020")
# Question 26B: What trends do you see? Are there any outliers?
#The scatterplot shows that there is a trend with moose density increase causing vehicle collisions to increase, some areas there is a slight drop in collisions the higher the moose density but then it continues upwards. One that stands out to me is the 110 collisions with the moose density only being at about 0.9-1.0.
# Question 27: Which ecoregions have the highest number of moose collisions per person?
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita
#Northern_Peninsula_Forests and Avalon_Forests have the highest number of moose collisions per person.
# Question 28: Create scatterplot of collisions per capita vs human population.
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop, xlab = "Collisions per Capita", ylab = "Human Population", main = "Collisions per Capita vs Human Population")
# Question 29: What trends do you see? Does this make sense based on what you know about moose and human populations in NL?
#i don't see any complete trends, i can slightly see that some areas of a higher human population have less collisions per capita but this does fluctuate throughout the graph. This doesn't fully make sense to me given what i know about our moose and human population as i predicted areas with a higher human population would have a higher amount of collisions  per capita, despite the graph showing that the area with the highest human population (and moose population) actually has the lowest collisions, at less than 0.0005.