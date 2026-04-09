install.packages(dplyr)
#
#
#Question 1:
library(dplyr)
#
#
#Question 2:
MoosePopulation <- read.csv("~/Downloads/MoosePopulation.csv")
moosedata <- MoosePopulation
rm(MoosePopulation)
#
#
#Question 3:
View(moosedata)
moose_clean <- na.omit(moosedata)
#
#
#Question 4:
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#
#
#Question 5:
year_min <- min(moose_sel$Year, na.rm = TRUE)
#The oldest year is 1904.
#
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
#The highest estimated moose population is 41250.
#
#
#Question 6:
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#
#
#Question 7:
plot(moosedata2$Year, moosedata2$MooseDensity, 
xlab = "year", 
ylab = "Moose per sq km", 
main = "Moose density in Newfoundland ecoregions over time")
#
#
#Question 8:
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#
plot(moose_west$Year, moose_west$MooseDensity, 
xlab = "year", 
ylab = "Moose per sq km", 
main = "Moose density in Western Newfoundland Forests over time", type="l")
#
#
#Question 9:
moose_2020 <- moosedata2 %>% filter(Year == 2020)
#
moose_2020_high <- moose_2020 %>% filter(MooseDensity > 2.0)
#
arrange(moose_2020_high, desc(MooseDensity))
#
#
#Question 10:
moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()
#
#
#Question 11:
SaplingStudy <- read.csv("~/Downloads/SaplingStudy.csv")
saplings <- SaplingStudy
rm(SaplingStudy)
#
sap_clean <- na.omit(saplings)
#
#
#Question 12:
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
#
avg_browse_reg <- sap_reg_browse %>% arrange(desc(mean_browsing))
print(avg_browse_reg)
#The highest average browsing score is in the Nothern Peninsula Forests and the lowest average browsing score is in the Strait Of Belle Isle Barrens.
#
#
#Question 13:
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(mean_height = mean(Height, na.rm = TRUE))
print(sap_reg_height)
#
sap_reg_height_low <- sap_reg_height %>% filter(mean_height < 20)
print(sap_reg_height_low)
#Northern Peninsula Forests and Western Forests have average heights less than 20cm.
#
#
#Question 14:
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
#
avg_browse_spe <- sap_spe_browse %>% arrange(desc(mean_browsing))
print(avg_browse_spe)
#Black Ash has the highest browsing score and Black spruce has the lowest browsing score.
#
#
#Question 15:
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(fir_reg_browse)
#
#
#Question 16:
barplot(fir_reg_browse$mean_browsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Balsam Fir Browsing Intensity by Ecoregion", col = "forestgreen", cex.names = 0.6)
#
#
#Question 17:
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(spruce_reg_browse)
#
barplot(spruce_reg_browse$mean_browsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Black Spruce Browsing Intensity by Ecoregion", col = "darkgreen", cex.names = 0.6)
#
#Numerous ecoregions show that Balsam Fir have a higher browsing intensity than Black Spruce, but some ecoregions show similar browsing intensity between the two.
#
#
#Question 18:
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()
#No, the same number of tree saplings were not counted in each ecoregion.
#
#
#Question 19:
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally()
print(sap_spe_tally)
#No, the same number of tree saplings were not counted for each species.
#
#
#Question 20:
#No, the SaplingStudy dataset is not evenly distributed, Balsam Fir is over represented and Black Ash is underrepresented, and the Northern Peninsula Forests are over represented while the Long Range Barrens are underrepresented.
#
#It's important to recognize bias in ecological datasets because uneven sampling can lead to inaccurate representations of browsing intensity which then draws skewed conclusions.
#Recognizing these biases helps to ensure that conclusions represent the systems and not the sampling species'.
#
#
#Question 21:
moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
print(moose_2020b)
#
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#
#
#Question 22:
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE), mean_moose_density = mean(MooseDensity, na.rm = TRUE))
print(sum_spe_browse)
#
#
#Question 23:
#Yes, the figure supports the researchers' hypothesis. At low moose density, browsing is concentrated on Willow and Alder, which is the preferred species.
#As the moose density increases, browsing also increases and becomes more constant throughout all species.
#
#Moose favour Willow the most, followed by Alder, as they have the highest average browsing scores.
#Moose favour Black Spruce and Black Ash the least because they have a lower browsing score.
#
#Black Ash is not shown on the figure because it probably didn't have enough data after filtering the dataset to be included in the clean version.
#
#
#Question 24:
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
#
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#
#
#Question 25:
moose_coll2 <- moose_coll %>% rename_with(~ "Ecoregion", study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
#
#
#Question 26:
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Number of Moose-Vehicle Collisions (2020)", main = "Relationship Between Moose Density and Vehicle Collisions")
#
#The scatterplot shows a positive relationship between moose density and collisions, higher density also shows higher collision rates.
#When the moose density is around 1.0, the amount of collisions is very high, meaning this one is an outlier. This is probably the area has more traffic or less woods.
#
#
#Question 27:
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop) %>% arrange(desc(coll_per_capita))
print(coll_merge_per_capita)
#
#
#Question 28:
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Moose Collisions per Person", main = "Moose Collisions per Capita vs Human Population")
#
#
#Question 29:
#The plot shows that moose collisions per person are higher when there's less human population and the collisions per person decrease when the human population increases.
#This makes sense in Newfoundland because in rural communities there's a lower human population and a larger moose population which makes the encounters on these rural roads more frequent.

