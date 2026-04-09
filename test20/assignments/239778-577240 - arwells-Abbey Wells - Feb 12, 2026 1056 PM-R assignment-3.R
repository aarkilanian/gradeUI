#Title: R assignment
#Author: Abbey Wells
#Date: February 12th, 2026

#Question 1
library(dplyr)

#Question 2
moosedata <- read.csv("MoosePopulation.csv")

#Question 3
moose_clean <- na.omit(moosedata)

#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5
min()
year_min <-(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
xlab = "Year",
ylab = "Moose per sq km",
main = "Moose density in Newfoundland ecoregions over time")

#Question 8
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Western Forests over time")

#Question 9
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10
moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()

#Question 11
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

#Question 12
sap_reg_browse <- sap_clean %>%
group_by(Ecoregion) %>%
summarise(AverageBrowsing = mean(BrowsingScore))
print(sap_reg_browse)
# The ecoregion with the highest average moose browsing is Northern_Peninsula_Forests
#and lowest is StraitOfBelleIsleBarrens

avg_browse_reg <- sap_reg_browse %>%
arrange(desc(AverageBrowsing))
print(avg_browse_reg)

#Question 13
names(sap_clean)
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(AverageHeight = mean(Height))
print(sap_reg_height)

sap_reg_height_low <- sap_reg_height %>%
filter(AverageHeight < 20)
print(sap_reg_height_low)

#Question 14
sap_spe_browse <- sap_clean %>%
group_by(Species) %>%
summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_spe_browse)
# Black_Ash has highest browsing score while Black_Spruce has the lowest.

avg_browse_spe <- sap_spe_browse %>%
arrange(desc(AverageBrowsing))
print(avg_browse_spe)

#Question 15
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarise(AverageBrowsing = mean(BrowsingScore))
print(fir_reg_browse)

#Question 16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average browsing score", main = "Balsam Fir browsing intensity by ecoregion", col = "pink", cex.names = 0.6) 

#Question 17
spruce_reg_browse <- sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarise(AverageBrowsing = mean(BrowsingScore))
print(spruce_reg_browse)

barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, main = "Black Spruce browsing by ecoregion", xlab = "Ecoregion", ylab = "Average browsing score", cex.names = 0.6)

#Black Spruce browsing varies by ecoregion and is different compared to Balsam Fir.

#Question 18
sap_reg_tally <- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>%
print()

#Question 19
sap_spe_tally <- sap_clean %>%
group_by(Species) %>%
tally()
print(sap_spe_tally)

#Question 20
# The SaplingStudy dataset is not evenly distributed. Some ecoregions and species have
# more saplings counted than others, which means certain areas or species are
# overrepresented while others are underrepresented.

# Recgonizing bias is important because uneven sampling can affect  the results 
# make them less accurate

#Question 21
moose_2020b <- moose_clean %>%
filter(Year == 2020) %>%
mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Question 22
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarise(AvgBrowsing = mean(BrowsingScore), AvgDensity = mean(MooseDensity))
print(sum_spe_browse)

#Question 23
#a. Yes the figure supports the hypothesis. At low density browsing scores are more different
#between species, but at higher density the scores are more similar.

#b. Moose favour Alder and Willow since they consistantly have higher browsing scores.
#black spruce is browsed the least overall.

#c. Black Ash is not shown on the figure because there was likely not enough
#data for it in the datasets to calculate averages

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25
moose_coll2 <- moose_coll %>%
rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

#Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", 
ylab = "Moose-Vehicle Collisions", main = "Moose Density vs Moose-Vehicle Collisions")
# As moose density increases the moose-vehicle collisions also increase. There is one
#outlier point that is way higher than the rest.

#Question 27
coll_merge_per_capita <- coll_merge %>%
mutate(coll_merge_per_capita = collisions2020 / human_pop)
#Northern Peninsula Forests and North Shore Forests have the highest moose-vehicle collisions per person

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Moose Collisions per Person",  main = "Collisions per capita vs Human Population")

#Question 29
#Areas with lower human populations tend to have higher moose collisions per person.
# This makes sense because moose are more common in rural areas with less people.