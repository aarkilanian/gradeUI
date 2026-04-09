# Name: Alex Fagan
# Biology 1002 - R Assignment
# Date: February 13th, 2026

install.packages("dplyr")
#Q1
library(dplyr)
library(readr)
#Q2
MoosePopulation <- read.csv("MoosePopulation.csv")
View(MoosePopulation)
moosedata <- read.csv("MoosePopulation.csv")
#Q3
moose_clean <- na.omit(moosedata)
na.omit(moosedata)
View(moose_clean)
moose_clean <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_clean)
View(moose_clean)
#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
min(moose_sel$Year)
1904
#The oldest observation was made in 1904
max(moose_sel$Estimated_Moose_Pop)
41250
#The maximum estimated moose population is 41250
#Q5
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)
#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
#Q8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", type = "l", main = "Moose density in Newfoundland ecoregions over time")
#Q9
moose_2020 <- filter(moosedata2, Year == "2020")
View(moose_2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
View(moose_2020_high)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
View(moose_2020_high_byD)
#Q10
moosefinal <- moose_2020 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% 
print()
SaplingStudy <- read.csv("SaplingStudy.csv")
View(SaplingStudy)
#Q11
saplings <- SaplingStudy
View(saplings)
sap_clean <- na.omit(saplings)
View(sap_clean)
View(saplings)
View(SaplingStudy)
#Q12
avg_browse_reg <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
View(avg_browse_reg)
#The highest browsing scores were in the Northern Peninsula Forests; however, the lowwest were in the Maritime Barrens.
#Q13
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height))
View(sap_reg_height)
#The Northern Peninsula Forests and Western Forests have an average height less than 20cm.
sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20)
View(sap_reg_height_low)
print(sap_reg_height_low)
print(sap_reg_height_low)
View(moose_clean)
View(sap_reg_height_low)
View(sap_clean)
#Q14
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore))
View(sap_spe_browse)
print(sap_spe_browse)
avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing))
View(avg_browse_spe)
#The Black Ash had the highest average browsing whereas the Black Spruce had the lowest average browsing.
#Q15
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
View(fir_reg_browse)
#Q16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Balsam Fir Browsing Intensity by Ecoregion", col = "red", cex.names = 0.6)
#Q17
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
View(spruce_reg_browse)
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Black Spruce Browsing Intensity by Ecoregion", col = "aquamarine", cex.names = 0.6)
#Black Spruce browsing is generally lower/higher than Balsam Fir depending on the ecoregion. In most regions, Balsam Fir tends to show a stronger browsing intensity, indicating it is a preferred food source.
#Q18
#Q19
sap_spe_tally<- sap_clean %>%
  + group_by(Ecoregion) %>%
  + tally() %>% 
  + print()
#Q20
#The SaplingStudy dataset is not evenly distributed. Some ecoregions and species have many more saplings sampled than others. This means that certain regions or species are overrepresented while others are underrepresented.
# Recognizing sampling bias is important because uneven sampling can distort ecological patterns and lead to wrong conclusions about browsing intensity or species vulnerability. If some regions or species are oversampled, they could seem more heavily browsed simply because more data were collected there.
#Q21
moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
View(moose_2020b)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
View(moose_sap)
#Q22
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(MeanBrowsing = mean(BrowsingScore), MeanMooseDensity = mean(MooseDensity,))
View(sum_spe_browse)
#Q23
#Yes, this supports the hypothesis. At a higher density, the moose eat more generally whereas at a lower density, the moose are more selective.
#The moose prefer the willow sapling and browse the black spruce the least.
#The black ash wasn't pictured because there wasn't enough sampled in order to include it in the data.
#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Q25
moose_coll2 <- moose_coll %>% rename_with(~ "Ecoregion", .cols = study_sites)
#Q26
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion", relationship = "many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density (moose/km²)", ylab = "Moose-Vehicle Collisions (2020)", main = "Relationship Between Moose Density and Collisions", pch = 19, col = "darkgreen")
#I can see that as moose density increases, moose vehicle collisions increase as well. There is an outlier however where the moose density is 1/km^2.
#Q27
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)
View(coll_merge_per_capita)
#Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions per Capita", main = "Scatterplot of Collisions per Capita vs Human Population", pch = 19, col = "purple")
#Q29
#Collisions per capita are highest in areas with small human populations and decline as population increases. That pattern fits Newfoundland, where moose are far more common in rural regions than in larger towns or cities.