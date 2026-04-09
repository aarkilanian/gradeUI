#Q1
install.packages("dplyr")
library(dplyr)
#Q2
moosedata <- MoosePopulation
#Q3
moose_clean <- na.omit(moosedata)
View(moose_clean)
#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel
#Q5
year_min <- min(moose_sel$Year)
year_min
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "moose density in Newfoundland ecoregions over time")
#Q8
moose_west<- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)
plot(moose_west$Year, moose_west$Moose_Density, type = "l", xlab = "Year", ylab = "Moose Density",main = "Change in Moose Density Over Time (Western Forests Region)")
library(dplyr)   
#Q9
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Q10
moosefinal <- moosedata2 %>% 
filter(Year == 2020) %>%    
filter(MooseDensity > 2.0) %>%   
arrange(desc(MooseDensity)) %>% 
print()  
#Q11
Saplings <- `SaplingStudy.(1)`
View(Saplings)
Saplings <- na.omit(Saplings)
Saplings_clean <- na.omit(Saplings)
#Q12
sap_reg_browse<- Saplings_clean %>%
group_by(Ecoregion) %>%
summarize(mean(BrowsingScore))
print(sap_reg_browse)
avg_browse_reg <- sap_reg_browse
avg_browse_reg <- arrange(sap_reg_browse, desc(`mean(BrowsingScore)`))
print(avg_browse_reg)
#The region with the highest browsing score is the Northern Peninsula Forests.
#The region with the lowest browsing score is the Straight Of Belle Isle Barrens.  
#Q13
sap_reg_height <- Saplings_clean %>%
group_by(Ecoregion) %>%
summarize(mean_height = mean(Height))         
print(sap_reg_height)
sap_reg_height_low <- sap_reg_height %>%
filter(mean_height < 20) 
print(sap_reg_height_low)
#The regions with heights less than 20 cm is the Northern Peninsula Forests with 19.9 cm and Western Forests with 18.9 cm. 
#Q14
sap_spe_browse <- Saplings_clean %>%
group_by(Species) %>%
summarize(mean_BrowsingScore = mean(BrowsingScore))
print(sap_spe_browse)
avg_browse_spe <- sap_spe_browse
avg_browse_spe <- arrange(sap_spe_browse, desc(mean_BrowsingScore))
print(sap_spe_browse)
#The species with the highest browsing score is the black ash.
#The species with the lowest browsing score is the black spruce.
#Q15
fir_reg_browse<- Saplings_clean %>%
group_by(Ecoregion) %>%
summarize(mean(BrowsingScore))
print(fir_reg_browse)
#Q16
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Balsam Fir Browsing Intensity by Ecoregion", col="Pink", cex.names = 0.6)
#Q17
spruce_reg_browse <- Saplings_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore))
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score",main = "Average Browsing Intensity on Black Spruce by Ecoregion",col = "blue",cex.names = 0.6) 
#Black spruce shows lower browsing intensity than balsam fir across ecosystems. 
#Q18
sap_reg_tally<- Saplings_clean %>%
group_by(Ecoregion) %>%
tally() %>% 
print()
#Q19
sap_spe_tally <- Saplings_clean %>%
group_by(Species) %>%
tally() %>%
print()
#Q20
#A region that seems to be represented well is the Northern Peninsula Forests whereas Straight of Belle Isle Barrens seem to be less represented. 
#In terms of specific species, balsam fir seems to be represented well but white birch is mentioned less.
#Its important to recognize bias in ecological data sets because it can cause inaccurate conclusions or patterns for species and regions.
#Q21
moose_2020b <- moose_clean %>%
filter(Year == 2020) %>%
mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, Saplings_clean, by = 'Ecoregion', relationship = "many-to-many")
#Q22
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(AvgBrowsing = mean(BrowsingScore), AvgDensity = mean(MooseDensity))
#Q23
library(ggplot2)
ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
geom_point(size = 3) +
theme_minimal() +
labs(title = "Browsing Intensity Across Moose Density by Species",
x = "Average Moose Density",
y = "Average Browsing Score")  
#a) The evidence supporting the researchers hypotheses is that at lower moose density the browsing score varies. At high densities, most species have higher browsing scores. 
#b) Moose appear to favor Willow and Alder the most and browse black spruce the least.
#c) Black Ash is not shown in the figure, likely because there was missing or inconclusive data for this species. 
#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Q25
moose_coll2 <- moose_coll %>%
rename(Ecoregion = study_sites)
coll_merge <- moose_2020 %>%
left_join(moose_coll2, by = "Ecoregion")
#Q26
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Number of Collisions (2020)", main = "Moose Density vs Moose-Vehicle Collisions")
# The trends I see from the scatter plot include less collisions where moose density is lower and very high number of collisions where moose density is moderate, suggesting possible outliers.
#Q27
coll_merge_per_capita <- coll_merge %>%
mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita %>%
arrange(desc(coll_per_capita)) %>%
print()
#Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Moose Collisions per Capita", main = "Collisions per Capita vs Human Population")
#Q29
#The trends I observe from the graph include a decrease in collisions per capita as human population increases, this may suggest that areas with lower populations are more affected by moose collisions. This makes sense with what I know about Newfoundland because high moose density areas are usually in rural areas that don't have high populations. There are more roads through moose habitat therefore increase the chance of a collision. It isn't often to see a moose in the city. 

