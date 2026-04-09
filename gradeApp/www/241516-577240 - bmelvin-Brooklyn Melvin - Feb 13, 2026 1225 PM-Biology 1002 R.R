#Biology 1002 R Assisngment
#Brooklyn Melvin
#February 13 2026
install.packages("dplyr")
#1
library("dplyr")
#2
moosedata <- read.csv("MoosePopulation.csv")
#3
moose_clean <- na.omit(moosedata)
#4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#5.a)
year_min <- min(moose_sel$Year)
#5.b)
year_max <- max(moose_sel$Year)
#6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
#8.a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#8.b)
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time", type = "l")
#9.a)
moose_2020 <- moosedata2 %>% filter(Year == 2020)
#9.b)
moose_2020_high <- moose_2020 %>% filter(MooseDensity > 2.0)
#9.c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#10
moosefinal <- moosedata2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()
#11
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
#12.a)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarise(AverageBrowsing = mean(BrowsingScore)) %>% print()
#12.b)
avg_browse_reg <-sap_reg_browse %>% arrange(AverageBrowsing)
#13.a)
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarise(mean(Height)) %>% print()
#13.b)
sap_reg_height_low <- sap_reg_height %>% filter(`mean(Height)` < 20) %>% print()
#The ecoregions that have heights less than 20cm are Northern_Peninsula_Forest and Western_Forests.
#14.a)
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarise(AverageBrowsing = mean(BrowsingScore)) %>% print()
#14.b)
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# The species with the highest browsing score is Black_Ash, and the lowest is Black_Spruce
#15
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarise(AverageBrowsing = mean(BrowsingScore)) %>% print()
#16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Balsam Fir Browsing Intensity by Ecoregion", col="forestgreen", cex.names=0.6)
#17.a)
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarise(AverageBrowsing = mean(BrowsingScore)) %>% print()
#17.b)
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "AverageBrowsing", main = "Average Black Spruce Browsing Intensity by Ecoregion", col = "blue", cex.names = 0.6)
#17.c) The Balsam Fir exhibits a consistently stronger browsing pressure, while Black Spruce has more moderate browsing.
#18
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()
#19
sap_spe_tally <- sap_clean %>% group_by(Species) %>%tally() %>% print()
#20.a) I think the Sapling Study is almost evenly distributed, with the exception being Black Ash ecoregion that only has one tally to represent it. 
#20.b) Recognizing bias in ecological data sets is important because it is cruial in avoiding skewed results. Failing to adress bias can lead to incorrect results and flawed scientific conclusions.  
#21.a)
moose_2020b <- moosedata2 %>% filter(Year == 2020) %>% mutate(MooseDensity)
#21.b)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
#22
sum_spe_browse <- moose_sap %>% group_by(Species) %>% group_by(Ecoregion) %>%summarise(AverageBrowsing = mean(MooseDensity)) %>% print()
#23.a) Yes, the figure supports the hypothesis. At low moose densities, browsing scores vary strongly by species,showing selectivity, while at higher densities the scores are more similar and high, suggesting more generalized browsing. 
#23.b) Moose appear to favor the Willow the most with the highest browsing scores. They favor Black_Spruce the least, with the lowest browsing scores. 
#23.c) The species not shown is Black_Ash because it only had one recorded browsing data.  
#24
collisions2020 <- c(56,60,14,36,48,10,40,110,6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#25.a)
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
#25.b)
coll_merge <- moose_coll2 %>% left_join(moose_2020, by = "Ecoregion")
#26.a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "MooseDensity", ylab = "collisions2020", main = "Moose Density rate relation to number of moose-vechicle collisions")
#26.b) 
#There is a strong trend between moose density and the number of moose-vehicle collisions, with collisions increasing as moose density increases. One point at very high moose density and collision numbers is an outlier compared to the rest of the regions.
#27
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)
#28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions Per Capita", main = "Moose Collisions Per Capita vs Human Population")
#29
#Regions with lower human populations tend to have higher moose collisions per capita, most likely because in rural areas with small populations, humans and moose share the roads more frequently.This makes sense because there is a large moose population in comparison to the rural areas with a small population in Newfoundland, and the roads are heavily surrounded by woodland. 

