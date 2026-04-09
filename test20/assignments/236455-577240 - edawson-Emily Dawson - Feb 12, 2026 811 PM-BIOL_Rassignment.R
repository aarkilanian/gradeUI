#Bio 1002 Rassignment .
# Emily Dawson 2026-02-12

#install/set-up
library(dplyr)
moosepopulation <- read.csv("C:/Users/ezdow/OneDrive/Desktop/Bio1002_Rassignment/MoosePopulation.csv.csv")
#no NA to omit in data set
#Q4
moose_sel <- select(moosepopulation, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5
year_min <- min(moosepopulation$Year, na.rm = TRUE)
min(year_min)
# [1] 1904
max(moosepopulation$Estimated_Moose_Pop, na.rm = TRUE)
# [1] 41250

#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", ylab = "moose per sq km", main = "Moose density in NL ecoregions over time")

#Q8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "year", ylab = "moose per sq km", main = "Moose density in western forests NL")

#Q9
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high_byD <- filter(moose_2020, MooseDensity > 2.0)
arrange(moose_2020_high_byD, desc(MooseDensity))

#Q10
moosefinal <- moosedata2 %>% filter(Year==2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print ()

#part two
#setup
saplings <- read.csv("C:/Users/ezdow/OneDrive/Desktop/Bio1002_Rassignment/SaplingStudy.csv.csv")
sap_clean<-na.omit(saplings)

#Q12
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
avg_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowsing)) %>% print()
#Western forest had the highest average browsing score while Strait Of Belle Isle Barrens had the lowest

#Q13
sap_red_height_low <- sap_clean %>% group_by(Ecoregion) %>% summarize(mean_height=mean(Height)) %>% print()
#the northern peninsula forest and western forest are the ecoregions wth average heights under 20cm

#Q14
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(mean(BrowsingScore)) %>% print()
#Black_Spruce has the lowest browsing score while Black_Ash has the highest

#Q15
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(MeanBrowsing = mean(BrowsingScore)) %>% print()

#Q16
barplot( fir_reg_browse$MeanBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Balsam Fir Browsing Intensity by Ecoregion", col = "forestgreen", cex.names = 0.6)

#Q17
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(MeanBrowsing = mean(BrowsingScore)) %>% print()
barplot( spruce_reg_browse$MeanBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Black spruce Browsing Intensity by Ecoregion", col = "pink", cex.names = 0.6)
#There are a couple differences between the Balsam Fir and Black Spruce, Balsam Fir has a higher numbers in the Avalon_forests, easternHyperBarrens, and MaritimeBarrens, While Black spruce has an additional ecoregion
#Both of the tree species do well in the northern peninsula forests.

#Q18
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()

#Q19
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()
#No, most species had different numbers besides adler and willow

#Q20
#I don't think the dataset is evenly distributed, some saplings such as Balsam fir are over represented while others like Black_ash are under represented.
#It is important to recognize bias in datasets because a bias can cause the results to be slightly skewed for the favored or under represented speicies. 
#Bias can lead to misleading results or even mostly inaccurate results and not reflect the true ecological process.

#Part 3

#Q21
moose_sap <- left_join(moose_2020, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Q22
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize((mean(BrowsingScore)), mean(MooseDensity)) %>% print()

#Q23
#a Using the graph, there is some support for the researchers hypothesis, browsing intensity seems more species specific at lower moose densities(strong preferences) compared to higher moose densities where everything is browsed frequently (generalist shift)
#b The moose favor the willow sapling the most while browsing the black spruce the least 
#c Black ash hasn't been shown the the figure, this is likely because it isn't shown in the sapling dataset

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25
moose_coll2 <- moose_coll %>% rename_with(~ "Ecoregion", study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")

#Q26
plot(coll_merge$MooseDensity, coll_merge$collisions2020)
#labled version
plot(coll_merge$MooseDensity, coll_merge$collisions2020)
#The trend I noticed is that generally, as moose density increases so does number of collisions
#There was an Outlier, at 1.0 density, collisions went up above 100.

#Q27
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020/human_pop)

#Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Moose Collisions per Capita", main = "Moose-Vehicle Collisions per Capita vs Human Population")

#Q29
#the general trend I noticed is that moose collisions per capita decreases with increasing human population
#This makes sense for what I know about moose and human population in NL, in NL most moose are fair away from larger human populated areas. Less moose means less collisions, matching the graph.
