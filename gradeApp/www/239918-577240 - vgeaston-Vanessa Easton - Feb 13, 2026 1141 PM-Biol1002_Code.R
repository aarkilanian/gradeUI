# Q1: download dyplyr
install.packages("dplyr")
library(dplyr)

#Q2: download moosedata
moosedata <- read.csv("MoosePopulation.csv")

#Q3: clean up data, remove N/A
moose_clean <- na.omit(moosedata)

#Q4: select columns of interest
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5 a: find oldest data
year_min <- min(moose_sel$Year)

#b: largest moose population
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#Q6: calculate moose density
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Q7: create plot
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Q8 a: filter to only show observations from the western forest ecoregion
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#b: line graph for moose density
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland 
     western forest ecoregions over time")
       
#Q9 : all ecoregions trends for recent years
#a: for 2020
moose_2020 <- filter(moosedata2, Year == "2020")

#b: 2020 high density
moose_2020_high <- filter(moosedata2, MooseDensity > 2)

#c: Assort MooseDensity in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Q10: pipe, connect lines of code
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Q11 a: Load sapling data
saplings <- read.csv("SaplingStudy.csv")

#b: remove N/A
sap_clean <- na.omit(saplings)

#Q12 a: group the data

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#b: organize by browsing score
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
# Northern Peninsula Forests had the highest average browsing score,
#and Strait of Belle Isle Barrons had the lowest


#Q13 a: average tree height
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageTreeHeight = mean(Height)) %>%
  print()

#b: ecoregions below 20cm
sap_reg_height_low <- filter(sap_reg_height, AverageTreeHeight < 20) %>%
  print()
# Northern Peninsula and Western forests have average tree heights below 20cm, 
# meaning they are considered to be severely browsed by moose.

#Q14 a: how average browsing score varies across different sapling species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsingScore = mean(BrowsingScore)) %>%
  print()

#b: species with highest and lowest browsing scores
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsingScore))

#Q15: Balsam Fir browsing intensity
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(FirAvgBrowsing = mean(BrowsingScore))

#Q16: Balsam Fir bar plot
barplot(fir_reg_browse$FirAvgBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", 
        ylab = "Average Browsing score", main = "Average Balsam Fir Browsing Score by Ecoregion", 
        col = "forestgreen", cex.names = 0.6) 

#Q17 a: black spruce browsing score
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(SpruceAvgBrowsing = mean(BrowsingScore))

#b: Black Spruce bar plot
barplot(spruce_reg_browse$SpruceAvgBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", 
        ylab = "Average Browsing score", main = "Average Black Spruce Browsing Score by Ecoregion", 
        col = "forestgreen", cex.names = 0.6) 

#c: Black Spruce browsing compared to Balsam Fir
# Black Spruce has lower average browsing intensity than Balsam Fir across most ecoregions, 
# excluding the Long Range Barrens and Western Forests.

#Q18: number of saplings in each ecoregion
sap_reg_tally<- sap_clean %>%
group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Q19: number of saplings per species
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Q20 
# a: I don't believe the sapling dataset is evenly distributed, as some ecoregions and sapling species
#were overrepresented, while others were underrepresented. by ecoregion, the North Shore forests 
# have eight saplings recorded, while the Strait of Belle Isle only has one. Part of this large gap in the data 
# is due to the Strait of Belle isle Barrens having so few trees, but also because the Black Spruce and 
# Balsam Fir are overrepresented in the North Shore data. The overrepresentation of some species is also visible 
# whene you view the data from the sap_spe_tally dataset. Notably, the Balsam fir is recoreded 11 times,
# and the Black Ash just once.

# b: Recognizing bias in ecological datasets is important because uneven 
# representation can lead to misleading conclusions and affect how well results 
# can be generalized.

#Q21 a: filter moose_clean for 2020
moose_2020b<- filter(moose_clean, Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

#b: Join the datasets
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Q22: Average browsing score and moose density for each species in each ecoregion
sum_spe_browse<- moose_sap %>% 
  group_by(Ecoregion, Species) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore), AverageDensity = mean(MooseDensity)) %>%
  print()

#Q23: ggplot questions
#a: At low moose density, Alder and Willow have higher browsing scores that the other tree species, 
# and with an increased moose density, all species have a high browsing score. This evidence supports 
# the researchers hypothesis that browsing is more selective at a lower moose density.

#b: Based on the ggplot provided, moose favour Alder and willow the most, and Black Spruce the least. 
# This suggests that moose find Alder and Willow to be the most palatable.

#c: Black Ash is not depicted on the plot because the database only has one recording of the species,
# and it overlaps with the willow at the same browsing score of 5, and density of 2.5.

#24: add vector values, create dataset
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25 a: rename study_sites
moose_coll2<- moose_coll %>%
  rename(Ecoregion = study_sites)

#b: join databases
coll_merge<- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")

#Q26 a: moose density relation to moose-vehicle collisions
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density", 
     ylab = "Moose-Vehicle Collisions", 
     main = "The Relation Between Moose Density 
     and Moose-Vehicle Collisions")

#b: The plot shows that for the most part, and increase in moose density means an increase in collisions. 
# An outlier shown on the plot is plotpoint for the Avalon forests, indicating 110 collisions 
# at a moose density of 0.96.

#Q27: collision per capita
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

#Q28: plot of coll_per_capita versus human_pop
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Population", 
     ylab = "Collisions per Capita", 
     main = "Collisions per Capita 
     in relation to population")

#Q29: This scatterplot shows that the collisions per capita are higher for low population areas. 
# this makes sense because the number of collisions has a greater affect per capita on a smaller population.



