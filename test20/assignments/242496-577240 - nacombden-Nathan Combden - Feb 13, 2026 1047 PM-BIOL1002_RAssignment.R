#Q1
#install and import dplyr
install.packages("dplyr")
library(dplyr)

#Q2
#import moose dataset
moosedata <- read.csv("MoosePopulation.csv")

#Q3
#view dataset
View(moosedata)

#omit empty data and view again
moose_clean <- na.omit(moosedata)
View(moose_clean)

#Q4
#select only columns of importance
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)

#Q5
#a) find oldest observation and return value
year_min <- min(moose_sel$Year)
year_min

#b) find highest estimated population and return value
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max

#Q6
#calculate moose density for each region and view new dataset
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)

#Q7
#plot new dataset
plot(moosedata2$Year,moosedata2$MooseDensity, 
	xlab = "year", 
	ylab = "Moose per sq km", 
	main = "Moose density in newfoundland ecoregions over time")

#Q8
#a) create new dataset for western forests ecoregion
moose_west <- filter(moosedata2, Ecoregion== "Western_Forests")

#b) plot line graph
plot(moose_west$Year, moose_west$MooseDensity,
	type = "l", 
	xlab = "year", 
	ylab = "Moose per sq km", 
	main = "Moose density in western forests ecoregion over time")
#Q9
#a) filter only 2020 data
moose_2020 <- filter(moosedata2, Year == 2020)

#b) filter above 2.0 density
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

#c) arrange in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#View new dataset
View(moose_2020_high_byD)

#Q10
#repeat using pipes 
moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()

#------------------------Part 2------------------------

#Q11
#load and clean sapling dataset
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
View(sap_clean)

#Q12
#a) find the mean browsing pressure per region
sap_reg_browse <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()

#b) sort results by descending browsing score
avg_browse_reg <- sap_reg_browse %>%
arrange(desc(AverageBrowsing)) %>%
print()
#the Northern peninsula forests had the highest browsing score, 
#Strait of belle isle barrens had the lowest

#Q13
#a) find mean tree height for each region and print
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(AverageHeight = mean(Height)) %>%
print()

#b) filter regions with average height less than 20cm
sap_reg_height_low <- sap_reg_height %>%
filter(AverageHeight < 20) %>%
print()
#the regions with avg height less than 20cm are 
#northern peninsula forests and western forests

#Q14
#a) group data by species and find mean browsing score for each group
sap_spe_browse <- sap_clean %>%
group_by(Species) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()

#b) rearrange data by decreasing browsing score
avg_browse_spe <- sap_spe_browse %>%
arrange(desc(AverageBrowsing)) %>%
print()
#the species with the highest browsing score is black ash
#the lowest is black spruce

#Q15
#find browsing intensity by ecoregion for balsam fir
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()

#Q16
#make bar graph for balsam fir browsing intensity
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion,
	xlab = "Ecoregion",
	ylab = "Average browsing intensity",
	main = "Average Balsam Fir browsing by ecoregion",
	col = "forestgreen",
	cex.names = 0.6)

#Q17
#a) find browsing intensity by ecoregion for black spruce
spruce_reg_browse <- sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()

#b) make bar graph for black fir browsing intensity
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion,
	xlab = "Ecoregion",
	ylab = "Average browsing intensity",
	main = "Average Black Spruce browsing by ecoregion",
	col = "forestgreen",
	cex.names = 0.6)
#c) The black spruce has much higher browsing intensity in the long range barrens
#and western forests. The balsam fir has much higher intensity in eastern hyper oceanic barrens,
#avalon forests and maritime barrens.

#Q18
#find number of trees in each ecoregion
sap_reg_tally<- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>% 
print()

#Q19
#find number of trees counted for each species
sap_spe_tally <- sap_clean %>%
group_by(Species) %>%
tally() %>%
print()

#Q20
#a) I dont think the sapling study dataset accurately reflects the system
#I'm trying to understand, while most of the trees are fairly evenly
#counted, but the black ash was severely undercounted and the balsam fir
#was slightly overcounted.
#b) It is important to recognize bias because uneven datasets can lead to skewed
#results and patterns. This can lead to false conclusions about ecosystem processes.

#------------------------Part 3------------------------

#Q21
#a) filter 2020 moose data and create density column
moose_2020b <- moose_clean %>%
filter(Year == 2020) %>%
mutate(MooseDensity = Estimated_Moose_Pop / Area)
View(moose_2020b)

#b) join moose_2020b data to sap_clean dataaet
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
View(moose_sap)

#Q22
#calculate average browsing score and moose density for each species and ecoregion
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(
Mean_BrowsingScore = mean(BrowsingScore),
Mean_MooseDensity = mean(MooseDensity)) %>%
print()

#Q23
#a) The figure shows evidence that supports the researchers hypothesis.
#The moose show higher preference at lower densities, with some plants being
#chosen more than others, and much lower preference at high density
#with all plants being selected almost the same amount.

#b) The moose favour willow and alder saplings the most,with those species
#being chosen most at high and low densities. They favour black spruce the least,
#with it being chosen the least at high and low density.

#c) Black ash is not shown in the figure, this is because black ash saplings were
#severely underrepresented with only 1 sapling counted.

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(
	collisions2020, 
	human_pop, 
	study_sites)
#Q25
#a) rename ecoregion to study_sites
moose_coll2 <- moose_coll %>%
rename(Ecoregion = study_sites)
#b) join into new dataset
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion", relationship = "many-to-many")

#Q26
#a) create scatterplot of moose density and collisions
plot(
  coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of collisions",
  main = "Moose density to moose vehicle collisions")

#b)I see a trend of the number of collisions increasing with the moose density.
#An outlier is at 1.0 moose density where there are over 100 collisions, despite
#having relatively low density.

#Q27
#calculate collisions per capita for each ecoregion
coll_merge_per_capita <- coll_merge %>%
mutate(coll_per_capita = collisions2020 / human_pop)
View(coll_merge_per_capita)

#Q28
#create scatterplot of collisions per capita vs human population
plot(
  coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  xlab = "Human population",
  ylab = "Moose collisions per capita",
  main = "Human population versus moose collisions per capita")

#Q29
#I see the trend of moose collisions per capita decreasing as human population
#increases. This trend makes sense because in wooded areas with a lower human
#population there are higher populations of moose that are more likely to
#wander onto the road due to less traffic scaring them away.
#An outlier is the last point past the 250000 human population mark where
#there is a very small number of collisions after no collisions from 100000
#human population to 250000 human population.























