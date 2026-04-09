### R Assignment Part 1 - 1002 ###
# set Working Directory\
setwd("RAssignment")
# Question 1 install dplyr package\
install.packages("dplyr")
library(dplyr)
# Question 2 read.csv\
moosedata <- read.csv("MoosePopulation.csv")
# Question 3 removing missing data\
moose_clean <- na.omit(moosedata)
# Question 4 simplifying data\
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# Question 5 a \
select(moose_sel, Year)
year_min <- min(moose_sel$Year)
# Question 5 b\
moose_max <- max(moose_sel$Estimated_Moose_Pop)
# Question 6 moose density\
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
# Question 7 a plotting data\
plot(moosedata2$Year, moosedata2$MooseDensity,xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland Ecoregions over time")
# Question 8 western forests data\
moose_west <- filter(moosedata2, Ecoregion == "Western_Forest")
# Question 8 b\
plot(moose_west$Year, moose_west$MooseDensity,type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Western forest regions over time")
# Question 9 a ecoregion trends in recent years\
moose_2020<- filter(moosedata2, Year == "2020")
# Question 9 b highest moose density\
moose_2020_high <- filter(moose_2020, MooseDensity >2.0000000 )
# Question 9 c arranging dataset\
moose_2020_high_byd <- arrange(moose_2020_high,desc(MooseDensity))
# Question 10 connecting code lines\
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print ()
# Question 11 rename dataset\
saplings <- read.csv("SaplingStudy.csv")
# Question 11b removing missing data\
sap_clean <- na.omit(saplings)
# Question 12a moose browsing pressure\
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print ()
# Question 12b high/low moose browsing\
avg_browse_reg <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize (AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  arrange(desc(AverageBrowsing)) %>%
  print(avg_browse_reg)
# Highest Browsing 5.0 (North_Shore_Forests and Northern_Peninsula_Forests)\
# Lowest Browsing: 0.0 (Eastern_Hyper_Oceanic_Barrens and Maritime_Barrens)\
# Question 13a average tree height variations\
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(Height)) %>%
  print()
#Question 13b finding avg heights less than 20cm\
sap_reg_height_low <- # northern_peninsula_forests and western_forests have avg heights <20cm print()
# Question 14a sapling species browsing score\
  sap_spe_browse<- sap_clean %>%
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print ()
# Question 14b
avg_browse_spe <- sap_clean %>%
  group_by(Species) %>%
  summarize (AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
# black_ash has the highest browsing score and black_spruce has the lowest
#Question 15 balsam fir browsing intensity\
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()
# Question 16 balsam fir bar graph\
barplot(fir_reg_browse$MeanBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Intensity", main = "Balsam Fir Browsing Intensity Variety in Different Ecoregions", col = "forestgreen",cex.names = 0.5)
# Question 17a black spruce browsing intensity\
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()
# Question 17b dataset\
barplot(spruce_reg_browse$MeanBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Intensity", main = "Black Spruce Browsing Intensity Variety in Different Ecoregions", col = "black",cex.names = 0.5)
# Question 17c ans: The black spruce showed higher browsing intensity than the balsam fir in the Eastern and Maritime ecoregions, showing the increase in moose in the other regions.
# Question 18 tree saplings in ecoregions\
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# Question 19 tree saplings within species\
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
# Question 20a ans: I do not think it is evenly distributed, some ecoregions (such as the North Shore Forests) and species (like Balsam fir and Black Spruce) are well represented while others are underrepresented, such as the Strait of Belle Isle Barrens ecoregion and Black ash species, with only 1 sample of each, giving less to compare and contrast.
# Question 20b ans: Recognizing bias in ecological datasets is important because any uneven sampling can alter the results and create false readings while reducing how much information can be applied across regions or species.
# Question 21a\
moose_2020b <- mutate(moose_2020, MooseDensity)
# Question 21b\
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
# Question 22\
sum_spe_browse <- moose_sap %>%
  group_by(Species,Ecoregion) %>%
  summarize(mean(BrowsingScore),mean(MooseDensity)) %>%
  print()
# Question 23a ans: Yes, the figure supports the hypothesis: at low moose density, browsing is focused on a few preferred species, while higher densities show more evenly spread browsing across species.\ 
# Question 23b ans: Moose browse willow and alder the most and black spruce the least based on their relative browsing scores.\
# Question 23c ans: Black ash is not shown, likely because there were too few observations to calculate an average.\
# Question 24\
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# Question 25a\
moose_coll2 <- moose_coll %>%
  rename_with(~"Ecoregion", study_sites)
# Question 25b\
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
# Question 26a\
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions in 2020",
     main = "Moose Density vs Moose-Vehicle Collisions",
     pch = 19)
# Question 26b ans: The number of moose-vehicle increased in higher moose density conditions and vice-versa. 1.0 has an extremely high amount of collisions compared to the rest.\
# Question 27\
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
# Question 28\
plot(coll_merge_per_capita$coll_per_capita,coll_merge_per_capita$human_pop,xlab = "Collisions Per Capita", ylab = "Human Population", main = "Human Population vs Collisions Per Capita",pch = 19)
# Question 29 ans: Areas with smaller human populations tend to have higher moose–vehicle collisions per person, while more populated areas have lower rates. This makes sense in Newfoundland because moose are more common in rural regions and people there usually drive longer distances.\