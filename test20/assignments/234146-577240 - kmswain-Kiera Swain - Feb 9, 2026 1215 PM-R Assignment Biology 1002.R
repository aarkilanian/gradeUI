# R Assignment
# Kiera Swain- 202413983

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

# Question 5 a oldest observation\
year_min <- min(moose_sel$Year)

# Question 5 b highest Estimated_Moose_Pop\
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Question 6 moose density\
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7 changes in moose density over year\
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8 a western forests data\
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8 b moose density change\
plot(moose_west$Year, moose_west$MooseDensity,type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forest Regions over time")

# Question 9 a ecoregion trends in recent years\
moose_2020 <- filter(moosedata2, Year == "2020")

# Question 9 b moose density greater than 2.0\
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Question 9 c ranging data set\
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10 connecting code lines\
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

### R Assignment Part 2 ###

# Question 11 rename data set\
saplings <- read.csv2("SaplingStudy.csv")

# Question 11 b removing missing data\
sap_clean <- na.omit(saplings)

# Question 12 a moose browsing pressure\
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

# Question 12 b highest and lowest moose browsing\
avg_browse_reg <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE) %>%
  arrange(desc(AverageBrowsing))
  print(avg_browse_reg)
  
# highest BrowsingScore: 5.0 (North_Shore_Forests and Northern_Peninsula_Forests)\
# lowest BrowsingScore: 0.0 (EasternHyperOceanicBarrens and Maritime_Barrens)\
  
# Question 13 a avg tree height across ecoregions\
  library (dplyr)
  sap_reg_height <- sap_clean %>%
    group_by(Ecoregion) %>%
    summarize(mean_height = mean(Height,na.rm = TRUE) %>%
    print()
    
# Question 13 b finding avg heights <20 cm\
  # Northern_Peninsula_Forests and Western_Forests\
sap_reg_height_low <- sap_reg_height %>%
  filter(mean(Height) < 20) %>%
print(sap_reg_height_low)

# Question 14 a sapling species browsing score\
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

# Question 14 b highest and lowest browsing\
avg_browse_spe <- sap_clean %>%
group_by(Species) %>%
  summarize (AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
# Highest AverageBrowsing Black_Ash (5)\
# Lowest AverageBrowsing Black_Spruce (2.33)\

# Question 15 balsam fir browsing intensity\
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 16 BalsamFir barplot\
barplot(fir_reg_browse$MeanBrowsing, names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregions", 
        ylab = "Average Browsing Intensity", main = "Average Moose Browsing Intensity on Balsam Fir by Ecoregion", 
        col = "forestgreen", cex.names = 0.5) 

# Question 17 a black spruce browsing intensity\
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

# Question 17 b bar graph\
barplot(spruce_reg_browse$MeanBrowsing, names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregions", 
        ylab = "Average Browsing Intensity", main = "Average Moose Browsing Intensity on Black Spruce by Ecoregion", 
        col = "forestgreen", cex.names = 0.5) 

# Question 17 c black spruce vs balsam fir\
# Balsam Fir is overall browsed more than Black Spruce.\
# This indicates that Balsam Fir is more preffered by moose than Black Spruce in most Ecoregions.\

# Question 18 counted tree saplings\
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

# Question 19 tree saplings within speices\
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

# Question 20 a\
# The SaplingStudy is not evenly distributed.
# North_Shore_Forests are well represented for ecoregions while Balsam Fir and Black Spruce are well represented by species.\
# Strait of Belle Isle Barrens is under represented by ecoregions while Black Ash is under represented for species.\

# Question 20 b\
# It's important to recognize bias in ecological datasets because uneven sampling can skew results.\
# Heavily sampled regions will dominate trends whle rarely sampled regions will give unreliable answers.\

### R Assignment Part 3 ###

# Question 21 a\
moose_2020b <- mutate(moose_2020, MooseDensity)

# Question 21 b\
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

# Question 22 avg browsing score and density for each ecoregion\
sum_spe_browse <- moose_sap %>%
  group_by(Species,Ecoregion) %>%
  summarize(mean(BrowsingScore),mean(MooseDensity)) %>%
  print()

# Question 23 a\
# Yes. Low moose density leads to high browsing on a few preferred species and higher moose density means more species show moderate to high browsing.\

# Question 23 b\
# Most favoured would be Willow as it has highest browsing scores across densities, the least favoured would be Black Spruce because of it’s very low browsing at low density.\

# Question 23 c\
# Black ash is the species not shown.\
# Likely a low amount of browsing or insufficient data for averages.\

# Question 24\
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25 a joining datasets\
moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", study_sites)

# Question 25 b joining datasets\
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")

# Question 26 a moose density vs moose-vehicle collisions\
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions",
     pch = 19)

# Question 26 b\
# Higher moose density leads to more moose-vehicle collisions, 1.0 has very high collisions compared to other sites.\
# Low densities have fewer collisions.\

# Question 27 ecoregions with the highest number of moose collisions per person\
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

# Question 28 scatterplot of coll_per_capita vs human_pop\
plot(coll_merge_per_capita$coll_per_capita,
     coll_merge_per_capita$human_pop,
     xlab = "Human Population",
     ylab = "Moose Collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population",
     pch = 19)

# Question 29\
# As human population increases, moose collisions per capita decrease, very small populations have  higher per-capita collision rates.\
# This ties into Newfoundland, since rural areas have a lot of moose and long driving distances on highways, whereas urban areas have less moose encounters and slower traffic.\
