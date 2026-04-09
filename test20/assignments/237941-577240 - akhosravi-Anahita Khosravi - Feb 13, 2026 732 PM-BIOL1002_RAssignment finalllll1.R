setwd("~/BIOL1002_RAssignment")
getwd()
list.files()
moose<-read.csv("MoosePopulation.csv")
head(moose)
str(moose)
dim(moose)
install.packages("dplyr")
# Q1=
library(dplyr)
# Q2:
# Import MoosePopulation dataset
moosedata <- read.csv("MoosePopulation.csv")
# Q3:
# Remove rows with missing values
moose_clean <- na.omit(moosedata)
# Q4:
# Select columns of interest
moose_sel <- select(moose_clean,Ecoregion,Year,Area,Estimated_Moose_Pop)
# Q5:
#Oldest year: 1904
year_min <- min(moose_sel$Year)
#Highest population: 41250
moose_max <- max(moose_sel$Estimated_Moose_Pop)
# Q6:
moosedata2 <- mutate( moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
# Q7:
plot(
  moosedata2$Year,
  moosedata2$MooseDensity,
  type = "l",
  xlab = "Year",
  ylab = "Moose per sq km",
  main = "Moose density in Newfoundland ecoregions over time"
)
# Line graph indicates changes in moose density over the time
# Q8:
# Filter data for Western Forests
moose_west <- filter(
  moosedata2,
  Ecoregion == "Western_Forests"
)
plot(
  moose_west$Year,
  moose_west$MooseDensity,
  type = "l",
  xlab = "Year",
  ylab = "Moose per sq km",
  main = "Moose density in Western Forests over time"
)
# Q9:
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
# Q10:
moosefinal<- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
# Q11:
# Load SaplingStudy dataset
saplings <- read.csv("SaplingStudy.csv")
# Remove the rows with missing values
sap_clean <- na.omit(saplings)
# Q12:
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
# Ecoregion with the highest average browsing is Northern_Peninsula_Forests 4.57
# Ecoregion with the lowest average browsing is StraitOfBelleIsleBarrens 1   
# Q13:
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions with average sapling heights less than 20 cm are Northern_Peninsula_Forests and Western_Forests
# Q14:
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# Species with highest average browsing is Black_Ash and Species with lowest average browsing is Black_Spruce
# Q15:
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Q16:
barplot(
  fir_reg_browse$AverageBrowsing,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average browsing intensity",
  main = "Balsam Fir browsing by ecoregion",
  col = "forestgreen",
  cex.names = 0.6
)
# Q17:
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(
  spruce_reg_browse$AverageBrowsing,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average browsing intensity",
  main = "Black Spruce browsing by ecoregion",
  col = "darkgreen",
  cex.names = 0.6
)
# At all, Black Spruce indicates lower average browsing intensity than Balsam Fir across most ecoregions, while both species experience higher browsing in North_Shore_Forests and Northern_Peninsula_Forests.
# Q18:
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
# Q19:
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# Q20:
# a) The SaplingStudy data isn't evenly distributed. Some ecoregions and species have more samples than others.
# b) It's important to know about bias because uneven data can give wrong results about moose browsing patterns.
# Q21:
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(
  moose_2020b,
  sap_clean,
  by = "Ecoregion",
  relationship = "many-to-many"
)
# Q22:
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
# Q23:
# a) It seems at lower moose density, browsing looks more selective. With increasing density, browsing seems more general across species.
# b) Moose seems to prefer Balsam_Fir and Black_Ash: Black_Spruce is grazed less compared to others.
# c) Black_Ash is not clearly shown because it has very few samples in the dataset.
# Q24:
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c(
  "North_Shore_Forests",
  "Northern_Peninsula_Forests",
  "Long_Range_Barrens",
  "Central_Forests",
  "Western_Forests",
  "EasternHyperOceanicBarrens",
  "Maritime_Barrens",
  "Avalon_Forests",
  "StraitOfBelleIsleBarrens"
)
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# Q25:
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020b,moose_coll2,by = "Ecoregion")
# Q26:
plot(
  coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose density",
  ylab = "Number of collisions",
  main = "Moose density vs vehicle collisions"
)
# It's a general trend where higher moose density is linked to more collisions.
# Some ecoregions appear as outliers.
# Q27:
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
# Q28:
plot(
  coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  xlab = "Human population",
  ylab = "Collisions per capita",
  main = "Collisions per capita vs human population"
)
# Q29:
# Areas with smaller human populations sometimes have higher collisions per capita. It may because moose are more common in rural areas.

