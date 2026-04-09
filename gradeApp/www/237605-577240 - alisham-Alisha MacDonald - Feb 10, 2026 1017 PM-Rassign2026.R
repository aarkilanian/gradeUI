#Instal dplyer
install.packages("dplyr")

# Load libraries
library(dplyr)

# Set working directory
setwd("C:/Users/Alisha/OneDrive/Desktop/Biology/R assignment")

# Import data
Moosedata <- read.csv("MoosePopulation.csv")

#Remove missing values
Moosedata <- na.omit(Moosedata)

#Select columns of interest
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Oldest observation, 1904
min(Moosedata$Year)

#Highest population number, 41250
max(Moosedata$Estimated_Moose_Pop)

#Standardize data by creating new column 
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)

#Graph data
plot(Moosedata$Year, Moosedata$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time",
     type = "l")

#Observations for Western_Forests ecoregion
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")

#Graph MooseDataWest
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in the Western Forests Ecoregion of Newfoundland over time",
     type = "l")

#9a Filter for year 2020
MooseData_2020 <- filter(Moosedata, Year == "2020")

#9b Filter for Moose density
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)

#9c Arrange MooseDensity column
MooseData_2020_b <- arrange(MooseData_2020_b, desc(MooseDensity))

#10 Use pipes to repeat Q9
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Part II: Tree Sampling Study
#11a Import data
Treesapling <- read.csv("SaplingStudy.csv")

#11b Remove NAs
#Remove missing values
Treesapling <- na.omit(Treesapling)

#12 Mean BrowisingScore across Ecoregions
Treesapling %>%
  group_by(Ecoregion) %>% 
  summarize (mean(BrowsingScore)) %>%
  print()
#On average the Northern_Penisula has the highest Moose BrowsingScore and Strait of Belle Island has the lowest 

#13 Average tree height per Ecoregion
Treesapling %>%
group_by(Ecoregion) %>% 
  summarize (mean(Height)) %>%
  print()
#Areas with average heights less than 20cm are Wester_Forests, Northern_Penisula

#14 Average BrowsingScore across differnt species
Treesapling %>%
  group_by(Species) %>% 
  summarize (mean(BrowsingScore)) %>%
  print()
#The highest BrowsingScore is Black_Spruce
#The lowest is Black_Spruce 

#15 Filter then group and find mean 
BalsamFir <- Treesapling %>%
  filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))

#16 Barplot 
barplot(BalsamFir$`mean(BrowsingScore)`, 
        names.arg = BalsamFir$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average browsing intensity", 
        main = "Average intensity of moose browsing on balsam fir saplings based on Ecoregion", 
        col = "forestgreen", 
cex.names = 0.4) 

#17a filter, group and mean
BlackSpruce <- Treesapling %>%
  filter(Species == "Black_Spruce") %>% 
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))

#17b Barplot
barplot(BlackSpruce$`mean(BrowsingScore)`, 
        names.arg = BlackSpruce$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average browsing intensity", 
        main = "Average intensity of moose browsing on black spruce saplings based on Ecoregion", 
        col = "forestgreen", 
        cex.names = 0.5) 

#17c Black Spruce and Balsam Fir are both grazed on heavily in the North Shore, Central and Northern Peninsula forests.
#Balsam fir is moderately grazed on in the Avalon, Eastern Hyper Oceanic Barrens and Maritime Barrens while black spruce isn't.

#18 Tally Ecoregion group
EcoregionTally<- Treesapling %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#No, a varying number of saplings were counted in each Ecoregion.

#19 Tally Species group
SpeciesTally<- Treesapling %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#A varying number of saplings were counted per species

#20a Strait of Belle Isle Barrens and Maritime Barrens are underrepresented Ecoregions while North Shore and Northern Peninsula are slightly over represented.
# In terms of tree species Balsam fir is underrepresented while Black Ash is severely underrepresented.

#20b 

#Part 3
#21
moose_2020b <- Moosedata %>%
  filter(Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop/Area)

#21b
moose_sap <- left_join(moose_2020b, Treesapling, by = 'Ecoregion', relationship = "many-to-many")

#22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore),
  mean_MooseDensity  = mean(MooseDensity)) %>%
  print()

#23
#a.Yes the graph shows that has average moose density goes up do does average browsing score.
#Only willow is browsed at very low moose density's while all tree species recored
#were browsed at high moose density.

#b.Willow is the tree of choice for Moose.Alder is good too. While Black Spruce and Balsam Fir are much less popular.

#c. Black Ash isn't on the graph. I think that's because there's only 1 sample in Western Forests
# which isn't enough to determine if Moose prefer it in other areas.

#6.0.1 Moose-vehicle collisions

#24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#25a.
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
#b 
coll_merge <- left_join(MooseData_2020, moose_coll2, by = "Ecoregion")

#26
#a.
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "MooseDensity",
     ylab = "Number of Moose-Vehicles Collisons in 2020",
     mian = "Relationship Between Moose Density and Vehicle Collisons")
#b. For the most part the number of moose-vehicle collisions increases
# with moose density. Except for MooseDensity, 1.0, that density amount has an 
#extremely high outlier for number of moose-vehicle collisions being over 100.

#27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

#28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population")

#29
# There are fewer moose-vehciel collisons as human population increases. This makes
# sense because most people in Newfoudnland live in urban areas with less moose. 