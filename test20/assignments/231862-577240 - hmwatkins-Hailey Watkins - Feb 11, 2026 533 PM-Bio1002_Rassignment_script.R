library(dplyr)

#First step is to save the moose data
moosedata <-read.csv("MoosePopulation.csv")

#Let's get rid of the NA values from our data set
moose_clean <- na.omit(moosedata)

#We are simplifying the data set to only include certain columns
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5.a) The oldest observation is in 1904 with estimated 4 in the moose sighting in western forests.
year_min <- min(moose_sel$Year)

#Question 5.b) The greatest moose population recorded was 41250 in 2020 in central forests.
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#We are defining moosedata2 so that we use the data where the population is the most dense.  
moosedata2 <- moose_sel

moosedata2$MooseDensity <- moosedata2$Estimated_Moose_Pop / moosedata2$Area

#Here we are plotting the density of moose over years
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over years")

#We are defining moose_west so we can make a plot only including the western forests data
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#This is a linear plot to show the density for moose in western forests over the years
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western forests over years")

#We are defining the moose population data for 2020
moose_2020 <- filter(moosedata2, Year == "2020")

#This filters out data that is less than the density of 2.0 (moose/km^2)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

#This sorts the data in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Using pipes condenses this code into one without having to define multiple steps
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#This is the start of Part 2, so I imported the Sapling Study
saplings <- read.csv("SaplingStudy.csv")

#This omits data with NA in them
sap_clean <- na.omit(saplings)

#When looking at this data it shows a range from 1-4.57, with most regions within 4. 
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

#The highest is Northern_Peninsula_Forests with 4.571429
#The lowest is StraitOfBelleIsleBarrens with 1.000000

#We are using pipes to connect the sap_reg_browse to avg_browse_reg
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(MeanBrowsing))


#The average height ranges from 18.9cm to 32.4cm with most between 22.3cm and 29.9 cm
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(MeanHeight = mean(Height)) %>%
  print()

#This connects sap_reg_height with heights less than 20cm
sap_reg_height_low <- sap_reg_height %>%
  filter(MeanHeight < 20) %>%
  print()

#The ecoregions under 20cm are Northern_Peninsula_Forests with 19.9 and Western_Forests with 18.9

#The average browsing range from 2.3 from Black_Spruce to 5.0 with Black_Ash. 
#Willow and Alder are similar at 4.3 and 4.25, respectively
#Balsam_Fir and White_Birch are similar at 3.13 and 3.14, respectively. 
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

#This put the MeanBrowsing in descenind order
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(MeanBrowsing))
#The highest browsing score is Black_Ash with 5
#The lowest Browsing score is Black_Spruce with 2.33

#This filtered the species with Balsam_Fir in each region
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

#This created a bar plot
barplot(
  fir_reg_browse$MeanBrowsing,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing intensity",
  main = "Average Browsing Intensity on Balsam Fir in Ecoregions",
  col = "pink",
  cex.names = 0.6
)

#This allowed us to only see the Black_Spruce in each region
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print ()

#This is another barplot
barplot(
  spruce_reg_browse$MeanBrowsing,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Intensity",
  main = "Average Browsing Intensity on Black Spruce in Ecoregions",
  col = "purple",
  cex.names = 0.6
)
#Both Balsam Fir and Black Spruce have high browsing intensity in the Northern_Peninsula_Forests
#The Black Spruce were very low in the Maritime_Barrens but for the Balsam Fir, the browsing intensity was at 2.

#This allowed us to see how many saplings were in each region
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#Avalon_Forests, Central_Forests, EasternHyperOceanicBarrens, Western_Forests, and Long_Range_Barrens had 5
#Maritime_Barrens had 3
#North_Shore_Forests had 8
#Northern_Peninsula_Forests had 7
#StraitOfBelleIsleBarrens had 1

#This allowed us to see the number of saplings by species 
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#No the saplings per species ranged from 11 with Balsam_Fir to 1 with Black_Ash.
#Black_Spruce had 9, Alder and Willow had 8 and White_Birch had 7. 

#20a) I believe that the SaplingStudy set was very evenly represented except for StraitOfBelleIsleBarrens, which was not represented as much as the rest.
#20b) I think it is important to to recognize bias in ecological datasets because if there is over representation of one item over another it can mess with accuracy.
#20b) I also believe that every item should be represented equally so that patterns can be tracked. 

#This allowed us to filter only the year 2020 from the moose data and to add a moose density column
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

#This combined the moose data set and the sapling data set
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")

#This allowed us to calculate the average moose density and average browsing score for each region
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgMooseDensity = mean(MooseDensity),
  ) %>%
  print()

#23a) There is evidence that this hypothesis is supported. When looking at high density, you can see that the browsing score is also higher and more concentrated. 
#23a) It is also concentrated at low moose density as well, just a little more spread out across the browsing scale. 

#23b) It appears that Moose like Willow the most with the highest browsing and moose density
#23b) Black Spruce are shown the least with lower browsing rates and lower moose density.

#23c) Black Ash is not shown on the figure because it only has one data set. This would mean there is not enough data to include it on the plot.

#These are the vectors needed for 6.0.1
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

#This took the vectors and created a data set
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#This combined the data sets
moose_coll2 <- moose_coll %>%
  rename_with(~"Ecoregion", study_sites)

#This is the renaming of the data set
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")

#This is a scatterplot 
plot(
  coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of 2020 moose-vehicle collisions",
  main = "Moose Density vs. 2020 Moose-Vehicle Collisions",
  pch = 19,
  col = "blue"
)

#26b) One pattern I see is that the higher the density, the more collisions.
#26b) An outlier would be that with the moose density of 1, there is an extreme amount of collisions. 

#This allowed us to see the collisions per capita by making a new column
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
#27. The region with the highest number of accidents per person is Northern_Peninsula_Forests with 0.005 collisions per capita.
#27. They had 60 collisions with 12000 people. 

#This is another scatterplot
plot(
  coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  xlab = "Human Population",
  ylab = "Moose Accidents per capita",
  main = "Human Populations vs. Moose Accidents per capita",
  pch = 19,
  col = "magenta" 
)

#29. When looking at the graph it is easy to tell that the places with low human populations have the most accidents.
#29. This makes sense when thinking about Newfoundland because more rural areas of the province have much more forest that holds moose.
#29. The more forest leads to more moose which will lead to more accidents, but this happens in places with less people since towns with less houses have more trees. 
