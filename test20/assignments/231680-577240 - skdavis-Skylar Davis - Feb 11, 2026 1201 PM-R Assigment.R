#BIOL 1002-Managing Biological Data Assignemnet 
# Skylar Davis 
#202317121
#Date:
#Part 1:
# Question 1: Install and Load dplyr package 
install.packages("dplyr")
library(dplyr)

#Question 2: Import Moosepopulation.csv
Moosedata <- read.csv("C:/Users/Skylar Davis/bio/MoosePopulation (1).csv")
#Question 3: View the data and remove misisng values 
View(Moosedata)
Moosedata <- na.omit(Moosedata)

#Question 4: Select only the colums of interest
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5a: What is the oldest observation in the dataset 
min(Moosedata$Year)
#Answer: The oldest year is 1904 
#Question 5b:What is the highest moose population
max(Moosedata$Estimated_Moose_Pop)
#Answer: The highest moose population is 41250
#Find which ecoregin has the maximun moose population
View(Moosedata)
#Answer:Central_Forests

#Question 6: Calculate moose density for each ecoregion 
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)
#Quesion 7: Create a line graph of moose density over time
plot(Moosedata$Year, Moosedata$MooseDensity, 
     type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8a: Filter for Western Forests
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")

#Question 8b : Line graph 
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, 
     type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests over time")
#Question 9a: High moose density
MooseData_2020 <- filter(Moosedata, Year == 2020)

#Q9b
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)

#Q9c
arrange(MooseData_2020_b, desc(MooseDensity))

#Question 10
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Part 2
#Question 11a: 
Saplings <- read.csv("C:/Users/Skylar Davis/bio/SaplingStudy.csv")
#Question 11b: 
Saplings <- na.omit(Saplings)

#Question 12:Calculate mean 
Saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore)) %>%
  print()
#Q12b: Highest is Northern_Peninsula_Forest,lowest is StraitOfBelleIsleBarrens 

#Question 13a:
Saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height)) %>%
  print()
#Question 13b:Northern_Peninsula_Forest and Western_Forests 

#Question 14:
Saplings %>%
  group_by(Species) %>%
  summarize(mean_browsing = mean(BrowsingScore)) %>%
  print()
#Question 14b: Highest is Black_Ash, lowest is Black_Spruce
#Question 15
BalsamFir <- Saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore)) %>%
  print()

#Question 16:
barplot(BalsamFir$mean_browsing, 
        names.arg = BalsamFir$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Mean Browsing Score", 
        main = "Balsam Fir browsing intensity by ecoregion", 
        col = "forestgreen",
        cex.names = 0.6)

#Question 17:
BlackSpruce <- Saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore)) %>%
  print()
#Q17b:
barplot(BlackSpruce$mean_browsing, 
        names.arg = BlackSpruce$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Mean Browsing Score", 
        main = "Black Spruce browsing intensity by ecoregion", 
        col = "darkgreen",
        cex.names = 0.6)
#Q17c:
#Balsam Fir has higher browsing scores than Black spruce in most ecoregions.
#Both species are most heavily browsed in North_Shore_Forests and Northerb_Peninsula_Forests.

#Question 18:
EcoregionTally <- Saplings %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Question 19:
SpeciesTally <- Saplings %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Question 20a:
#The dataset is not evenly distributed. Black Ash is severely underrepresented with only 1 sample,
# while Balsam Fir is overrepresented with 11 samples. This uneven sampling makes it difficult to
# fairly compare browsing patterns across all species.
#Q20b:
#Recognizing bias is important because species with fewer samples (like Black Ash) may not accurately
# represent true browsing patterns, while overrepresented species may disproportionately influence our conclusions
# about overall moose browsing preferences.

#Part 3:
#Question 21a:
MooseData_2020 <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
#Q20b:
MooseSaplingData <- left_join(MooseData_2020, Saplings, by = 'Ecoregion', relationship = "many-to-many")

#Q22:
BrowsingBySpeciesDensity <- MooseSaplingData %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore),
            mean_density = mean(MooseDensity)) %>%
  print()


#Q23: ggplot2 package had compatibility issues with R 4.5.2
# Answered questions based on direct data analysis instead
#Q23a:
#Yes, there is some evidence supporting the hypothesis. At low moose densities, browsing scores range
# widely from 0 to 4.5 (moose are highly selective), while at high densities, browsing scores are more
# clustered between 3.5-5.0 (moose browse more species, showing more generalist behavior).

#Q23b:
#Moose favor Willow the most, with consistently high browsing scores around 4.5-5.0 across all densities.
# Black Spruce is browsed the least, with many browsing scores near 0, especially at lower moose densities.

#Q23c:
#Black Ash is not shown on the figure because it only had 1 sample in the dataset,
# which is insufficient for calculating meaningful averages or creating reliable data points

#Q24:
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
StudySites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", 
                "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", 
                "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")

# Combine vectors into a data frame
MooseCollisions <- data.frame(StudySites, HumanPopulation, Collisions_2020)
#Q25a:
# First attempt
CollisionMooseData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")
# Answer: The join fails because the column names don't match. MooseData_2020 has "Ecoregion"
# while MooseCollisions has "StudySites". They need to have the same column name to join.

#Q25b:
# Rename the column so they match
MooseCollisions <- MooseCollisions %>%
  rename(Ecoregion = StudySites)

#Q25c:
CollisionMooseData <- left_join(MooseData_2020, MooseCollisions, by = "Ecoregion")

#Q26a:
plot(CollisionMooseData$MooseDensity, CollisionMooseData$Collisions_2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions in 2020",
     main = "Moose-Vehicle Collisions vs Moose Density")

#Q26b:
#There is a general positive trend showing that higher moose density tends to be associated with more collisions.
# There is a clear outlier at the top with around 110 collisions at moderate moose density (~1.0), which is much
# higher than other ecoregions with similar densities. This is likely the Avalon region with high human population.

#Q27:
CollisionMooseData <- CollisionMooseData %>%
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)

#Q28:
plot(CollisionMooseData$HumanPopulation, CollisionMooseData$CollisionsPerCapita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs Human Population")
#Q29:
#There is a strong negative trend showing that areas with smaller human populations have much higher
# collisions per capita. This makes sense because rural areas with fewer people have more moose habitat
# and less developed infrastructure, while urban areas like Avalon (270,000 people) have more controlled
# roads and wildlife management despite having the highest total collision numbers.






