# Title: Biology 1002 R assignment
# Programmer: Darron Myers
# Date: 2/12/2026

# Set working directory
setwd ("C:/Users/megam/Desktop/Biology/Biology1002/R2Assignment")
getwd()
list.files()

#Question 1 

install.packages("dplyr") #install dplyr
library(dplyr)

Moosedata <- read.csv("MoosePopulation.csv") #load data set

#Question 3

View(Moosedata)  #view data entry

MooseData <- na.omit(Moosedata) #remove NA values

#Question 4

Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop) #simplify dataset

#Question 5 What is the oldest data entry?

min_year <- min(Moosedata$Year) #minimum year = 1904

max_Estimated_Moose_Pop <- max(Moosedata$Estimated_Moose_Pop, na.rm = TRUE) #maximum estimated moose population = 41250

#Question 6

Moosedata <- mutate (Moosedata, MooseDensity = Estimated_Moose_Pop/Area) #add MooseDensity column 

#Question 7

plot (Moosedata$Year, Moosedata$MooseDensity,                           #plot new graph  
      xlab = 'year',                                                     #Label
      ylab = "Moose per sq km",
      main = "Moose density in Newfoundland ecoreigons over time")       #Figure title  

#Question 8

#a
MooseDataWest <- filter (Moosedata, Ecoregion == "Western_Forests")  #Filter desired data for viewing 

#b
plot(MooseDataWest$Year, MooseDataWest$MooseDensity,  type = "l",               #Use plot function to make line graph
xlab = "Years of recording", ylab = "Moose Density per decade",           #axis labels and title
main = "Moose Density over Time in Western Forests")

#Question 9

#a
MooseData_2020 <- filter(Moosedata, Year == 2020)                       #filter results from 2020

#b

MooseData_2020 <- MooseData_2020 %>% filter (MooseDensity > 2.0)   #filter out non conformed units

MooseData_2020 <- filter(MooseData_2020, MooseDensity > 2.0)       #filter density of greater than 2

#c
MooseData_2020 <- arrange(MooseData_2020, desc (MooseDensity))      #arrange density column in descending order

#Question 10

MooseData_final <- Moosedata %>%            #perform operations using pipe operators
  filter (Year == 2020) %>% 
  filter (MooseDensity > 2.0) %>%
  arrange (desc(MooseDensity)) %>%
  print()

#R assignment part 2

#Question 11
library(dplyr)                            #load package
Saplings <- read.csv ("SaplingStudy.csv") #load data
Saplings_clean <- na.omit(Saplings)       #remove na values

#Question12

Browsing_by_Ecoregion <- Saplings_clean %>%  #mean browsing score by ecoreigon
  group_by (Ecoregion) %>%
  summarize (meanBrowsing = mean(BrowsingScore))  
list.files()            
print (Browsing_by_Ecoregion)  #print result

#comment : 
# The Northern Peninsula Forests has the most moose browsing at 4.571429
# The Strait of Belle Isle Barrens has the Least moose browsing at 1.00

#Question 13
Height_by_Ecoregion <- Saplings_clean %>%
  group_by(Ecoregion) %>%
  summarize(meanHeight = mean(Height))

print(Height_by_Ecoregion) #print results

SeverlyBrowsed <- Height_by_Ecoregion %>%
  filter(meanHeight < 20)

print (SeverlyBrowsed)

#comment:
#Eco regions with average tree heights less than 20 cm (severely browsed) are The Northern Peninsula Forests
#and the Western Forests

#Question14
Browsing_by_Species <- Saplings_clean %>%
  group_by(Species) %>%
  summarize(meanBrowsing = mean(BrowsingScore))

print(Browsing_by_Species)
#comment:
#The species with the highest mean browsing is Black Ash at 5
# The species with the lowest mean browsing is Black Spruce at 2.33

#Question15
BalsamFir <- Saplings_clean %>%                 #filter Balsam Fir and mean browsing by region
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(meanBrowsing = mean(BrowsingScore))
print(BalsamFir)                                #print result


#Question16

barplot(BalsamFir$meanBrowsing,                 #barplot for Balsam Fir intensity
        names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Balsam Fir by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

#Question17
BlackSpruce <- Saplings_clean %>%             #filter Black Spruce and calculate mean browsing by Ecoregion  
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(meanBrowsing = mean(BrowsingScore))
print(BlackSpruce)                            #print results
barplot(BlackSpruce$meanBrowsing,             #plot data
        names.arg = BlackSpruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Black Spruce Ecoregion",
        col = "darkolivegreen",
        cex.names = 0.6)
#Comment:
#Overall, Black Spruce appears to have higher/lower browsing than Balsam Fir
#depending on the Eco region, but patterns differ across regions.

#Question 18
EcoregionTally <- Saplings_clean %>%   #count sapling per Ecoregion
  group_by(Ecoregion) %>%
  tally()
print(EcoregionTally)                   #print data

#Comment:
#The number of saplings counted is not the same across all regions

#Question 19
SpeciesTally <- Saplings_clean %>%     #count number of saplings per species
  group_by(Species) %>%
  tally()

print(SpeciesTally)
#Comment:
#The number of saplings counted differs among species

#Question 20
#Comment:
#a -The sapling study data set does not appear evenly distributed.
#Some Eco regions and species are over represented, while others have fewer.
#b - Recognizing bias is important because uneven sampling can lead to misleading 
#conclusions about browsing patterns and ecosystem processes.

#Rass Part 3
#Question 21
library(dplyr)                                  #load library
setwd("C:/Users/megam/Desktop/Biology/Biology1002/R2Assignment")
MooseData_2020 <- Moosedata %>%            #filter 2020 moose data
  filter(Year == 2020) 


MooseSaplingData <- left_join(MooseData_2020,              #Join moose and sapling data from 2020
                              Saplings_clean,
                              by = "Ecoregion",
                              relationship = "many-to-many")
print(MooseSaplingData)                                     #print data

#Question22 
BrowsingBySpeciesDensity <- MooseSaplingData %>%           #average browsing score and density by species and region
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  )
print(BrowsingBySpeciesDensity)                         #print Data

#Question 23
#Installed and viewed code from Biology department before removing for fear of copyright issues in the future
#Comment a - There is some evidence supporting the hypothesis.
#At low moose density, browsing appears to be more selective,
#while at higher densities browsing becomes more uniform.
#Comment b - Moose likely favour the most palatable species, such as Balsam Fir
#and appear to browse species like Black Spruce the least.
#Comment c - The species not shown is one with missing data or a low sample size,
#which prevents it from appearing clearly in the plot.

#Question24

Collisions_2020 <- c(56,60,14,36,48,10,40,110,6)                  #create vectors
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
StudySites <- c("North_Shore_Forests",
                "Northern_Peninsula_Forests",
                "Long_Range_Barrens",
                "Central_Forests",
                "Western_Forests",
                "EasternHyperOceanicBarrens",
                "Maritime_Barrens",
                "Avalon_forests",
                "StraitOfBelleIsleBarrens")
MooseCOllisions <- data.frame(StudySites,                         #Moose collisions
                              HumanPopulation,
                              Collisions_2020)
print(MooseCOllisions)                                            #Print data

#Question25
#a
MooseCollisionData <- left_join(MooseData_2020,             #try to join data sets
                                MooseCOllisions,            #this won't work when the column names are different
                                by = "Ecoregion")
#b
MooseCOllisions <- MooseCOllisions %>%                    #rename columns to match
  rename(Ecoregion = StudySites)



#c
MooseCollisionData <- left_join(MooseData_2020,         #join with moose data from 2020
                                MooseCOllisions,
                                by = "Ecoregion")
print(MooseCollisionData)                                      #print data

#Comment:
#The original join fails because of different colummn names
#renaming the colmun allows them to be join correctly

#Question26
plot(MooseCollisionData$MooseDensity,            #scatterplot of moose density and collisions
     MooseCollisionData$Collisions_2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions",
     main = "Moose Density vs Moose-Vehicle Collisions")
#Comment:
#There appears to be a positive relationship between moose density and 
#collisions, though some reigions may be outliers.

#Question 27

MooseCollisionData_per_capita <- MooseCollisionData %>%                  #collisions per capita data
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)

print(MooseCollisionData)                                                #print data

#Question 28

plot(MooseCollisionData$HumanPopulation,                             #scatterplot of collisions per capita vs human pop
     MooseCollisionData$CollisionsPerCapita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Moose Collisions Per Cpaita vs Human Population")

#Question 29

#Comment:
#Eco regions with lower human populations often have higher 
#collisions per capita.  This makes sense because moose density
#can remain high when fewer people are present.



