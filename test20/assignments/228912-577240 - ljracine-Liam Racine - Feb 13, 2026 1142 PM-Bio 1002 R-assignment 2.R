# Title: bio 1002 R assignment 1
# Author: Liam Racine
# Date: 01-28-2026

#install dplyer
install.packages(dplyer)

#Set working directory 
setwd("C:/Users/sawsh/OneDrive/Documents/Bio/Bio 1002 R-assignment 1/")

#Question 1 
library("dplyr")
#answer
#Attaching package: ‘dplyr’
#
#The following objects are masked from ‘package:stats’:
#  
#  filter, lag
#
#The following objects are masked from ‘package:base’:
#  
#  intersect, setdiff, setequal,
#union
#Warning message:
#  package ‘dplyr’ was built under R version 4.3.3 

#Question 2
moosedata <- read.csv("MoosePopulation.csv")
#answer results in importated moosedata of 54 obs and 6 variables

#question 3
View(moosedata)
#brings up the moosedata as a table witha  lot of empty spaces
moose_clean <- na.omit(moosedata)
#creats a new data sit of 37obs of 6 variables where has the NA data removed

#question 4 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#creates a new data set of 37 obs of 4 variables using the 4 selcted tables of Ecoregion, Year, Area, and Estimated_Moose_Pop

#Question 5 
#a 
year_min <- min(moose_sel$Year)
#runs a min fuction on the moose_sel data set and returns the year of 1904 (saved as year_min) which is the oldest year of the data
#b 
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#adds a moose_max varrabile that stores the results of the Max fuction ran on the moose_sel data set. the returned value is 41250 moose saved as moose_max

#Question 6 
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#creats a new data table with a colum for the population Density of the Moose population be deviding the estimated population by the Area and saving this to the new data set.

#Question 7 
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab =  "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
#produces a plot graph of the Moose population density in newfoundland over time

#question 8
#a 
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# creates a filtered down data set of just the Western_Forests region which contains 6 obs. store it as moose_west
#b 
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose density in Western Forests Ecoregion of Newfoundland")
# creates a line graph of showing the change in population density within the Western Forests Ecoregion overtime

#Question 9 
#a
moose_2020 <- filter(moosedata2, Year == "2020")
#creates a new data set from moosedata2 which just holds the data from the year 2020 and stores it as moose_2020
#b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#creates a new data set filtered from moose_2020 that has all the data samples where the population density was over 2.0 and saves it as moose_2020_high 
#c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#creates a new data table from moose_2020_high which has the data samples shorted in decsending order and saves it as moose_2020_high_byD

#Question 10
moosefinal <- moosedata2 %>% 
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#combines the code from question 9 to produce a final moose data set in one simple opperation and out prints the results. see the output below
#Ecoregion Year  Area
#1 Northern_Peninsula_Forests 2020 12000
#2        North_Shore_Forests 2020  8400
#3            Western_Forests 2020  6800
#4            Central_Forests 2020 20500
#Estimated_Moose_Pop MooseDensity
#1               32000     2.666667
#2               21740     2.588095
#3               17000     2.500000
#4               41250     2.012195


#Question 11 
#a
saplines <- read.csv("SaplingStudy.csv")
#imports the SaplingStudy dataset
#b
sap_clean <- na.omit(saplines)
#gets rid of the N/A data within the saplines data set and saves as sap_clean

#Question 12
#a
sap_reg_browse <- group_by(sap_clean, Ecoregion) %>% summarize(BrowsingScore = mean(BrowsingScore))
print(sap_reg_browse)
#b
avg_browse_reg <- arrange(sap_reg_browse, desc(BrowsingScore)) 
#Higest amount was Northern_Peninsula_Forests Lowest was Strait Of Belle Isle Barrens

#Question 13
#a
sap_reg_height <- group_by(sap_clean, Ecoregion) %>% summarize(Height = mean(Height)) 
print(sap_reg_height)
#b
sap_reg_height_low <- filter(sap_reg_height, Height < 20)
#the regions with a height lower than 20cm is Western Forests and Northern Peninsula Forests
print(sap_reg_height_low)

#Question 14 
#a
sap_spe_browse <- group_by(sap_clean, Species) %>% summarize(BrowsingScore = mean(BrowsingScore))
print(sap_spe_browse)
#b
avg_browse_spe <- arrange(sap_spe_browse, desc(BrowsingScore))
#The species with the highest browsing score is Black Ash and the lowest is Black Spruce

#Question 15
fir_reg_browse <- filter(sap_clean, Species != "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(BrowsingScore = mean(BrowsingScore))

#Quesetion 16
barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "browsing intensity", main = "browsing intensity across different ecoregions of Newfoundland of Balsam Fir", col = "forestgreen", cex.names = 0.6)

#Question 17
#a
spruce_reg_browse <- filter(sap_clean, Species != "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(BrowsingScore = mean(BrowsingScore))
#b
barplot(spruce_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "browsing intensity", main = "browsing intensity across different ecoregions of Newfoundland of Black Spurce", col = "skyblue", cex.names = 0.6)
#c 
# the Black Spruce and the Balsam Fir compare fairly similarly across ecoregions with the exception of around the Martime_Barrens where the spruce is browsed a lot more than the fir

# Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#the same number of saplings where not counted 

#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species)%>% 
  tally() %>% 
  print()
#They where not

#Question 20
#a
#I think the sapling study is unevenly distributed as the Balsam Fir is over represented and the Black Ash is under
#b
#it is important to recognize any bias as it may cause results or conclusions that are reflected only within the data and sample set not the world or ecosystem 

#Question 21
#a
moose_2020b <- filter(moose_clean, Year == "2020") %>% mutate(moose_2020b, MooseDensity = Estimated_Moose_Pop / Area )
#b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Question 22

sum_spe_browse <- group_by(moose_sap, Ecoregion, Species) %>% summarize(BrowsingScore = mean(BrowsingScore), .groups = 'drop') 
print(sum_spe_browse)

#question 23
#a the mose do not appear to show any strong prefernces at low density 
#b Willow is the perfered species by moose having only one data point that is not 4 or 5 on the browsing score
#c black ash is not on the figure most likely because they only have one sample in the dataset

#question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#question 25
#a 
moose_coll2 <- moose_coll %>% rename_with(Ecoregion = study_sites)
  