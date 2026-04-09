install.packages("dplyr")
library(dplyr)
moose<-read.csv("MoosePopulation (1).csv")
sapling<-read.csv("SaplingStudy.csv")
colnames(moose)
colnames(sapling)
str(moose)
str(sapling)
#question 1
library(dplyr)
#question 2
library(dplyr)
moosedata<-read.csv("MoosePopulation (1).csv")
#question 3
library(dplyr)
View(moosedata)
moose_clean<-na.omit(moosedata)
#question 4
library(dyplr)
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5
#a)
library(dyplr)
year_min<-min(moose_sel$Year)
year_min
#b)
library(dyplr)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
moose_max
#question 6
library(dplyr)
moosedata<-read.csv("MoosePopulation (1).csv")
moose_clean<- na.omit(moosedata)
moose_sel<- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moosedata2<- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop/Area)
head(moosedata2)
#question 7
library(dplyr)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type= "1"
     xlab= "Year"
     ylab= "Moose per sq km"
     main= "Moose density in Newfoundland"
#question 8
moose_west<-filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type= "1",
     xlab= "Year",
     ylab= "Moose per sq km",
     main= "Moose density in western forests"
#question 9
#a)
moose_2020<-filter(moosedata2, Year==2020)
#b)
moose_2020_high<-filter(moose_2020, MooseDensity>2.0)
#c)
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
#question 10
  moosefinal<-moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

  