# Title: My R script for moose lab
# Author: Tessa Morton
# Date: 26-04-2020

#1
# Load libraries needed ---------------------------
library(dplyr)

# Set working directory ---------------------------
setwd("C:/Users/temor/Desktop/BIOL1002_RAssignment")

#2
# Load data ---------------------------
moosedata <- read.csv(file = "MoosePopulation.csv")
View(moosedata)
#3
na.omit(18)
na.omit(33)
na.omit(36)
na.omit(39)
na.omit(42)
na.omit(43)
na.omit(44)
na.omit(45)
na.omit(46)
na.omit(47)
na.omit(48)
na.omit(49)
na.omit(51)
na.omit(52)
na.omit(53)
na.omit(54)

moose_clean <- na.omit(moosedata)
#4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
#5
# Analyse data ---------------------------
year_min <- min(moose_sel$Year)
#1904
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#41250
#6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#7
# Plot data ---------------------------
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year,
     moose_west$MooseDensity, 
     type = "1",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#9
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity>2)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()