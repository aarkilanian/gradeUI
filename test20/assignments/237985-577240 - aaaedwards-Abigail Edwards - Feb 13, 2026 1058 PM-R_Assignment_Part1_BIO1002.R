###QUESTION 1 
install.packages("dplyr")
library(dplyr)

###QUESTION 2
getwd()
setwd("C:/Users/gaile/Downloads")
moosedata <- read.csv("MoosePopulation.csv")

###QUESTION 3
moose_clean <- na.omit(moosedata)

###QUESTION 4 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

###QUESTION 5
years <- select(moose_clean, Year)
year_min <- min(years)
year_min
###1904

###b)
estimated_moose_pop <- select(moose_clean, Estimated_Moose_Pop)
estimated_moose_pop_max <- max(estimated_moose_pop)
estimated_moose_pop_max
###41250

###QUESTION 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

###QUESTION 7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose Per Sq/Km", main = "Moose Density in Newfoundland Ecoregions Over Time")

###QUESTION 8
###a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

###b)
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose Per Sq/Km", main = "Moose Density in Newfoundland Ecoregions Over Time")

###QUESTION 9
###a)
moose_2020 <- filter(moosedata2, Year == "2020")

###b)
moose_2020_high <- filter(moose_2020, MooseDensity >2.0)

###c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

###QUESTION 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()