#Question 1
library(dplyr)

#Question 2
moosedata <- read.csv("MoosePopulation.csv")

#Question 3
View(moosedata)
moose_clean <- na.omit(moosedata)

#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5
year_min <- min(moose_sel$Year, na.rm = TRUE)
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
  xlab = "Year",
  ylab = "Moose per sq km",
  main = "Moose density in Newfoundland ecoregions over time")
  
#Question 8 
#a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#b
plot(moosedata2$MooseDensity, moose_sel$Ecoregion,
     type = "l",
     xlab = "Density",
     ylab = "Moose per sq km",
     main = "Density in the Western Forest Regions")
#Question 9 
#a
moose_2020 <- moosedata2 %>%
  filter(Year == 2020)
plot(moosedata2$Year, moose_sel$Ecoregion,
     type = "l",
     xlab = "Density",
     ylab = "Year",
     main = "Density Trends for 2020")
#b
moose_2020_high <- moose_2020 %>%
  filter(MooseDensity > 2.0)
plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Density",
     main = "Density Trends for 2020")
#c
arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print(moosefinal)
