library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean,Ecoregion,Year,Area,Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
#1904
year_max <- max(moose_sel$Estimated_Moose_Pop)
#41250
moosedata2 <- mutate(moose_sel,MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose Density over time in Western Forests",
     type = "l")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byp <- arrange(moose_2020_high, desc(MooseDensity))
moosefinaL = moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()