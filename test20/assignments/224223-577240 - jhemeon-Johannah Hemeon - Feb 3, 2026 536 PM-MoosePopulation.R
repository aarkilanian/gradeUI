install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("C:/Users/jrhem/Downloads/MoosePopulation.csv")
View(moosedata)
na.omit(moosedata)
moose_clean <- na.omit(moosedata)
select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_sel$Year)
year_min <- min(moose_sel$Year)
max(moose_sel$Estimated_Moose_Pop)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()
