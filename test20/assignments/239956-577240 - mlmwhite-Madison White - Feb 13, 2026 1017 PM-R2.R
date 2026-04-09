install.packages("tidyverse")
#grwgbr
library(dplyr)
View(MoosePopulation)
na.omit(moosedata)
moosedata <- MoosePopulation
moose_clean <- moosedata
select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
na.omit(moose_sel)
moose_sel <- na.omit(moose_sel)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

moose2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose2020, MooseDensity > "2" )
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
View(SaplingStudy)
sapling <- (SaplingStudy)
sap_clean <- na.omit(sapling)


sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

arrange(moose_2020_high, desc(MooseDensity)
avg_browse_reg <- arrange(sap_clean, desc(mean(BrowsingScore)))
#highest value are western_forest,north_shore_forest,Northern_peninsula_forest,central_forests,Western_forests
#Lowest value are EasternHyperOceanicBarrens,Maritime_Barrens
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(Height)) %>%
  print()
sap_reg_Height_low <- filter(sap_reg_height, "mean(Height)" < 20)
#Areas under 20-> Northern_Peninsula_Forests, Western_Forests
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- arrange(sap_clean, desc(mean(BrowsingScore)))



