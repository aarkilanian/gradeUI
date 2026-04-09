##Jozie Edwards 
#Question 1 
install.packages("dplyr")
library(dplyr)
#Question 2 
moosedata <- read.csv("MoosePopulation.csv")
#Question 3 
View(moosedata)
moose_clean <- na.omit(moosedata)
View(moose_clean)
#Question 4 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5 
#(a)
year_min <- min(moose_sel$Year)
year_min
#year min is 1904L
#(b)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
#year max 41250L
#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7 
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Question 8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year,
     moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests ecoregion over time")
#Question 9 
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Question 10 
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Question 11
saplings<-read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
View(sap_clean)
#Question 12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
##Northern_Peninsula has the highest browsing scores.
##StraitOfBelleisle has the lowest browsing scores.
#Question 13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Northern_Peninsula and western_Forests have average tree heights below 20 cm
#Question 14 
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
##Black_Ash has the highest browsing score.
##Black_Spruce has the lowest browsing score.
#Question 15 
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#Question 16 
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Balsam Fir by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
#Question 17 
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Black Spruce by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
##Black Spruce browisng varies strongly by ecoregion, with the highest # average browsing observed in North_Shore_Forests and Northern_Peninusla, and a very low browsing in EasternHyperOceanic and Maritime_Barrens.
#Question18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#Question 20 
# The SaplingStudy dataset is not evenly distributed, as some ecoregions and tree species have more sampled saplings than others, indicating potential overrepresentation and underrepresentation.
# Recognizing sampling bias is important because uneven sampling can skew results and lead to incorrect conclusions about moose browsing patterns across regions or species.
#Question 21 
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
View(moose_sap)
#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity  = mean(MooseDensity)
  ) %>%
  print()
#Question 23 
# (a) There is some support for the hypothesis: at low moose density the browsing scores vary more by species (more selective),
# while at higher densities most species cluster at high browsing scores (more uniform/generalist browsing).
# (b) # Moose appear to browse Willow the most overall (consistently highest average browsing scores),
# and they browse Balsam_Fir / Black_Spruce the least at low moose densities (lowest average browsing scores on the plot).
# (C) # Any species not shown is missing because it did not appear in the joined summary table (no matched observations after joining/filtering/NA removal),
# so ggplot had no data points to plot for that species.
#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Question 25 

moose_coll2 <- moose_coll %>%
  rename_with(~"Ecoregion", study_sites)
coll_merge <- left_join(moose_2020,moose_coll2,
  by = "Ecoregion"
)
#Trying again 
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
#Question 26
plot(
  coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of Moose-Vehicle Collisions",
  main = "Moose Density vs Moose-Vehicle Collisions"
)
# There appears to be a positive relationship where regions with higher moose density tend to have more moose-vehicle collisions.
# One possible outlier is Avalon Forests, which has very high collisions compared to its moose density, likely due to high human population and traffic.

#Question 27 
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
#Question 28 
plot(
  coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  xlab = "Human Population",
  ylab = "Collisions Per Capita",
  main = "Moose Collisions Per Capita vs Human Population"
)
#Question 29 
# Collisions per capita tend to be higher in regions with smaller human populations, while more populated regions show lower per-capita collision rates.
# This makes sense because rural areas have more moose habitat and driving exposure per person compared to urban regions.
