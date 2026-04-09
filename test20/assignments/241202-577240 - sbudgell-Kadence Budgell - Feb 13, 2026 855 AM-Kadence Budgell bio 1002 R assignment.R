install.packages(“dplyr”)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion,   Year,   Area,  Estimated_Moose_Pop)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
year_min <- min(moose_sel$Year)
year_min
moosePopulation <-mutate(moose_sel,MooseDensity = Estimated_Moose_Pop / Area)
plot(moodePopulatio$Year,moosePopulation$MooseDensity, type = "l",   xlab = "Year",  ylab = "Moose per sq km",    main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(MoosePopulation, Ecoregion == "Western_Forests")
plot( moose_west$Year,  moose_west$MooseDensity,  type = "l",
      xlab = "Year",  ylab = "Moose density (moose per km²)",
      main = "Moose density over time in Western Forests")
moose_2020 <- filter(MoosePopulation, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- MoosePopulation %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingsStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(     AverageBrowsing = mean(BrowsingScore)  )
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_reg
# The ecoregion with the highest average browsing is at the top of the table, and the lowest average browsing is at the bottom.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(  AverageHeight = mean(Height)  )
print(sap_reg_height)
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)
print(sap_reg_height_low)
# Ecoregions with average tree heights less than 20 cm are considered severely browsed by moose.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize( AverageBrowsing = mean(BrowsingScore)  )
print(sap_spe_browse)
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_spe
# The species with the highest average browsing score is at the top of the table, and the species with the lowest average browsing score is at the bottom.
fir_reg_browse <- san_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(mean_browse = mean(BrowsingScore, na.rm = TRUE))
barplot(
  fir_reg_browse$mean_browse,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Intensity",
  main = "Average Moose Browsing on Balsam Fir by Ecoregion",
  col = "pink",
  cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(mean_browse = mean(BrowsingScore, na.rm = TRUE))
spruce_reg_browse
barplot(
  spruce_reg_browse$mean_browse,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Intensity",
  main = "Average Moose Browsing on Black Spruce by Ecoregion",
  col = "purple ",
  cex.names = 0.6)
# Overall, Black Spruce shows lower average browsing intensity than Balsam Fir across most ecoregions,
# suggesting moose preferentially browse Balsam Fir when it is available.
sap_reg_tally <- san_clean %>%
  group_by(Ecoregion) %>%
  tally()
sap_reg_tally
# The number of tree saplings counted was not the same across ecoregions, as some ecoregions had more sampled trees than others.
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#The SaplingStudy dataset is not evenly distributed. Some ecoregions and tree species are overrepresented in the dataset, while others have much lower sample sizes,  which suggests uneven sampling effort.
moosePopulation2020 <- MoosePopulation %>%
  filter(Year == 2020)
moose_sap <- left_join(
  moosePopulation2020,
  sap_clean,
  by = "Ecoregion",
  relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE),
    mean_MooseDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()
#Yes, the figure supports the hypothesis. At low moose density, browsing intensity is higher on preferred species, while at higher moose density browsing intensity increases across more species, suggesting a shift toward more generalist browsing.
#Moose appear to favour Willow the most, as it shows the highest average browsing scores across densities, while Black Spruce is browsed the least, with consistently low browsing intensity.
#Black Ash is not shown in the figure, likely because it was absent or too rare in the sampled ecoregions to calculate reliable average browsing scores.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c(
  "North_Shore_Forests",
  "Northern_Peninsula_Forests",
  "Long_Range_Barrens",
  "Western_NL_Forests",
  "Central_NL_Forests",
  "Avalon_Forests",
  "South_Coast_Forests",
  "Boreal_Shield",
  "Tundra")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_moose <- left_join(MoosePopulation2020, moose_coll2, by = "Ecoregion")
plot(
  moose_coll$MooseDensity,
  moose_coll$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of Moose-Vehicle Collisions",
  main = "Moose Density vs Moose-Vehicle Collisions")
#There is a positive relationship between moose density and the number of moose-vehicle collisions, with regions of higher moose density generally experiencing more collisions. One or two regions appear as potential outliers, likely reflecting differences in road density or human activity.
moose_coll_per_capita <- moose_coll %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
moose_coll_per_capita <- moose_coll %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(
  moose_coll_per_capita$human_pop,
  moose_coll_per_capita$coll_per_capita,
  xlab = "Human Population",
  ylab = "Moose Collisions Per Capita",
  main = "Moose Collisions Per Capita vs Human Population")
#Moose collisions per capita tend to be higher in regions with lower human populations, likely because rural areas have high moose densities but fewer people. This pattern makes sense given Newfoundland’s large moose population and extensive road networks in sparsely populated regions.
