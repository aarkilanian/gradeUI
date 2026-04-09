library(dplyr)
moose_clean <- na.omit(MoosePopulation)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_sel$Year)
max(moose_sel$Estimated_Moose_Pop)
year_min <- 1904
moose_max <- 41250
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland west forest ecoregions over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moosedata2, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
sap_clean <- na.omit(SaplingStudy)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(mean_Browsingscore = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
avg_browse_reg <- arrange(sap_reg_browse, desc(mean_Browsingscore))
#Northern Peninsula Forests have the highest browsing score, Strait of Belle Isle Barrens lowest
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(mean_Height = mean(Height, na.rm = TRUE))
print(sap_reg_height)
sap_reg_height_low <- sap_reg_height %>%
  filter(mean_Height < 20)
print(sap_reg_height_low)
#Northern Peninsula and Western Forests have a mean height < 20
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(mean_Browsingscore = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
avg_browse_spe <- arrange(sap_spe_browse, desc(mean_Browsingscore))
#Black Ash has highest browsing score, Black Spruce lowest
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(mean_Browsingscore = mean(BrowsingScore, na.rm = TRUE))
barplot(fir_reg_browse$mean_Browsingscore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Browing Score", main = "Balsam Fir Browsing Score", col = "forestgreen", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(mean_Browsingscore = mean(BrowsingScore, na.rm = TRUE))
barplot(spruce_reg_browse$mean_Browsingscore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Browing Score", main = "Black Spruce Browsing Score", col = "darkblue", cex.names = 0.6)
#similar browsing in North shore, Northern Peninsula and Central. Spruce has little to no browsing in Oceanic, Maritime, and Avalon, while Balsam has at least some.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#Maritime Barrens and Strait of Belle Isle ecoregions are underepresented in the dataset, along with Black Ash in species
#It is important to recognize bias as one area may seem to have much more browsing, but in reality the species in another ecoregion are just underepresented
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(
    mean_Browsingscore = mean(BrowsingScore, na.rm = TRUE),
    mean_Moose_Density = mean(MooseDensity, na.rm = TRUE)
  )
print(sum_spe_browse)
#Question 23 a: No, the hypothesis is not supported. Though the moose browse without much preference in low densities, is high densities the browsing score is always high
#Question 23 b: The moose prefer willow saplings, and browse black spruce the least
#Question 23 c: Black as is not shown, as it is only recorded in one place one time, not gathering enough data
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Desnity",
     ylab = "# of Collisions 2020",
     main = "Moose Density vs Collisions")
#More accidents as the density increases, however, the Avalon forests have many more collisions with a smaller density, probably because of the sheer number of vehicles on the road
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop,
     xlab = "Collisions per Capita",
     ylab = "Human Population",
     main = "Human Population vs Collisions per Capita")
#This trend makes sense, as the higher the population, the more cars on the road, equals more accidents, creating more collisions per capita