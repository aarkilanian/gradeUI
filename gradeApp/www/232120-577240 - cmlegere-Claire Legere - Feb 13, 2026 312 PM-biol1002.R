library("dplyr")
data <- read.csv("MoosePopulation.csv")
data <- read.csv("/Users/clairelegere/Downloads/MoosePopulation.csv")
moose_clean <- na.omit(data)
moose_clean <- data
moose_sel <- select(data, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
year_min <- min(moose_sel$Year)
year_min
#1904
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
moose_max
#41250
moosedata2 <- mutate(moose_sel,MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)
moose_year_avg <- moosedata2 %>%
  group_by(Year) %>%
  summarise(mean_density = mean(MooseDensity, na.rm = TRUE))
plot(moose_year_avg$Year, moose_year_avg$mean_density,
     type = "l",
     xlab = "Year",
     ylab = "Average Moose per sq km",
     main = "Average Moose Density Over Time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density Over Time in Western Forests")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("/Users/clairelegere/Downloads/SaplingStudy.csv")
sap_clean <- na.omit(saplings)
summary(sap_clean)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
sap_reg_browse
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_reg
#Highest browsing region is Northern Peninsula Forests (4.57)
#Lowest browsing region is Strait of Belle Isle Barrens (1)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
#Ecoregions with an average sapling height below 20cm: Northern Peninsula
#Forests (19.9) Western Forests (18.9)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_spe
#Black Ash has highest browsing (5), Black Spruce has lowest browsing (2.33)
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "darkblue",
        cex.names = 0.6)
# Black Spruce generally experiences higher browsing compared to Balsam Fir
#across most ecoregions.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#a) The dataset is not very evenly distributed. Some ecoregions, such as the North
#Shore Forests (8) and the Northern Peninsula Forests (7) are overrepresented,
#whilst the Strait of Belle Isle Barrens (1) is underrepresented. This same
#isue appears amongst species, as Balsam Firs (11) are overrepresented, and
#Black Ash (1) is underrepresented.
#b) Recognizing bias in ecological datasets is important because uneven sampling 
#can alter analyses and result in innaccurate conclusions about species
#abundance or habitat characteristics.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Density)
head(moose_clean)
moose_2020b <- moosedata2 %>%
  filter(Year == 2020)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship =
                         "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    Avg_BrowsingScore = mean(BrowsingScore, na.rm = TRUE),
    Avg_MooseDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()
# a) The hypothesis is supported by the wide spread in browsing scores at low moose 
#densities,suggesting selective feeding, whereas at higher densities,
#most species show high browsing scores, demonstrating generalist behavior.
#b) Moose favor Willow and Alder the most, as these have the highest browsing
#scores across all density levels. They browse Black Spruce the least, as it
#consistently shows the lowest browsing scores.
#c) Black Ash is not visible on the figure. This is likely due its data points
#being overlapped by another species with the same browsing scores, or having no
#recorded observations for Black Ash in this specific area. 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests",
                 "Northern_Peninsula_Forests",
                 "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens",
                 "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion", relationship
                        = "many-to-many")
head(coll_merge)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (per sq km)",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs. Vehicle Collisions",
     pch = 19, col = "darkgreen")
#ecoregions with more moose do tend to have more collions, however there is an
#outlier where populations with a density of 1.0 (out of a 0-3.0 scale) 
#had over double the amount of collisions as areas with a 3.0 density.
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita)) %>%
  select(Ecoregion, coll_per_capita)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population",
     pch = 19, col = "darkred")
#The general trend here is that more moose collions occur in areas with a lower 
#human population. This makes sense because areas with high human populations 
#are more urban, and have less wildlife, so most collisions would be with objects
#or other humans rather than moose, whereas more rural areas would have more moose
# near the roads, resulting in collisions. 
