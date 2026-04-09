# Title: My R script for R-assignment bio 1002
#Author: Hannah Best 
# Date 10-02-2026

#Question 1 
library(dplyr)
install.packages("dplyr")
library(dplyr)


#Question 2 used RStudio way (the "import" feature) 

#Question 3  
View(data1)
na.omit(data1)
moose_clean <- na.omit(data1)

#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5 
# a) 1904L 
year_min <- min(moose_sel$Year, na.rm = TRUE)
# b) 
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8 
# a) 
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# b)
wf <- moosedata2[moosedata2$Ecoregion == "Western_Forests", ]
plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

#Question 9 
# a)
moose_2020 <- filter(moosedata2, Year == 2020)
# b)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
# c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10 
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Question 11
# a)
saplings <- data2
# b)
sap_clean <- na.omit(saplings)

#Question 12
# a)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
# b)
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
# Highest browsing score: Northern_Peninsula_Forests and Lowest browsing score: StraitOfBelleIsleBarrens

#Question 13
# a)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>%
  print()
# b)
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions listed above have average tree heights < 20 cm (severely browsed by moose)

#Question 14
# a)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
# b)
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
# Highest browsing: first row of avg_browse_spe
# Lowest browsing: last row of avg_browse_spe

#Question 15 
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))

#Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing Intensity by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

#Question 17
# a) 
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
# b) 
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
# c) 
# Black Spruce browsing appears similar but a little lower compared to Balsam Fir across ecoregions.
# The species with the taller bars shows greater browsing pressure.

#Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#Question 20
# The dataset is not evenly distributed. North_Shore_Forests and Northern_Peninsula_Forests 
# have the most sampled saplings, while StraitOfBelleIsleBarrens has very few samples. 
# For species, Balsam_Fir and Black_Spruce are overrepresented, while Black_Ash is highly underrepresented.

# Recognizing bias is important because uneven sampling can lead to misleading conclusions 
# about moose browsing patterns. If some regions or species are sampled more often, the results 
# may not accurately represent the entire ecosystem.

#Question 21
# a)
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
# b)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgMooseDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()

#Question 23
# a)
# Moose appear to show selective browsing at low moose density, with high browsing on preferred species,
# but at higher densities browsing scores become high for most species, suggesting more generalist feeding.
# This supports the researchers’ hypothesis.
# b)
# Moose favour Willow the most (highest browsing scores) and browse Black Spruce the least overall 
# (lowest browsing scores across densities).
# c)
# Black Ash is not shown on the figure because it likely had too few observations or missing data
# after filtering/averaging, so it did not appear in the summarized dataset used for the plot.

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens",
                 "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                 "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25
# a)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
# b)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

#Question 26
# a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose/km²)",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Relationship Between Moose Density and Collisions")
# b)
# There is a general positive relationship between moose density and vehicle collisions,
# with higher-density regions (around 2–2.5 moose/km²) showing more collisions. 
# However, one clear outlier exists where collisions are extremely high (~110) at only 
# moderate moose density (~1), suggesting human population or road density may also play a major role.

#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions Per Capita vs Human Population")

#Question 29
# Collisions per capita tend to be higher in regions with smaller human populations,
# while highly populated regions show lower collisions per person. This makes sense
# because moose are more common in rural areas of Newfoundland where fewer people live,
# increasing the chance of collisions per individual driver.















