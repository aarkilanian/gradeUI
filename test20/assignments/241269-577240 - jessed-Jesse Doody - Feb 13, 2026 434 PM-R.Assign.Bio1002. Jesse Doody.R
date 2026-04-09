
library(dplyr)
moose_clean <- na.omit(MoosePopulation)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density Over Time in Western Forests")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


sap_clean <- na.omit(SaplingStudy)



sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Balsam Fir by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

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
        col = "darkgreen",
        cex.names = 0.6)

sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#Question 20a) I don't believe this data set is evenly distributed as some 
#species are represented more than others
#b) Recognizing a bias within data sets is important as misrepresented or
#over represented information can lead to a skewing of fact leading to improper
# interpretations of results.

moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()

#Q23 a) There is evidence supporting this hypothesis. From this graph we can
# see at lower densities browsing intensity becomes more selective across species
# at higher densities we see similar results across species. 
# b) Moose favor species with the highest browsing score which from the plot seems 
# to be the balsam fir and black spruce the least. 
# c) Mountain Ash is not shown in the figure due to not having matching data after 
# the sets were merged.

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests",
                 "Western_Forests","EasternHyperOceanicBarrens",
                 "Maritime_Barrens","Avalon_Forests",
                 "StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)


moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_coll2, moose_2020b,
                        by = "Ecoregion")

#Q26. There is a positive relation between moose density and vehicle collisions
# which shows that the higher the density the more collisions that occur, some 
# are outliers which may be due to differences in human population.


plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions (2020)",
     main = "Moose Density vs Vehicle Collisions (2020)")
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Person",
     main = "Collisions Per Capita vs Human Population")

#Q29. A trend I've noticed is that regions with lower human population result in
# higher collisions per capita. This is due to population size being small meaning
# collisions represent a higher proportion relative to population size. This makes
# sense as rural areas like Newfoundland have a high density of moose population
# but relatively low human populations leading to an increased # of collisions per
# capita.