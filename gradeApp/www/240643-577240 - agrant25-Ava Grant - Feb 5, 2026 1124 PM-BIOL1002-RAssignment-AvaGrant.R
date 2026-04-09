#AvaGrant
#BIOL1002-R-Assignment
#Feb 5 2026, due Feb 13 2026

#Part1
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose Density", main = "Moose Density in Newfoundland Western Forests Over Time")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Part2
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))%>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
#Northern Peninsula Forests had the highest average browsing score, and Strait of Belle Isle Barrens had the lowest.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- filter(sap_reg_height, AverageHeight<20) %>%
  print()
#Northern Peninsula Forests and Western Forests have averages heights less than 20cm
sap_spe_browse <- sap_clean %>%
  group_by(Species)%>%
  summarize(AverageBrowsing = mean(BrowsingScore))%>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  + arrange(desc(AverageBrowsing))%>%
  + print()
#Black Ash has the highest browsing and Black Spruce has the lowest
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Score by Ecoregion", col = "forestgreen", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarize(AverageBrowsing = mean(BrowsingScore))%>%
  print()
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Score by Ecoregion", col = "forestgreen", cex.names = 0.6)
#Black spruce has a lower average browsing score by ecoregion, and has a few regions with no browsing. Balsam fir has a higher average across more ecoregions.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print ()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#20a I think that the saplingstudy dataset is not evenly distributed. The black ash for example was only counted once, while balsim fir was counted 11 times.
#20b It is important to recognize bias because it can skew our perception of the data. If there is a lower sampling size of one species, the results will be way more inaccurate and allow us to draw conclusions that are untrue.
#Part3
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore), AverageDensity = mean(MooseDensity)) %>%
  print()
#23a Based on the figure, the researcher's hypothesis is supported. Moose show strong preferences for the Willow species at low density, while at high density every species is around the same.
#23b Moose favour Willow and do not favour black-spruce.
#23c Black Ash is not shown on the figure because the tally was only one.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Number of Moose-Vehicle Collisions (2020)", main = "Moose Density vs. Vehicle Collisions", pch = 16, col = "forestgreen")
#As moose density increases, so does the number of collisions. There are a few outliers, but the general trend is an increase.
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions per Capita",  main = "Moose Collisions per Capita vs Human Population", pch = 16, col = "darkred")
#29 As human population decreases, the collisions per capita increases. This would make sense, as where there are less people there tends to be more moose.
