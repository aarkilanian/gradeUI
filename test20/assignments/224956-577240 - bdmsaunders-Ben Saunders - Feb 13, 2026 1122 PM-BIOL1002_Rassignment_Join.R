moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
  mean_MooseDensity = mean(MooseDensity)
# The figure supports the hypothesis, since At low moose density, browsing is mostly on a few species, but at higher densities the scores increase.
# Moose appear to favor Willow the most across all data points. Moose also appear to favor spruce the least
# Black Ash doesn't appear in the figure, most likely because Moose don't prefer it.
  collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
  human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
  study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
  moose_coll <- data.frame(collisions2020, human_pop, study_sites)
plot(moose_2020b$MooseDensity, moose_coll$collisions2020,
 xlab = "Moose Density",
 ylab = "Moose-Vehicle Collisions (2020)",
 main = "Moose Density vs Collisions")  
# As moose density increases, collisions seem to also increase. However, there is one outlier at a density of 1, where collisions are unnaturally high.
coll_merge_per_capita <- moose_coll %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population",
     pch = 19,
     col = "beige")
# Collisions tend to be higher in regions with smaller populations. This is likely because rural areas often have more moose and roads passing through wilderness, increasing collision risk even with lower populations. 