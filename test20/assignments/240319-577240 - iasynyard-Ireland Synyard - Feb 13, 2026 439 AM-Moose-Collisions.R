library(dplyr)

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)

moose_2020 <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

#Question 25
#a
moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", .cols = study_sites)
#b
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion") %>%
  print()

#Question 26
#a
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density",
     ylab = "Collisions in 2020",
     main = "Moose vs Collisions",
     pch = 19,
     col = "red")
#b
#The moose and vehicle collisions increases as the density rises
#The outlier is one site which has a higher amount of collisions than others.

#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

coll_merge_per_capita %>%
  arrange(desc(coll_per_capita)) %>%
  select(Ecoregion, coll_per_capita)

#Question 28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Collisions vs Human Population",
     pch = 19,
     col = "darkgreen")

#Question 29
#Collisions per capita seem to be the highest in the ecoregions with very small
#human populations
#This makes sense for Newfoundland because it is a smaller area and so with denser 
#moose populations, it is more likely for collisions.