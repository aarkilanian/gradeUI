###QUESTION 21
###a)
moose_2020 <- filter(moose_clean, Year == "2020")
moose_2020b <- mutate (moose_2020, MooseDensity = Estimated_Moose_Pop/Area)

###b)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

###QUESTION 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(BrowsingScore=mean(BrowsingScore),MooseDensity=mean(MooseDensity)) %>%
  print()

###QUESTION 23
###a)
# Moose have a much higher preference when it comes to areas of of both extremes.
###b)
# The sapling species that is favored the most is the willow. The sapling species
# that is browsed the least is Black_Spruce.
###c)
# Black_Ash is not shown on the graph. This is because it only has one set of data
# collected.

###QUESTION 24
Collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
Human_Pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
Study_Sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(Collisions2020, Human_Pop, Study_Sites)

###QUESTION 25
###a)
moose_coll2 <- rename(moose_coll, Ecoregion = Study_Sites)

###b)
coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion", relationship = "many-to-many")

###QUESTION 26
###a) 
plot(coll_merge$MooseDensity, coll_merge$Collisions2020, main = "Scatter Plot", xlab = "MooseDensity", ylab = "Collison2020")

###b)
# At the point where the MooseDensity is at its highest is the same point where
# collisions were the most frequent. 

###QUESTION 27
coll_merge_per_capita <- mutate(coll_merge, Coll_Per_Capita = Collisions2020 / Human_Pop)

###QUESTION 28
plot(coll_merge_per_capita$Coll_Per_Capita, coll_merge_per_capita$Human_Pop, main = "Scatter Plot", xlab = "Coll_Per_Capita", ylab = "Human_Pop")

###QUESTION 29
# Wherever the human population is at its highest, is when the most collisions
# will occur.
