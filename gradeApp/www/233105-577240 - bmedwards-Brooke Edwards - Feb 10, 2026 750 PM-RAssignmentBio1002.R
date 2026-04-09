# Q1
install.packages("dplyr")
library(dplyr)

# Q2
moosedata <- read.csv("MoosePopulation.csv")

# Q3
moose_clean <- na.omit(moosedata)

# Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Q.5 a)
year_min <- min(moose_sel$Year)
  #year_ min 1960L

# Q5 b)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
  #moose_max  41250L

# Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area) 

# Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# Q8 a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Q8 b)
plot(
  moose_west$Year, 
  moose_west$MooseDensity, 
  type = "l", 
  xlab = "Year", 
  ylab = "Moose per km2", 
  main = "Moose density in Western Forests over time")

# Q9 a,b,c
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high <- arrange(moose_2020_high, desc(MooseDensity))

#Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()




#section 2

# Q11 a,b
saplings <- read.cvs("SaplingStudy.cvs")

sap_clean <- na.omit(saplings)

# Q12 a,b
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

# The highest average browsing intensity occurs in the Western_Forests,
# while the lowest average browsing intensity occurs in the 
# EasternHyperOceanicBarrens.

# Q13 a,b
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)

sap_reg_height_low 

# Western_Forest and North_shore_Forest have average sapling heights below 20cm,
# indicating heavier browsing pressure in these regions.

# Q14 a,b
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

avg_browse_spe

# Willow has the highest average browsing score, 
# while Black_spruce has the lowest avergae browsing score.

# Q15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(Averagebrowsing = mean(BrowsingScore)) %>%
  print()

# Q16
barplot(
  fir_reg_browse$Averagebrowsing,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average browsing score",
  main = "Average moose browsing on Balsam Fir by ecoregion",
  cex.names = 0.6
)

# Q17 a,b,c
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

barplot(
  spruce_reg_browse$AverageBrowsing,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average browsing score",
  main = "Average moose browsing on Black_Spruce by ecoregion",
  col = "pink",
  cex.names = 0.6
)

# Moose browsing on Black spruce is generally lower than on Balsam Fir across 
# most ecoregions,suggesting that moose prefer Balsam Fir over Black Spruce as 
# a food source.

# Q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

# Q19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()

print(sap_spe_tally)

# Q20 a, b
# The SaplingStudy dataset does not appear to be evenly distributed, as some 
# ecoregions and tree species have more sampled individuals than others, 
# indicating possible sampling bias

# Recongnizing sampling bias is important because uneven sampling can distort 
# interpretations of browsing pressure and lead to incorrect ecological 
# conclusions.


#section 3

#Q21 a, b
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Q22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) 
  
print(sum_spe_browse)

# Q23 a, b, c

# The figure supports the reseacrhers hypothesis. At low moose densities, 
# browsing intesities varies more among the species, indictating selective 
# feeding, while at higher densities browsing intesitites become more uniform 
# across each species.

# Moose appear to favour Willow and Alder most, consitently shows higher average 
# browsing scores, while Black Spruce is browsed the least across densities. 

# Some species are not shown, due to lack of sufficent data to calculate
# averages, or not present in all ecoregigons. 

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Q25 a, b
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Q26 a, b
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle collisions",
     main = "Moose Density vs Vehicle Collissions (2020)")

# There is a positive relationship between moose denisty and number of veichle 
# collisons. some ecoregions appear to have higher collision numbers than expected 
# suggested possible outliers.

# Q27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_merge_per_capita = collisions2020 / human_pop)

# Q28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_merge_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Moose Collisions Per Capita vs Human Population")

# Q29

# Ecoregions with lower populations tend to have higher collissions per capita,
# higher populations show lower. This is due to rural areas have fewer people,
# and high moose presence. Higher populated areas have less area for moose to live,
# so these results all add up on both ends.





 
