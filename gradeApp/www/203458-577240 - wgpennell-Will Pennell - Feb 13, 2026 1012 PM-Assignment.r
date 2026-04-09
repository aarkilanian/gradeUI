### Question 1:
library(dplyr)

### Question 2:
moosedata <- read.csv("MoosePopulation.csv")
# View(moosedata)

### Question 3:
moose_clean <- na.omit(moosedata)
# View(moose_clean)

### Question 4:
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

### Question 5:
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

### Question 6:
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

### Question 7:
plot(moosedata2$Year, moosedata2$MooseDensity,
    xlab = "year",
    ylab = "Moose per sq km",
    main = "Moose density in Newfoundland ecoregions over time"
)

### Question 8:
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests")

### Question 9: ***
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

### Question 10:
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

### Question 11:
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
View(sap_clean)

### Question 12:
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_reg_browse)

avg_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowsing))
# Highest - Northern_Peninsula_Forests, Lowest - StraitOfBelleIsleBarrens

### Question 13:
sap_reg_height <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageHeight = mean(Height))
View(sap_reg_height)

sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20)
print(sap_reg_height_low)
# It seems like Northern_Peninsula_Forests & Western_Forests are damaged by browsing w/ heights less than 20cm

### Question 14:
sap_spe_browse <- sap_clean %>% 
  group_by(Species) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))

avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing))
View(avg_browse_spe)
# Highest - Black_ASh, lowest - Black_spruce

### Question 15:
fir_reg_browse <- sap_clean %>% 
  filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))

### Question 16:
barplot(fir_reg_browse$AverageBrowsing,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Average Browsing Score by region (FIR)",
  col = "lightblue",
  las = 2,
  cex.names = 0.5
)

### Question 17:
spruce_reg_browse <- sap_clean %>% 
  filter(Species == "Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))

barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score", 
        main = "Average Browsing Score by region (SPRUCE)",
        col = "pink",
        las = 2,
        cex.names = 0.5
)
#  Spruce appears to not appear whatsoever in EasternHyperOceanicBarrens and Maritime_Barrens
#  However spruce appears to have higher scores in Long_Range_Barrens and North_Shore_Forests than fir. 


### Question 18:
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

### Question 19:
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()

## Question 20:
# When it comes to Species count, there is only 1 sample for Black_Ash. Furthermore, there is 11 samples for Balsam_Fir. This is very unbalanced.
# Furthermore, there is only 1 sample for StraitOfBelleIsleBarrens and 3 for Maritime barrens, compared to 7 for Northern_Peninsula_Forests. This is unbalanced as well.

# Having an unbalanced dataset can lead to false patterns appearing due to flukes and luck, this can lead to poor conclusions from the study. 
# Furthermore if you were to do any sort of statistical modelling, it would likely lead to severe overfitting and poor generalization.


### Question 21:
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

### Question 22:
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore), AverageDensity = mean(MooseDensity))
View(sum_spe_browse)

## Question 23:
# Yes the visualization appears to support the hypothesis, that moose are generally more picky when there is low moose density, with Alder and Willow having the highest Average Browsing Scores at Avg Moose Density from [0, 1], much higher than any other species. When in comparison to high moose density, all species have high browsing score.
# Moose seem to enjoy Willows and Alders the most, and seem to dislike Black Spruce.
# Black_Ash, is not pictured on the graph due to a z-order issue. It's sole data point overlaps with Willow	Western_Forests	5.00	2.5000000, and that datapoint is drawn over it.

### Question 24:
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

### Question 25:
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020b, by = 'Ecoregion', relationship = "many-to-many")
View(moose_coll2)

### Question 26:
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
  xlab = "Moose Density", 
  ylab = "Collisions", 
  main = "Moose Density vs Collisions (2020)",
  pch = 15,
  col = "brown"
)

# There appears to be a positive correlation between moose density and collisions.
# There is an outlier in the data, "Avalon_Forests has a much higher rate of collisions than other points.
# This is likely due to the higher human population in the area, normalization should ideally be done.

### Question 27:
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)

### Question 28:
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
  xlab = "Human Population", 
  ylab = "Collisions per Capita", 
  main = "Human Population & Collisions per Capita (2020)",
  pch = 15,
  col = "lightgreen"
)

### Question 29:
# Human population appears to be inversely correlated with collisions per capita. This is likely due to the fact that Newfoundland is sparsely populated, except the Avalon Peninsula.
# Perhaps this is due to people driving across areas with little human population and encountering more moose. Due to the spread out nature of Newfoundland's major towns.
