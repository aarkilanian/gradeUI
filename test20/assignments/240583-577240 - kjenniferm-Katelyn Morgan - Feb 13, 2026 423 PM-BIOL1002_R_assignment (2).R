# Title: R assignment #1 BIOL 1002
# Author: Katelyn Morgan
# Date: Feb. 10. 2026

Moosedata <- read.csv("MoosePopulation.csv")

# Question 1
install.packages("dplyr")
library(dplyr)

# Question 2
Moosedata <- read.csv("MoosePopulation.csv")

# Question 3
View(Moosedata)
Moose_clean <- na.omit(Moosedata)

# Question 4 
Moose_sel <- select(Moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5a
min(Moosedata$Year)
# Oldest observation (year): 1904

# Question 5b
max(Moosedata$Estimated_Moose_Pop)
# Highest Estimated_Moose_Pop: 41250
# This number is for: Central_Forests

# Question 6
moosedata2 <- mutate(Moose_sel, 
                    MooseDensity = Estimated_Moose_Pop / Area)

# Question 7
plot(Moosedata$Year, Moosedata$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8b
plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")

# Question 9a
moose_2020 <- filter(Moosedata, Year == 2020)

#Question 9b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Question 9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Question 11
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

# Question 12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
# Ecoregion with highest average browsing: Northern Peninsula Forests
# Ecoregion with lowest average browsing: StraitOfBelleIsleBarrens 

# Question 13 
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

# Ecoregions with average height less than 20 cm: Northern Peninsula Forests and Western Forests 

# Question 14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

# Species with highest browsing score: Black_Ash
# Species with lowest browsing score: Black_Spruce

# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

# Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Average Balsam Fir browsing by ecoregion",
        col = "LightGreen",
        cex.names = 0.6)

# Question 17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Average Black Spruce browsing by ecoregion",
        col = "darkolivegreen3",
        cex.names = 0.6)

# Compared to Balsam Fir, Black Spruce shows that it is browsed less than Balsam Fir across most ecoregions.
# The Balsam Fir is eaten more often, especially in the Northern Peninsula Forests. 

# Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

# Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Question 20
# The SaplingStudy dataset is not evenly distributed because Balsam Fir
# appears overrepresented and Black Ash appears underrepresented.

# Recognizing bias in ecological datasets is important because
# it can affect results and lead to incorrect conclusions about species abundance or patterns.

# Question 21
moose_2020b <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

# Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()

# Question 23
library(ggplot2)
ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

# There is evidence supporting the hypothesis because
# browsing appears more selective at low moose density and more similar
# among species at higher density.

# Moose appear to favour Alder the most and browse
# Balsam Fir the least. Alder has the highest average browsing scores across
# ecoregions, while Balsam Fir has lower scores overall

# The species Black Ash is not shown because there were no
# observations for that species after filtering. Black Ash had too few or no data 
# points remaining after filtering, so it could not be plotted. 

# Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose density",
     ylab = "Number of collisions in 2020",
     main = "Moose density vs moose-vehicle collisions")
# There appears to be an upward trend between moose density and collisions.
# One possible outlier is the point with about 1.0 moose density and very high collisions
# since it is much higher than the others.

# Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# Question 28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human population",
     ylab = "Collisions per person",
     main = "Collisions per capita vs human population")

# Question 29
# In this graph, the Collisions per capita generally decreased as human population
# increased. This makes sense because in places with larger human populations,
# collisions are spread across more people, so the number of collisons per person
# is lower. 