# Title: BIOL 1002 R Assignment
# Author: Tristan Spurrell 202511105
# Date: 13-02-2026

#Question 1
install.packages("dplyr")
library(dplyr)

#Question 2
moosedata <- read.csv("MoosePopulation.csv")

#Question 3
View(moosedata)
moose_clean <- na.omit(moosedata)

#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5
year_min <- min(moose_sel$Year)
year_min
#The oldest observation in the data set is from 1904 
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
#The highest 'Estimated_Moose_Pop' is 41250

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
head(moosedata2)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
unique(moosedata2$Ecoregion)
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density over time in Western_Forests")

#Question 9
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD

#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Question 11
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

#Question 12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#Moose browsing pressure varies from around 1-4.57 for the different ecoregions listed
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
avg_browse_reg
#The highest moose browsing pressure is the Northern_Peninsula_Forests with a average browsing of 4.57
#The lowest moose browsing pressure is the StraitOfBelleIsleBarrens with a average of 1

#Question 13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
#The average tree height varys from around 32.4-18.9 across the different ecoregions listed
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)
sap_reg_height_low
#Regions with average height less than 20cm, Northern_Peninsula_Forests with 19.9 and Western_Forests 18.9

#Question 14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#The average browsing score varys from 5-2.33 across the different tree sapling species
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
avg_browse_spe
#The Black_Ash species has the highest browsing of 5 while the Black_Spruce have the lowest with a value of 2.33

#Question 15
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()

#Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Balsam Fir browsing intensity by ecoregion",
        col = "green",
        cex.names = 0.6)

#Question 17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Black Spruce browsing intensity by ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
#Black Spruce generally shows lower browsing than Balsam Fir across most ecoregions, with the biggest difference in EasternHyperOceanicBarrens ecoregion with Fir > Spruce

#Question 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#No there were different numbers of tree saplings counted for most regions.

#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#No, the same number of tree saplings counted for each species were not the same.

#Question 20
#a) The SaplingStudy dataset is not evenly distributed. Some ecoregions and tree species were sampled more than others, meaning certain groups are overrepresented while others are underrepresented.
#b)It is important to recognize bias in ecological datasets because uneven sampling can affect the results. If some regions or species are sampled more than others could lead to inaccurate conclusions about moose browsing patterns.

#Question 21
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")


#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
#a)There is some evidence supporting the hypothesis. At lower moose densities, browsing intensity varies more between species, suggesting selective feeding. At higher densities, browsing scores increase across most species.
#b)Moose appear to favor alder the most, as it consistently shows the highest average browsing scores. They browse black sspruce the least, as it has the lowest average browsing intensity.
arrange(sum_spe_browse, desc(AvgBrowsing))
arrange(sum_spe_browse, AvgBrowsing)
#c)The species "Alder", "Black_Spruce", "White_Birch", "Balsam_Fir", "Willow", "Black_Ash" is not shown in the figure because there were no matching moose density values for 2020 in that ecoregion, so an average density could not be calculated.
unique(sap_clean$Species)

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25
moose_coll2 <- rename_with(moose_coll, ~"Ecoregion", study_sites)
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")

#Question 26
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions",
     main = "Moose Density vs Collisions")
#Regions with more moose generally have more collisions, although a few regions do not follow the pattern exactly.

#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

#Question 28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Person",
     main = "Collisions per Capita vs Human Population")

#Question 29
#There is a negative relationship between human population and collisions per person. Areas with smaller populations have more collisions per person, which makes sense since rural areas usually have more moose and fewer people.













