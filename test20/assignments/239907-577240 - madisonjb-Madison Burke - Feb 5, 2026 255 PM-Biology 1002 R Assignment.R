#Madison Burke 
#Question 1 
install.packages("dplyr")
library(dplyr)
#Question 2
moosedata <- read.csv("MoosePopulation.csv")
#Question 3 
View(moosedata)
moose_clean <- na.omit(moosedata)
View(moose_clean)
#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5
year_min <- min(moose_sel$Year)
##(a) year_min 1904L
moose_max <- max(moose_sel$Estimated_Moose_Pop)
##(b) moose_max 41250L
#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Question 8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")
#Question 9
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Question 10 
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Question 11
saplings<- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
View(sap_clean)
#Question 12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
##Northern_Peninsula (4.57) had the highest average browsing
##StraitOfBelleIsle (1.0) had the lowest average browsing
#Question 13 
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
##Northern_Peninsula and Western_Forests have average tree heights less than 20cm and are considered severely browsed by moose.
#Question 14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
##Black_Ash (5.00) has the highest average browsing score
##Black_Spruce (2.33) has the lowest average browsing score
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
        ylab = "Average Browsing Score",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
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
        ylab = "Average Browsing Score",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
##Black Spruce browsing varies across ecoregions, with the highest browsing in North_Shore_Forests and Northern_Peninsula, and the lowest browsing in Maritime_Barrens. Compared to Balsam Fir, browsing intensity differs between species across regions.
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
## (a) The dataset does not appear evenly distributed because some ecoregions or species have more samples than others, meaning some groups may be overrepresented while others are underrepresented.
## (b) Recognizing bias is important because uneven sampling can affect conclusions and may lead to incorrect interpretations about browsing patterns or ecological trends.
#Question 21
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
View(moose_sap) 
#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AverageBrowsing = mean(BrowsingScore),
    AverageMooseDensity = mean(MooseDensity)
  ) %>%
  print()
#Question 23
##(a)The figure suggests moose are more selective at low density because browsing scores vary widely among species. At higher densities, browsing becomes more similar across species, supporting the hypothesis of less selective browsing.
##(b)Moose appear to favour Willow the most based on consistently high browsing scores, while Black Spruce is browsed the least.
##(c)A species may be missing because there were no valid observations after filtering or joining the datasets, so it could not be plotted.
#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Question 25
moose_coll2 <- moose_coll %>%
  rename_with(~"Ecoregion", study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
#Question 26
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-vehicle collisions (2020)",
     main = "Moose Density vs Moose-vehicle collisions")
##Collisions generally increase as moose density increases, showing a positive relationship.
# One or two ecoregions may sit noticeably above/below the trend, which could be considered outliers.
#Question 27
coll_merge_per_capita <- mutate(coll_merge,
                                coll_per_capita = collisions2020 / human_pop)
#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human population")
#Question 29
# Collisions per person tend to be higher in ecoregions with smaller human populations (values are more “inflated” when you divide by a small number).
# This makes sense because rural areas can have fewer people but still lots of moose and driving on moose habitat roads.