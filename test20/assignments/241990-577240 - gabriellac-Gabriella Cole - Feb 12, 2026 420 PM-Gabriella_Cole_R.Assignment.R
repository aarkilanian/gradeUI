install.packages("dplyr") #only run once
library(dplyr)
moosedata <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "l",
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion =="Western_Forests")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) 
# Question 12b) Northern_Peninsula_Forests had the highest browsing, StraitOfBelleIsleBarrens had the lowest
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)
print(sap_reg_height_low)
# Question 13b) Northern_Peninsula_Forests and Western_Forestshave average tree heights less than 20 cm.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
print(avg_browse_spe)
# Question 14b) Willow has the highest average browsing score and Black_Spruce has the lowest.
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
# Question 17c) Moose browse more heavily on Balsam Fir than on Black Spruce across most ecoregions.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# Question 20a) The SaplingStudy dataset is not evenly distibuted.Some species (eg. Balsam_Fir) and some ecoregions have more trees sampled than others,meaning certain groups are overrepresented while others are underrepresented.
# Question 20b) Recognizing bias is important because uneven sampling can affect our interpretation of browsing patterns. If some species or regions are overrepresented, the results may not accurately reflect true ecological patterns.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
sum_spe_browse <- moose_sap %>% 
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity))
print(sum_spe_browse)
# Question 23a) yes, the results show that browsing differs among species and tends to increase in areas with higher moose density, supporting the hypothesis.
# Question 23b) Moose appear to favour Willow and Balsam Fir the most, while Black Spruce is browsed the least.
# Question 23c) Black ash is not shown because it likely had insufficient data or missing values after merging the datasets.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020)
# Question 26b) There seems to be a positive relationship between moose density and the number of collisions. Regions with higher moose density tend to have more collisions, although there may be some variability between sites.
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita)
# Question 29) There seems to be a negative relationship between human population and collisions per capita. Less populated regions tend to have higher collisions per person, which makes sense because rural areas often have higher moose densities and more wildlife crossings.
