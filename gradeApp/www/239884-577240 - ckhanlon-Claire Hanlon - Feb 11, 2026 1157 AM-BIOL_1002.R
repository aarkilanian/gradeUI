library(readr)
library(dplyr)

moosedata <- read.csv("~/Downloads/MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_clean$Year, na.rm = TRUE)
moose_max <- max(moose_clean$Estimated_Moose_Pop, na.rm = TRUE)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

plot(moosedata2$Year, moosedata2$MooseDensity, xlab="Year", ylab="Moose per sq km", main="Moose density in Newfoundland ecoregions over time")

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type="l", xlab="Year", ylab="Moose per sq km", main="Moose Density Over Time in Western Forests Ecoregion")

moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

saplings <- read.csv("~/Downloads/SaplingStudy.csv")
sap_clean <- na.omit(saplings)

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore))
avg_browse_reg <- arrange(sap_reg_browse, desc(mean_browsing))

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height))
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

par(mar=c(12,4,4,2))
barplot(fir_reg_browse$AverageBrowsing,
        names.arg=fir_reg_browse$Ecoregion,
        ylab="Average Moose Browsing Intensity",
        main="Average Balsam Fir Browsing Intensity by Ecoregion",
        col="forestgreen",
        cex.names=0.6,
        las=2)

spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

par(mar=c(12,4,4,2))
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg=spruce_reg_browse$Ecoregion,
        ylab="Average Moose Browsing Intensity",
        main="Average Black Spruce Browsing Intensity by Ecoregion",
        col="darkolivegreen3",
        cex.names=0.6,
        las=2)

sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by="Ecoregion", relationship="many-to-many")

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore), MeanMooseDensity = mean(MooseDensity)) %>%
  print()

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- rename_with(moose_coll, ~ "Ecoregion", .cols = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by="Ecoregion")

plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab="Moose Density",
     ylab="Number of Collisions (2020)",
     main="Moose Density vs Vehicle Collisions")
abline(lm(collisions2020 ~ MooseDensity, data=coll_merge), col="blue")

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab="Human Population",
     ylab="Collisions per Person",
     main="Collisions per Capita vs Human Population")
