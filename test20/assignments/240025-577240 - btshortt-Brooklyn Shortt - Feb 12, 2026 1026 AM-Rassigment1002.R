library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moosedata <- MoosePopulation
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year, na.rm = TRUE)
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
year_min
moose_max
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata, Ecoregion == "Western_Forests")
moosedata2 <- mutate(moosedata2, MooseDensity = Estimated_Moose_Pop / Area)
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
western <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(western$Year, western$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western_Forests Over Time")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingStudy.csv")
saplings<-SaplingStudy
saplings <- read.csv("SaplingStudy.csv")
setwd("~/Downloads")
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
library(dplyr)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
print(avg_browse_reg)
head(avg_browse_reg, 1)
tail(avg_browse_reg, 1)
library(dplyr)

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE))
print(sap_reg_height)
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)
print(sap_reg_height_low)
library(dplyr)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE))
print(sap_reg_height)
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20)
print(sap_reg_height_low)
library(dplyr)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
head(avg_browse_spe, 1)
tail(avg_browse_spe, 1)
library(dplyr)

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
print(fir_reg_browse)
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Moose Browsing Intensity on Balsam Fir by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
library(dplyr)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
print(spruce_reg_browse)
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Moose Browsing Intensity on Black Spruce by Ecoregion",
        col = "darkolivegreen",
        cex.names = 0.6)
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally()
print(sap_reg_tally)
library(dplyr)
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()

print(sap_spe_tally)
#Question 20
# The SaplingStudy dataset is not evenly distributed. Some ecoregions and tree
# species have more saplings sampled than others, showing that certain
# regions or species may be overrepresented while others are underrepresented.
# Recognizing bias in ecological datasets is important because uneven sampling
# can lead to misleading conclusions about species preferences or ecological
# patterns. Biased data may overemphasize trends from heavily sampled groups
# while underestimating patterns in poorly sampled ones.
library(dplyr)
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean,
 by = "Ecoregion",relationship = "many-to-many")
print(moose_sap)
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE),
 AverageMooseDensity = mean(MooseDensity, na.rm = TRUE))
print(sum_spe_browse)
library(ggplot2)
library(ggplot2)

ggplot(sum_spe_browse,
       aes(x = AverageMooseDensity,
           y = AverageBrowsing,
           color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#Question 23
# The figure provides evidence supporting the researchers hypothesis. At lower
# moose densities, browsing intensity varies strongly among species, while at
# higher densities browsing becomes more uniform, suggesting a shift toward
# less selective feeding.

# Moose appear to favour Balsam Fir most, as it shows higher average browsing
# scores across ecoregions, while Black Spruce is browsed the least overall.

# White Spruce is not shown in the figure because it was not present in all
# ecoregions or did not have sufficient data after grouping and averaging.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests",
                 "Northern_Peninsula_Forests",
                 "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens",
                 "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
library(dplyr)
moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
print(coll_merge)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose per km²)",
     ylab = "Number of Moose-Vehicle Collisions (2020)",
     main = "Relationship Between Moose Density and Vehicle Collisions",
     pch = 16)
# The scatterplot shows general positive relationship between moose density
# and the number of moose-vehicle collisions, suggesting that regions with
# higher moose densities tend to experience more collisions. A potential
# outlier is the Avalon_Forests region, which has a high number of collisions
# relative to its moose density, likely due to higher human population and
# traffic levels.
library(dplyr)
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions per Person",
     main = "Moose-Vehicle Collisions per Capita vs Human Population",
     pch = 16)
#Question 29
# The plot shows that collisions per person are higher in regions with smaller
# human populations and lower in regions with larger populations. This makes
# sense because rural areas have more moose and fewer people, increasing the
# chance of moose-vehicle collisions per person.
