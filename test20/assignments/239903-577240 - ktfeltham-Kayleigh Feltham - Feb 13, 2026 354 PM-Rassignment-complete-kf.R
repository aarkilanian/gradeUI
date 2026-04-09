


moose_sel <- select(moose_clean,
                    Ecoregion,
                    Year,
                    Area,
                    Estimated_Moose_Pop)
install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
#1904
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#41250
moosedata2 <- mutate(moose_sel,
                     MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year,
     moosedata2$MooseDensity,type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Over Time")
moose_west <- filter(moosedata2,
                     Ecoregion == "Western_Forests")
plot(moose_west$Year,
     moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western Forests")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020,
                          MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high,
                               desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- arrange(sap_reg_browse,desc(AverageBrowsing))
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- filter(sap_reg_height,
                             AverageHeight < 20) %>%
  print()
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- arrange(sap_spe_browse,
                          desc(AverageBrowsing))
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Region",
        col = "forestgreen",
        cex.names = 0.6)
#17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Region",
        col = "blue",
        cex.names = 0.6)

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
moose_sap <- left_join(moose_2020b,
                       sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()
#23
install.packages("ggplot2")
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500,
               32000, 270000, 2300)

study_sites <- c("North_Shore_Forests",
                 "Northern_Peninsula_Forests",
                 "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens",
                 "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020,
                         human_pop,
                         study_sites)

View(moose_coll)
MooseCollision <- moose_coll %>%
  rename(Ecoregion = study_sites) 
Moosemerged <- moose_2020  %>%
  left_join(MooseCollision, by = 'Ecoregion')
plot(Moosemerged$MooseDensity, Moosemerged$collisions2020, 
     xlab="Moose Density", ylab = "number of moose-vehicle collisions in 2020", 
     main = "Moose Density vs. moose-vehicle collisions in 2020")
#26b: As the moose density increases the collisions increase. The outliar is when the moose density is at 1, the collisions are at its highest. 
#27
Moosemerged <- Moosemerged %>%
mutate(coll_per_capita = collisions2020 / human_pop)  
#28
plot(human_pop,collisions2020 , xlab="Human Population", ylab= "Collisions per Capita", main = "Collisions per Capita vs. Human Population")
#29:The least amount of people there is in an area, the more a collision will occur per capita. The more peeople per capita, the least they are affected.