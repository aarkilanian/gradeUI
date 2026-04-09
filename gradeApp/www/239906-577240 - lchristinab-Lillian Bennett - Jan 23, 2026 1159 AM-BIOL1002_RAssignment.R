# Title: Biology 1002 R Assignment
# By: Lillian Bennett
# Date Jan 16 2026

# Install dplyr package
install.packages("dplyr")

# 1. Load library
library("dplyr")

# 2. Load data 
moosedata <- read.csv("~/Documents/BIOL1002_RAssignment/MoosePopulation.csv")
View(moosedata)

# 3. Omit na
moose_clean <- na.omit(moosedata)
View(moose_clean)

# 4. Select data
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)

# 5.a) Minimum year 
year_min <- min(moose_sel$Year)
# [1] 1904

# 5.b) Highest estimated moose pop 
moose_max <- max(moose_sel$Ecoregion)
# [1] "Western_Forests"

# 6. Moose density 
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
View(moosedata2)

# 7. Plot
plot(moosedata2$Year, moosedata2$MooseDensity,
     type ="l",
     xlab="year", 
     ylab="Moose per sq km", 
     main="Moose density in Newfoundland ecoregions over time")

# 8.a) Filter
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
View(moose_west)

# 8.b) Line Graph Western_Forests
plot(moose_west$Year,moose_west$MooseDensity, 
     type="l",
     xlab="year",
     ylab="Moose per square km",
     main ="Moose density in newfoundland western forests ecoregion over time")

# 9.a) Filter year 2020
moose_2020 <- filter(moosedata2, Year == "2020")
View(moose_2020)

# 9.b) Filter 2020 and and moose density > 2.0 moose per sq km 
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
View(moose_2020_high)

# 9.c) Arrange moose density descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
View(moose_2020_high_byD)

# 10. Filter 2020 and moose density with arrange using pipes
moosefinal <- moosedata2 %>%
  filter(Year == "2020") %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#       Ecoregion Year  Area Estimated_Moose_Pop MooseDensity
# 1 Northern_Peninsula_Forests 2020 12000               32000     2.666667
# 2        North_Shore_Forests 2020  8400               21740     2.588095
# 3            Western_Forests 2020  6800               17000     2.500000
# 4            Central_Forests 2020 20500               41250     2.012195
# 11.a) Load SamplingStudy
saplings <- read.csv("~/Documents/BIOL1002_RAssignment/SaplingStudy.csv")
View(saplings)

# 11.b) Omit na (SalingStudy)
sap_clean <- na.omit(saplings)
View(sap_clean)
 
# 12.a) Moose browsing pressure variation across different Ecoregions
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsingScore = mean(BrowsingScore)) %>%
  print()
View(sap_reg_browse)
# A tibble: 9 × 2
# Ecoregion                  AvgBrowsingScore
# <chr>                                 <dbl>
#   1 Avalon_Forests                         2   
# 2 Central_Forests                        4   
# 3 EasternHyperOceanicBarrens             2.4 
# 4 Long_Range_Barrens                     2.6 
# 5 Maritime_Barrens                       1.83
# 6 North_Shore_Forests                    4.38
# 7 Northern_Peninsula_Forests             4.57
# 8 StraitOfBelleIsleBarrens               1   
# 9 Western_Forests                        4.5

# 12.b) Ecoregions with highest and lowest amount of moose browsing
avg_browse_reg <- arrange(sap_reg_browse, desc(AvgBrowsingScore))
View(avg_browse_reg)
# Highest - Northern_Peninsula_Forests
# Lowest - StraitofBelleIsleBarrens

# 13.a) Tree hight variation across different Ecoregions 
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(Height)) %>%
  print()
View(sap_reg_height)
# A tibble: 9 × 2
# Ecoregion                  `mean(Height)`
# <chr>                               <dbl>
#   1 Avalon_Forests                       32.4
# 2 Central_Forests                      23.8
# 3 EasternHyperOceanicBarrens           31.6
# 4 Long_Range_Barrens                   29.9
# 5 Maritime_Barrens                     26.7
# 6 North_Shore_Forests                  22.3
# 7 Northern_Peninsula_Forests           19.9
# 8 StraitOfBelleIsleBarrens             25.4
# 9 Western_Forests                      18.9

# 13.b) Filter average heights less than 20cm
sap_reg_height_low <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AvgHeight = mean(Height)) %>%
  filter(AvgHeight<20) %>%
  print()
View(sap_reg_height_low)
# A tibble: 2 × 2
# Ecoregion                  AvgHeight
# <chr>                          <dbl>
#   1 Northern_Peninsula_Forests      19.9
# 2 Western_Forests                 18.9

# 14.a) Browsing score variation across different species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize (AvgBrowsingScore = mean(BrowsingScore)) %>%
  print()
# A tibble: 6 × 2
# Species        AvgBrowsingScore
# <chr>                     <dbl>
#   1 "Alder "                   4.25
# 2 "Balsam_Fir"               3.14
# 3 "Black_Ash"                5   
# 4 "Black_Spruce"             2.33
# 5 "White_Birch"              3.14
# 6 "Willow"                   4.31

# 14.b) Species with highest and lowest amount of Moose Browsing
avg_browsing_spe <- arrange(sap_spe_browse, desc(AvgBrowsingScore))
View(avg_browsing_spe)
# Highest - Black_Ash
# Lowest - Black_Spruce

# 15. Balsam Fir browsing intensity variation by Ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsingScore = mean(BrowsingScore))
View(fir_reg_browse)

# 16. Balsam Fir bar Graph
barplot(fir_reg_browse$AvgBrowsingScore, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Browsing Score",
        main = "Balsam Fir Average Browsing Intensity by Ecoregion",
        col = "pink",
        cex.names = 0.6)

# 17.a) Black Spruce browsing intensity variation by Ecoregion
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsingScore = mean(BrowsingScore))
View(spruce_reg_browse)

# 16. Balsam Fir Bar Graph
barplot(spruce_reg_browse$AvgBrowsingScore, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Browsing Score",
        main = "Black Spruce Average Browsing Intensity by Ecoregion",
        col = "magenta",
        cex.names = 0.6)

# 17.c) The black spruice has a very low browsing intensity in Eastern Hyper 
# Oceanic Barrens and Marritime Barrins, and the Balsam Fir has no Browsing score
# for the Western Forests like Black Spruce does. 

# 18. Number of Tree Samplings counted in each Ecoregion
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
View(sap_reg_tally)
# A tibble: 9 × 2
# Ecoregion                      n
# <chr>                      <int>
#   1 Avalon_Forests                 5
# 2 Central_Forests                5
# 3 EasternHyperOceanicBarrens     5
# 4 Long_Range_Barrens             5
# 5 Maritime_Barrens               3
# 6 North_Shore_Forests            8
# 7 Northern_Peninsula_Forests     7
# 8 StraitOfBelleIsleBarrens       1
# 9 Western_Forests                5

# 19. Number of Tree saplings counted in each species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
View(sap_spe_tally)
# A tibble: 6 × 2
# Species            n
# <chr>          <int>
#   1 "Alder "           8
# 2 "Balsam_Fir"      11
# 3 "Black_Ash"        1
# 4 "Black_Spruce"     9
# 5 "White_Birch"      7
# 6 "Willow"           8

# 20.a) The Sampling Study had the Ecoregions of North Shore and Northern 
# Peninsula overrepresented and Strait of Belle Isle underrepresented. The
# Black Ash Species was underrepresented and the Balsam Fir was overrepresented.

# 20.b) It is important to recognize bias in ecological datasets because the data
# can show certian trends for overrepresented regions or species as compared to
# underrepresented regions or species, therefor their must be caution when 
# comparing if there may be a bias or uneven distribution of ecological datasets.

# 21.a) MooseData from 2020 with Moose Density column
moose_2020b <- filter(moose_clean, Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
View(moose_2020b)

# 21.b) Join MooseData from 2020 with Saplings Dataset using Ecoregion Column
moose_sap <- left_join(moose_2020b, sap_clean, 
                       by = "Ecoregion", relationship = "many-to-many")
View(moose_sap)

#22. Browsing by Species Density
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize (AvgBrowsingScore = mean(BrowsingScore), 
             AvgMooseDensity = mean(MooseDensity)) %>%
  print()
View(sum_spe_browse)
# A tibble: 38 × 4
# Groups:   Species [6]
# Species      Ecoregion                  AvgBrowsingScore AvgMooseDensity
# <chr>        <chr>                                 <dbl>           <dbl>
#  1 "Alder "     Avalon_Forests                          4             0.962
# 2 "Alder "     Central_Forests                         4.5           2.01 
# 3 "Alder "     EasternHyperOceanicBarrens              4             0.5  
# 4 "Alder "     Long_Range_Barrens                      3.5           0.602
# 5 "Alder "     Maritime_Barrens                        3.5           0.960
# 6 "Alder "     North_Shore_Forests                     4.5           2.59 
# 7 "Alder "     Northern_Peninsula_Forests              5             2.67 
# 8 "Alder "     Western_Forests                         5             2.5  
# 9 "Balsam_Fir" Avalon_Forests                          2             0.962
# 10 "Balsam_Fir" Central_Forests                         3.5           2.01 
# ℹ 28 more rows
# ℹ Use `print(n = ...)` to see more rows

#23. Figure to visualize average browsing intensity on different tree species
#     changes with moose density across ecoregions
library (ggplot2)
ggplot(sum_spe_browse, aes(x = AvgMooseDensity, y = AvgBrowsingScore,
                           color=Species)) + geom_point(size=3) +
  theme_minimal()+
  labs(title = "Browsing Intensity Across Moose Density by Species", 
       x = "Average Moose Density", 
       y= "Average Browsing Score")

#23. a) There is evidence to support the researchers' hypothesis that average
# browsing intensity on different species changes with moose density across 
# ecoregions. Moose do show a strong preference when there are less moose in an
# and a more generalist browsing when there are more mooose around to share with. 
# Some types are not eaten at all while others are very high in Browsing Score. 

#23. b) The Willow seems to be the preferred Tree of choice. The Browsing Score
# is the highest in both low and high density areas. Black Spruce seems to be
# the least favorite, it is not chosen at all in low density areas and chosen
# the least in high density areas. 

#23. c) Black Ash does not show up on the figure because the browsing score and 
# moose density are both very high so the plot would be on the far right of
# the figure. 

#24. Collisions in 2020
collisions2020 <- c(56,60, 14, 36, 48, 10, 40, 110,6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests",
               "Long_Range_Barrens", "Central_Forests", "Western_Forests",
               "EasternHyperOceanicBarrens", "Maritime_Barrens",
               "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(study_sites, human_pop, collisions2020)
View(moose_coll)

#25. a) Renamne column in Moose Collisions 2020 to match Moose Data 2020
moose_coll2 <- moose_coll%>%
  rename(Ecoregion = study_sites) # rename_with will not work here, used rename()
View(moose_coll2)

#25. b) Join Moose Collisions 2020 with Moose Data 2020
coll_merge <- left_join(moose_2020, moose_coll2, 
                        by = "Ecoregion", 
                        relationship = "many-to-many")
View(coll_merge)

#26. a) Moose density relation to number of Moose Vehicle Collisions
plot (coll_merge$MooseDensity, coll_merge$collisions2020, 
      xlab = "Moose Density", 
      ylab = "Moose Collisions",
      main = "Moose Density related to Moose Vehicle Collisions")

#26. b) There is a trend that when Moose Density increases so does Moose Collisions, 
# when Density is zero there are very few collsions but when Density is above 2.5
# collisions increase significantly. There is one outlier in Density at 1 and there
# are over 100 collisions.

#27. Collisions per Capita
coll_merge_per_capita <- mutate(coll_merge, 
                                coll_per_capita = collisions2020/human_pop)
View(coll_merge_per_capita)

#28. Scatterplot of Collisions per Capita versus Human Population
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita related to Human Population")

#29. There are more collisions per capita in regions with smaller human populations. 
# Based on our knowledge of moose and human population in Newfoundland, 
# this is because there are less moose population where there are more human population
# and therefore there are less moose accidents. 











