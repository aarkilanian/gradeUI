# Name: Jack Fagan (202511251
# Biology 1002 R Assignment
# Date: February 13, 2026

install.packages("dplyr")
library(dplyr)
Moosedata <- read.csv("MoosePopulation.csv")
View(Moosedata)
 Moosedata <- na.omit(Moosedata)
 Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)
 min(Moosedata$Year)
[1] 1904
 # The oldest observation was made in 1904.
   max(Moosedata$Estimated_Moose_Pop)
[1] 41250
 # The highest estimated moose population is 41250.
   # This number is for a particular Ecoregion as it only represents the estimated moose population in Central Forests.
   Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)
 plot(Moosedata$Year, Moosedata$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
 MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")
 View(MooseDataWest)
 plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
       +      xlab = "year",
       +      ylab = "Moose per sq km",
       +      main = "Moose density in Newfoundland Western Forests Region over time")
plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
       +      type = "l",
       +      xlab = "Year",
       +      ylab = "Moose per sq km",
       +      main = "Moose density over time in Western Forests")
MooseData_2020 <- filter(Moosedata, Year == 2020)
View(MooseData_2020)
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)
View(MooseData_2020_b)
arrange(MooseData_2020_b, desc(MooseDensity))
Ecoregion Year  Area
1 Northern_Peninsula_Forests 2020 12000
2        North_Shore_Forests 2020  8400
3            Western_Forests 2020  6800
4            Central_Forests 2020 20500
Estimated_Moose_Pop MooseDensity
1               32000     2.666667
2               21740     2.588095
3               17000     2.500000
4               41250     2.012195
View(MooseData_2020_b)
MooseData_final <- Moosedata %>%
  +     filter(Year == 2020) %>%
  +     filter(MooseDensity > 2.0) %>%
  +     arrange(desc(MooseDensity)) %>%
  +     print()
Ecoregion Year  Area
1 Northern_Peninsula_Forests 2020 12000
2        North_Shore_Forests 2020  8400
3            Western_Forests 2020  6800
4            Central_Forests 2020 20500
Estimated_Moose_Pop MooseDensity
1               32000     2.666667
2               21740     2.588095
3               17000     2.500000
4               41250     2.012195
library(readr)
SaplingStudy <- read_csv("SaplingStudy.csv")
View(SaplingStudy)
Saplings <- read.csv("SaplingStudy.csv")
SaplingsClean <- na.omit(Saplings)
View(SaplingsClean)
library(dplyr)
 BrowsingSummary <- SaplingsClean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(MeanBrowsing = mean(BrowsingScore, na.rm = TRUE))
 print(BrowsingSummary)
Ecoregion                  MeanBrowsing
<chr>                             <dbl>
  1 Avalon_Forests                     2   
2 Central_Forests                    4   
3 EasternHyperOceanicBarrens         2.4 
4 Long_Range_Barrens                 2.6 
5 Maritime_Barrens                   1.83
6 North_Shore_Forests                4.38
7 Northern_Peninsula_Forests         4.57
8 StraitOfBelleIsleBarrens           1   
9 Western_Forests                    4.5 
 #Highest Browsing: Northern Peninsula Forests (4.57)
   #Lowest Browsing: Strait of Belle Isle Barrens (1)
   View(SaplingsClean)
 sap_reg_height <- SaplingsClean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height)) %>% print()
Ecoregion                  AverageHeight
<chr>                              <dbl>
  1 Avalon_Forests                      32.4
2 Central_Forests                     23.8
3 EasternHyperOceanicBarrens          31.6
4 Long_Range_Barrens                  29.9
5 Maritime_Barrens                    26.7
6 North_Shore_Forests                 22.3
7 Northern_Peninsula_Forests          19.9
8 StraitOfBelleIsleBarrens            25.4
9 Western_Forests                     18.9
 View(sap_reg_height)
 sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20) %>% print()
Ecoregion                  AverageHeight
<chr>                              <dbl>
  1 Northern_Peninsula_Forests          19.9
2 Western_Forests                     18.9
 # Ecoregions with average height < 20 (Northern Peninsula Forests, and Western Forests)
 sap_spe_browse <- SaplingsClean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
Species        AverageBrowsing
<chr>                    <dbl>
  1 "Alder "                  4.25
2 "Balsam_Fir"              3.14
3 "Black_Ash"               5   
4 "Black_Spruce"            2.33
5 "White_Birch"             3.14
6 "Willow"                  4.31
 avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing))
 View(avg_browse_spe)
 # Highest browsing: Black Ash
   # Lowest browsing: Black Spruce
   fir_reg_browse <- SaplingsClean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()

Ecoregion                  AverageBrowsing
<chr>                                <dbl>
  1 Avalon_Forests                        2   
2 Central_Forests                       3.5 
3 EasternHyperOceanicBarrens            2   
4 Long_Range_Barrens                    1   
5 Maritime_Barrens                      2   
6 North_Shore_Forests                   4.5 
7 Northern_Peninsula_Forests            4.25
 View(fir_reg_browse)
 barplot( fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Balsam Fir Browsing by Ecoregion", col = "forestgreen", cex.names = 0.6 # reduces label size for readability
           + View(avg_browse_spe)
           Error: unexpected symbol in:
             "t( fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Balsam Fir Browsing by Ecoregion", col = "forestgr
           View"
 View(avg_browse_spe)
 View(fir_reg_browse)
 barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Balsam Fir Browsing by Ecoregion", col = "forestgreen", cex.names = 0.6
+ )
 spruce_reg_browse <- SaplingsClean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()

  Ecoregion                  AverageBrowsing
  <chr>                                <dbl>
1 Avalon_Forests                         0.5
2 Central_Forests                        3  
3 EasternHyperOceanicBarrens             0  
4 Long_Range_Barrens                     2  
5 Maritime_Barrens                       0  
6 North_Shore_Forests                    4  
7 Northern_Peninsula_Forests             4  
8 Western_Forests                        3.5
 View(spruce_reg_browse)
 barplot( spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Black Spruce Browsing by Ecoregion", col = "forestgreen", cex.names = 0.6 )
 # Although both plots are very similar, it is clear that moose prefer balsam fir. 
 sap_reg_tally <- SaplingsClean %>% group_by(Ecoregion) %>% tally() %>% print()

  Ecoregion                      n
  <chr>                      <int>
1 Avalon_Forests                 5
2 Central_Forests                5
3 EasternHyperOceanicBarrens     5
4 Long_Range_Barrens             5
5 Maritime_Barrens               3
6 North_Shore_Forests            8
7 Northern_Peninsula_Forests     7
8 StraitOfBelleIsleBarrens       1
9 Western_Forests                5
 View(sap_reg_tally)
 sap_spe_tally <- SaplingsClean %>% group_by(Species) %>% tally() %>% print()

  Species            n
  <chr>          <int>
1 "Alder "           8
2 "Balsam_Fir"      11
3 "Black_Ash"        1
4 "Black_Spruce"     9
5 "White_Birch"      7
6 "Willow"           8
 View(sap_spe_browse)
 View(sap_spe_tally)
 # The sapling study dataset is not evenly distributed as some species were sampled much more times than others. For example, the balsam fir was tallied 11 times, while the black ash was talied only once. 
 # It's important to recognize sampling bias as it can lead to various misinterpretations in data collected. In this case, a misinterpretation of browsing patterns. 
 moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = MooseCount / Area)
 MooseCount <- read.csv("MoosePopulation.csv")
 View(MooseCount)

 colnames(moose_clean)
[1] "Ecoregion"           "Year"               
[3] "Area"                "Estimated_Moose_Pop"
[5] "MooseDensity"       
 moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
 View(moose_2020b)
 moose_sap <- left_join(moose_2020b, SaplingsClean, by = 'Ecoregion', relationship = "many-to-many")
 View(moose_sap)
 View(sum_spe_browse)
 print(sum_spe_browse)
# Groups:   Species [6]
   Species      Ecoregion      mean_BrowsingScore mean_MooseDensity
   <chr>        <chr>                       <dbl>             <dbl>
 1 "Alder "     Avalon_Forests                4               0.962
 2 "Alder "     Central_Fores…                4.5             2.01 
 3 "Alder "     EasternHyperO…                4               0.5  
 4 "Alder "     Long_Range_Ba…                3.5             0.602
 5 "Alder "     Maritime_Barr…                3.5             0.960
 6 "Alder "     North_Shore_F…                4.5             2.59 
 7 "Alder "     Northern_Peni…                5               2.67 
 8 "Alder "     Western_Fores…                5               2.5  
 9 "Balsam_Fir" Avalon_Forests                2               0.962
10 "Balsam_Fir" Central_Fores…                3.5             2.01 

 print(sum_spe_browse, n = Inf)

# Groups:   Species [6]
   Species        Ecoregion    mean_BrowsingScore mean_MooseDensity
   <chr>          <chr>                     <dbl>             <dbl>
 1 "Alder "       Avalon_Fore…               4                0.962
 2 "Alder "       Central_For…               4.5              2.01 
 3 "Alder "       EasternHype…               4                0.5  
 4 "Alder "       Long_Range_…               3.5              0.602
 5 "Alder "       Maritime_Ba…               3.5              0.960
 6 "Alder "       North_Shore…               4.5              2.59 
 7 "Alder "       Northern_Pe…               5                2.67 
 8 "Alder "       Western_For…               5                2.5  
 9 "Balsam_Fir"   Avalon_Fore…               2                0.962
10 "Balsam_Fir"   Central_For…               3.5              2.01 
11 "Balsam_Fir"   EasternHype…               2                0.5  
12 "Balsam_Fir"   Long_Range_…               1                0.602
13 "Balsam_Fir"   Maritime_Ba…               2                0.960
14 "Balsam_Fir"   North_Shore…               4.5              2.59 
15 "Balsam_Fir"   Northern_Pe…               4.25             2.67 
16 "Black_Ash"    Western_For…               5                2.5  
17 "Black_Spruce" Avalon_Fore…               0.5              0.962
18 "Black_Spruce" Central_For…               3                2.01 
19 "Black_Spruce" EasternHype…               0                0.5  
20 "Black_Spruce" Long_Range_…               2                0.602
21 "Black_Spruce" Maritime_Ba…               0                0.960
22 "Black_Spruce" North_Shore…               4                2.59 
23 "Black_Spruce" Northern_Pe…               4                2.67 
24 "Black_Spruce" Western_For…               3.5              2.5  
25 "White_Birch"  Avalon_Fore…               1.5              0.962
26 "White_Birch"  Central_For…               4                2.01 
27 "White_Birch"  EasternHype…               1.5              0.5  
28 "White_Birch"  Long_Range_…               2.5              0.602
29 "White_Birch"  North_Shore…               4                2.59 
30 "White_Birch"  Northern_Pe…               4.5              2.67 
31 "White_Birch"  Western_For…               4                2.5  
32 "Willow"       Central_For…               5                2.01 
33 "Willow"       EasternHype…               4.5              0.5  
34 "Willow"       Long_Range_…               4                0.602
35 "Willow"       North_Shore…               5                2.59 
36 "Willow"       Northern_Pe…               5                2.67 
37 "Willow"       StraitOfBel…               1                0.01 
38 "Willow"       Western_For…               5                2.5  
 # 23. a) The pattern does indeed fit the hypothesis. At low moose density, browsing intesity greatly varies. At high moose density, browsing scores converge as moose become less selective. 
 # b) Moose prefer the willow the most. Moose dislike the black spruce, and black ash.
 # c) Black ash is missing from the graph as there are no usable browsing observations for the species.
 collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6) 
 human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
 study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
 moose_coll <- data.frame(collisions2020, human_pop, study_sites)
 View(moose_coll)
 moose_coll2 <- moose_coll %>% rename_with(~ "Ecoregion", study_sites)
 View(moose_coll2)
 View(MooseData_2020)
 coll_merge <- left_join(MooseData_2020, moose_coll2, by = "Ecoregion")
 View(coll_merge)
 plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Moose-Vehicle Collisions (2020)", main = "Moose Density vs. Moose-Vehicle Collisions")
 #26. b) There is a correlation with moose density and moose-vehicle collisions. They are directly proportional meaning as moose density increases, so does the amount of collisions.
 coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)
 View(coll_merge_per_capita)
 plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions per Capita", main = "Moose Collisions per Capita vs. Human Population")
 # Yes, this trend makes sense based on what we see in Newfoundland. In rural areas with a lower population of humans, the moose population tends to be elevated, leading to a higher amount of collisions per capita where there are a lower amount of people.
