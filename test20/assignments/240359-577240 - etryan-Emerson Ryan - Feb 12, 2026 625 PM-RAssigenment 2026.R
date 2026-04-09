"R Assignment - BIOL 1002
 13/02/2026
 Emerson Ryan, 202511125"

#Question 1
library(dplyr)

#Question 2
moosedata <- read.csv("C:/Users/Emers/OneDrive/MUN/Semester 2/R-assignment 1002/MoosePopulation.csv")

#Question 3
moose_clean <- na.omit(moosedata)

#Question 4
Moose_sel <- select(moose_clean, 
                    Ecoregion, 
                    Year, 
                    Area, 
                    Estimated_Moose_Pop)

#Question 5
#a. 
year_min <- min(Moose_sel$Year)

#b.
year_max <- max(Moose_sel$Year)

#Question 6
moosedata2 <- mutate(Moose_sel, 
                     MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(moosedata2$Year, 
     moosedata2$MooseDensity, 
     type = "l", 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8
#a. 
moose_west <- filter(moosedata2, 
                     Ecoregion == "Western_Forests")

#b.
plot(moose_west$Year, 
     moose_west$MooseDensity, 
     type = "l", 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in western forests over time")

#Question 9
#a.
moose_2020 <- filter(moosedata2, 
                     Year == "2020")

#b.
moose_2020_high <- filter(moose_2020, 
                          MooseDensity >= 2.0)

#c. 
moose_2020_high_byD <- arrange(moose_2020_high, 
                               desc(MooseDensity))

#Question 10
moosefinal <- moosedata2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()
"                   Ecoregion Year  Area Estimated_Moose_Pop MooseDensity
1 Northern_Peninsula_Forests 2020 12000               32000     2.666667
2        North_Shore_Forests 2020  8400               21740     2.588095
3            Western_Forests 2020  6800               17000     2.500000
4            Central_Forests 2020 20500               41250     2.012195"

#Question 11
#a. 
saplings <- read.csv("C:/Users/Emers/OneDrive/MUN/Semester 2/R-assignment 1002/SaplingStudy.csv")

#b.
sap_clean <- na.omit(saplings)

#Question 12 
#a.
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarise(AverageBrowsing = mean(BrowsingScore)) %>% print()
" Ecoregion                  `AverageBrowse`
  <chr>                                      <dbl>
1 Avalon_Forests                              2   
2 Central_Forests                             4   
3 EasternHyperOceanicBarrens                  2.4 
4 Long_Range_Barrens                          2.6 
5 Maritime_Barrens                            1.83
6 North_Shore_Forests                         4.38
7 Northern_Peninsula_Forests                  4.57
8 StraitOfBelleIsleBarrens                    1   
9 Western_Forests                             4.5 "

#b. 
ave_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowsing)) %>% print()
"Ecoregion                  AverageBrowsing
  <chr>                                <dbl>
1 Northern_Peninsula_Forests            4.57
2 Western_Forests                       4.5 
3 North_Shore_Forests                   4.38
4 Central_Forests                       4   
5 Long_Range_Barrens                    2.6 
6 EasternHyperOceanicBarrens            2.4 
7 Avalon_Forests                        2   
8 Maritime_Barrens                      1.83
9 StraitOfBelleIsleBarrens              1   

Northeren_Peninsula_Forests, highest average browsing score
StraitofBellesIsLeBarrens, lowest average browsing score"

#Question13
#a.
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarise(RegHeight = mean(BrowsingScore)) %>% print()
"Ecoregion                  RegHeight
<chr>                          <dbl>
  1 Avalon_Forests                  2   
2 Central_Forests                 4   
3 EasternHyperOceanicBarrens      2.4 
4 Long_Range_Barrens              2.6 
5 Maritime_Barrens                1.83
6 North_Shore_Forests             4.38
7 Northern_Peninsula_Forests      4.57
8 StraitOfBelleIsleBarrens        1   
9 Western_Forests                 4.5"

#b.
sap_reg_height_low <- filter(sap_reg_height, RegHeight <= 20) %>% print()
"A tibble: 2 × 2
  Ecoregion                  RegHeight
  <chr>                          <dbl>
1 Northern_Peninsula_Forests      19.9
2 Western_Forests                 18.9

Western_Forests, lowewst avereage height"

#Question 14
#a. 
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarise(AverageBrowsingSpe = mean(BrowsingScore)) %>% print()
"Species        AverageBrowsingSpe
  <chr>                       <dbl>
1 Alder                      4.25
2 Balsam_Fir                 3.14
3 Black_Ash                  5   
4 Black_Spruce               2.33
5 White_Birch                3.14
6 Willow                     4.31"

#b.
ave_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsingSpe)) %>% print()
"Species        AverageBrowsingSpe
  <chr>                       <dbl>
1 Black_Ash                  5   
2 Willow                     4.31
3 Alder                      4.25
4 White_Birch                3.14
5 Balsam_Fir                 3.14
6 Black_Spruce               2.33

Black_Ash, hightest browsing
Black_Spruce, lowest browsing"

#Question 15
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarise(FirBrowsing = mean(BrowsingScore)) %>% print()
"Ecoregion                  FirBrowsing
  <chr>                            <dbl>
1 Avalon_Forests                    2   
2 Central_Forests                   3.5 
3 EasternHyperOceanicBarrens        2   
4 Long_Range_Barrens                1   
5 Maritime_Barrens                  2   
6 North_Shore_Forests               4.5 
7 Northern_Peninsula_Forests        4.25"

#Question 16
barplot(fir_reg_browse$FirBrowsing, 
        names.arg = fir_reg_browse$Ecoregion, 
        Xlab = "Ecoregion", 
        ylab = "Balsam Fir Browsing Score", 
        main = "Balsam Fir Browsing Score vs Ecoregion", 
        col = "yellow", 
        cex.names = 0.6)

#Question 17
#a.
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarise(SpruceBrowsing = mean(BrowsingScore)) %>% print()
"Ecoregion                  SpruceBrowsing
  <chr>                               <dbl>
1 Avalon_Forests                        0.5
2 Central_Forests                       3  
3 EasternHyperOceanicBarrens            0  
4 Long_Range_Barrens                    2  
5 Maritime_Barrens                      0  
6 North_Shore_Forests                   4  
7 Northern_Peninsula_Forests            4  
8 Western_Forests                       3.5"

#b.
barplot(spruce_reg_browse$SpruceBrowsing, 
        names.arg = spruce_reg_browse$Ecoregion, 
        Xlab = "Ecoregion", 
        ylab = "Black Spruce Browsing Score", 
        main = "Black Spruce Browsing Score vs Ecoregion", 
        col = "pink", 
        cex.names = 0.6)

#c. 
"Both the black spruce pop and baslam fir pop have similar browsing scores in
central forest, Northern Peninsula forest, and the western forest ecoregions. 
The black spruce pop has significantly lower browsing scores in the maritime barrens,
eastern hyper oceanic barrens, and avalon forest ecoregions then the balsam fir pop."

#Question 18
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()
" Ecoregion                      n
  <chr>                      <int>
1 Avalon_Forests                 5
2 Central_Forests                5
3 EasternHyperOceanicBarrens     5
4 Long_Range_Barrens             5
5 Maritime_Barrens               3
6 North_Shore_Forests            8
7 Northern_Peninsula_Forests     7
8 StraitOfBelleIsleBarrens       1
9 Western_Forests                5"

#Question 19
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()
"Species            n
  <chr>          <int>
1 Alder            8
2 Balsam_Fir      11
3 Black_Ash        1
4 Black_Spruce     9
5 White_Birch      7
6 Willow           8"

#Question 20 
#a.
"In this study the balsam fir is overrepresented compared to the rest of the trees 
observed in the study and the black ash is underrepresented with only one sample present.
Majority of the Ecoregions are fairly represented. However, northshore forests are
slightly overrepersented, while stright of belle isle barrens is fairly underrepresented. "

#b.
"Its important to reconize bias in ecological datasets because they could represent
inaccurate data. For example, the researchers may not have had access to many black ash trees,
making it harder to conduct reserch on this variable."

#Question 21
#a.
moose_2020b <- mutate(moose_2020,
                      MooseDensity = Estimated_Moose_Pop / Area)

#b. 
moose_sap <- left_join(moose_2020b, 
                       sap_clean, 
                       by = 'Ecoregion', 
                       relationship = "many-to-many")

#Question 22
sum_spe_browse <- moose_sap %>% group_by(Ecoregion, 
                                         Species) %>% summarise(AverageBrowsing = mean(BrowsingScore), 
                                                                AverageDensity = mean(MooseDensity)) %>% print()
"Ecoregion                  Species        AverageBrowsing AverageDensity
<chr>                      <chr>                    <dbl>          <dbl>
  1 Avalon_Forests             Alder                    4            0.962
2 Avalon_Forests             Balsam_Fir               2            0.962
3 Avalon_Forests             Black_Spruce             0.5          0.962
4 Avalon_Forests             White_Birch              1.5          0.962
5 Central_Forests            Alder                    4.5          2.01 
6 Central_Forests            Balsam_Fir               3.5          2.01 
7 Central_Forests            Black_Spruce             3            2.01 
8 Central_Forests            White_Birch              4            2.01 
9 Central_Forests            Willow                   5            2.01 
10 EasternHyperOceanicBarrens Alder                    4            0.5"

#Question 23
#a. 
"Based on the graph the hypothosis appers to be true. At lower moose density, the
moose perfer specific trees over others, while at high density, the moose take what they 
can get."

#b.
"Based on the graph the moose appear to favor willow sapplings the most. The black ash 
sapplings have no browsing score. However only one black ash sappling was observed, meaning 
theres not enough data to conclusively determine if the moose dislike it or not.
The moose least favorite sappling appears to be the black spruce, even in low density areas
the moose perfer the other sapplings over that one."

#c. 
"Theres not enough data on the black ash to conclusively determine whether or not
the moose enjoy the sappling or not."

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25
#a. 
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)

#b.
coll_merge <- left_join(moose_coll2, 
                        moose_2020, 
                        by = 'Ecoregion', 
                        relationship = "many-to-many")

#Question 26
#a.
plot(coll_merge$MooseDensity, 
     coll_merge$collisions2020, 
     pch = 19, xlab = "MooseDensity", 
     ylab = "Collisions2020", 
     main = "Moose Density v.s. Collisions in 2020")

#b. 
"A trend observed in the graph is that as moose density rises, so do the number of
collisons in 2020. An outlier in this data is when moose density is at 1.0 there are
a significant increase in collisions."

#Question 27
coll_per_capita <- mutate(coll_merge, 
                          coll_merge_per_capita = collisions2020 / human_pop)

#Question 28
plot(coll_per_capita$coll_merge_per_capita, 
     coll_per_capita$human_pop, 
     pch = 19, xlab = "Collisons per capita", 
     ylab = "Human pop", 
     main = "Collisions per capita vs human pop")

#Question 29
"A trend seen in the graph is that the denser the human population the less colisions there are
with moose. This makes sense in an area like St.Jonhs where theres more urban areas. In less populated areas
(more rural), there are more collisons per capita, which makes sense since rural 
areas have more densely populated forests."
