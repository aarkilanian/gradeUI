> # Importing Data -----------------------
> MoosePopulation <- read.csv("C:/Users/KatieTurpin/Downloads/MoosePopulation.csv")
> SaplingStudy <- read.csv("C:/Users/KatieTurpin/Downloads/SaplingStudy.csv")
> #
> # Title: Bio 1002 R assignment -------
> # Author: Katie Turpin
> # Date: 2026-02-13
> #    
> # Set working directory ---------------
> setwd("/Users/KatieTurpin/Documents/R_Bio_1002")
> #
> # QUESTION 1 ==========================
> # Installing dplyr --------------------
> install.packages("dplyr")
> # 
> # Loading dplyr -----------------------
> library(dplyr)
> #
> # QUESTION 2 ==========================
> # Naming data -------------------------
> moosedata <- read.csv("C:/Users/KatieTurpin/Downloads/MoosePopulation.csv")
> #
> # QUESTION 3 ==========================
> # Analyzing and omitting data ---------
> View(moosedata)
> na.omit(moosedata)
Ecoregion Year  Area
1         North_Shore_Forests 2020  8400
2  Northern_Peninsula_Forests 2020 12000
3          Long_Range_Barrens 2020  8800
4             Central_Forests 2020 20500
5             Western_Forests 2020  6800
6  EasternHyperOceanicBarrens 2020  4400
7            Maritime_Barrens 2020 21300
8              Avalon_Forests 2020  4200
9    StraitOfBelleIsleBarrens 2020   500
10        North_Shore_Forests 1992  8400
11 Northern_Peninsula_Forests 1992 12000
12         Long_Range_Barrens 1992  8800
13            Central_Forests 1992 20500
14            Western_Forests 1992  6800
15 EasternHyperOceanicBarrens 1992  4400
16           Maritime_Barrens 1992 21300
17             Avalon_Forests 1992  6400
19        North_Shore_Forests 1980  8400
20 Northern_Peninsula_Forests 1980 12000
21         Long_Range_Barrens 1980  8800
22            Central_Forests 1980 20500
23            Western_Forests 1980  6800
24 EasternHyperOceanicBarrens 1980  4400
25           Maritime_Barrens 1980 21300
26             Avalon_Forests 1980  6400
28        North_Shore_Forests 1960  8400
29 Northern_Peninsula_Forests 1960 12000
30         Long_Range_Barrens 1960  8800
31            Central_Forests 1960 20500
32            Western_Forests 1960  6800
34           Maritime_Barrens 1960 21300
35             Avalon_Forests 1960  4200
37        North_Shore_Forests 1940  8400
38 Northern_Peninsula_Forests 1940 12000
40            Central_Forests 1940 20500
41            Western_Forests 1940  6800
50            Western_Forests 1904  6800
Estimated_Moose_Pop Observation_Method
1                21740      Aerial_Survey
2                32000      Aerial_Survey
3                 5300        Camera_Trap
4                41250      Aerial_Survey
5                17000   Expert_Estimate 
6                 2200   Expert_Estimate 
7                20450   Expert_Estimate 
8                 4040      Aerial_Survey
9                    5        Camera_Trap
10               14000      Aerial_Survey
11               21000      Aerial_Survey
12                1540   Expert_Estimate 
13               27600      Aerial_Survey
14               11650      Aerial_Survey
15                1400        Camera_Trap
16               14500      Aerial_Survey
17                5320   Expert_Estimate 
19                7400      Aerial_Survey
20               15700   Expert_Estimate 
21                 400        Camera_Trap
22               13200      Aerial_Survey
23                3500      Aerial_Survey
24                 600   Expert_Estimate 
25                8300     Hunter_Reports
26                 200        Camera_Trap
28               10600      Aerial_Survey
29               18000      Aerial_Survey
30                1300     Hunter_Reports
31               17210      Aerial_Survey
32               14500   Expert_Estimate 
34               10300      Aerial_Survey
35                 200   Expert_Estimate 
37                3500   Expert_Estimate 
38                9600   Expert_Estimate 
40               11200   Expert_Estimate 
41                2300   Expert_Estimate 
50                   4              Count
Data_Source
1   GovOfNewfoundland
2      NGO_Monitoring
3  MemorialUniversity
4   GovOfNewfoundland
5         ParksCanada
6  MemorialUniversity
7  MemorialUniversity
8  MemorialUniversity
9         ParksCanada
10  GovOfNewfoundland
11     NGO_Monitoring
12 MemorialUniversity
13  GovOfNewfoundland
14  GovOfNewfoundland
15        ParksCanada
16  GovOfNewfoundland
17 MemorialUniversity
19  GovOfNewfoundland
20 MemorialUniversity
21        ParksCanada
22  GovOfNewfoundland
23  GovOfNewfoundland
24 MemorialUniversity
25  GovOfNewfoundland
26        ParksCanada
28  GovOfNewfoundland
29  GovOfNewfoundland
30  GovOfNewfoundland
31  GovOfNewfoundland
32        ParksCanada
34  GovOfNewfoundland
35        ParksCanada
37  GovOfNewfoundland
38  GovOfNewfoundland
40        ParksCanada
41        ParksCanada
50  GovOfNewfoundland
> moose_clean <- na.omit(moosedata)
> #
> View(moose_clean)
> #
> # QUESTION 4 ==========================
> select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
1         North_Shore_Forests 2020  8400
2  Northern_Peninsula_Forests 2020 12000
3          Long_Range_Barrens 2020  8800
4             Central_Forests 2020 20500
5             Western_Forests 2020  6800
6  EasternHyperOceanicBarrens 2020  4400
7            Maritime_Barrens 2020 21300
8              Avalon_Forests 2020  4200
9    StraitOfBelleIsleBarrens 2020   500
10        North_Shore_Forests 1992  8400
11 Northern_Peninsula_Forests 1992 12000
12         Long_Range_Barrens 1992  8800
13            Central_Forests 1992 20500
14            Western_Forests 1992  6800
15 EasternHyperOceanicBarrens 1992  4400
16           Maritime_Barrens 1992 21300
17             Avalon_Forests 1992  6400
19        North_Shore_Forests 1980  8400
20 Northern_Peninsula_Forests 1980 12000
21         Long_Range_Barrens 1980  8800
22            Central_Forests 1980 20500
23            Western_Forests 1980  6800
24 EasternHyperOceanicBarrens 1980  4400
25           Maritime_Barrens 1980 21300
26             Avalon_Forests 1980  6400
28        North_Shore_Forests 1960  8400
29 Northern_Peninsula_Forests 1960 12000
30         Long_Range_Barrens 1960  8800
31            Central_Forests 1960 20500
32            Western_Forests 1960  6800
34           Maritime_Barrens 1960 21300
35             Avalon_Forests 1960  4200
37        North_Shore_Forests 1940  8400
38 Northern_Peninsula_Forests 1940 12000
40            Central_Forests 1940 20500
41            Western_Forests 1940  6800
50            Western_Forests 1904  6800
Estimated_Moose_Pop
1                21740
2                32000
3                 5300
4                41250
5                17000
6                 2200
7                20450
8                 4040
9                    5
10               14000
11               21000
12                1540
13               27600
14               11650
15                1400
16               14500
17                5320
19                7400
20               15700
21                 400
22               13200
23                3500
24                 600
25                8300
26                 200
28               10600
29               18000
30                1300
31               17210
32               14500
34               10300
35                 200
37                3500
38                9600
40               11200
41                2300
50                   4
> moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
> #
> View(moose_sel)
> #
> # QUESTION 5 (a&b) ====================
> # Observations in dataset -------------
> min(moose_sel$Year)
[1] 1904
> year_min <- min(moose_sel$Year)
> # 
> max(moose_sel$Estimated_Moose_Pop)
[1] 41250
> moose_max <- max(moose_sel$Estimated_Moose_Pop)
> #
> # QUESTION 6 ==========================
> # Calculating moose density -----------
> mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
> moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
Ecoregion Year  Area
1         North_Shore_Forests 2020  8400
2  Northern_Peninsula_Forests 2020 12000
3          Long_Range_Barrens 2020  8800
4             Central_Forests 2020 20500
5             Western_Forests 2020  6800
6  EasternHyperOceanicBarrens 2020  4400
7            Maritime_Barrens 2020 21300
8              Avalon_Forests 2020  4200
9    StraitOfBelleIsleBarrens 2020   500
10        North_Shore_Forests 1992  8400
11 Northern_Peninsula_Forests 1992 12000
12         Long_Range_Barrens 1992  8800
13            Central_Forests 1992 20500
14            Western_Forests 1992  6800
15 EasternHyperOceanicBarrens 1992  4400
16           Maritime_Barrens 1992 21300
17             Avalon_Forests 1992  6400
19        North_Shore_Forests 1980  8400
20 Northern_Peninsula_Forests 1980 12000
21         Long_Range_Barrens 1980  8800
22            Central_Forests 1980 20500
23            Western_Forests 1980  6800
24 EasternHyperOceanicBarrens 1980  4400
25           Maritime_Barrens 1980 21300
26             Avalon_Forests 1980  6400
28        North_Shore_Forests 1960  8400
29 Northern_Peninsula_Forests 1960 12000
30         Long_Range_Barrens 1960  8800
31            Central_Forests 1960 20500
32            Western_Forests 1960  6800
34           Maritime_Barrens 1960 21300
35             Avalon_Forests 1960  4200
37        North_Shore_Forests 1940  8400
38 Northern_Peninsula_Forests 1940 12000
40            Central_Forests 1940 20500
41            Western_Forests 1940  6800
50            Western_Forests 1904  6800
Estimated_Moose_Pop MooseDensity
1                21740 2.5880952381
2                32000 2.6666666667
3                 5300 0.6022727273
4                41250 2.0121951220
5                17000 2.5000000000
6                 2200 0.5000000000
7                20450 0.9600938967
8                 4040 0.9619047619
9                    5 0.0100000000
10               14000 1.6666666667
11               21000 1.7500000000
12                1540 0.1750000000
13               27600 1.3463414634
14               11650 1.7132352941
15                1400 0.3181818182
16               14500 0.6807511737
17                5320 0.8312500000
19                7400 0.8809523810
20               15700 1.3083333333
21                 400 0.0454545455
22               13200 0.6439024390
23                3500 0.5147058824
24                 600 0.1363636364
25                8300 0.3896713615
26                 200 0.0312500000
28               10600 1.2619047619
29               18000 1.5000000000
30                1300 0.1477272727
31               17210 0.8395121951
32               14500 2.1323529412
34               10300 0.4835680751
35                 200 0.0476190476
37                3500 0.4166666667
38                9600 0.8000000000
40               11200 0.5463414634
41                2300 0.3382352941
50                   4 0.0005882353
> #
> View(moosedata2)
>#
> # QUESTION 7 ==========================
>plot(moosedata2$Year, moosedata2$MooseDensity, 
      +      xlab = "year", 
      +      ylab = "Moose per sq km", 
      +      main = "Moose density in Newfoundland ecoregions over time")
> #
> # QUESTION 8 ==========================
> # Western forests data-----------------
> filter(moosedata2, Ecoregion == "Western_Forests")
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
> #
> # Showing change in moose density------
> plot(moose_west$Year, moose_west$MooseDensity, 
       +      type = "l",
       +      xlab = "Year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Western Forests over time")
> #
> # QUESTION 9 ==========================
> # Saving 2020 dataset -----------------
> filter(moosedata2, Year == 2020)
> moose_2020 <- filter(moosedata2, Year == 2020)
> #
> # High densities in 2020 dataset-------
> filter(moose_2020, MooseDensity > 2.0)
> moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
> #
> # Arranging data ----------------------
> arrange(moose_2020_high, desc(MooseDensity))
> moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
> #
> # QUESTION 10 =========================
> # Using pipes -------------------------
> #
  > moosedata2 %>%
  +     filter(Year == 2020) %>%
  +     filter(MooseDensity > 2.0) %>%
  +     arrange(desc(MooseDensity)) %>%
  +     print()
> moosefinal <- moosedata2 %>%
  +     filter(Year == 2020) %>%
  +     filter(MooseDensity > 2.0) %>%
  +     arrange(desc(MooseDensity)) %>%
  +     print()
> #
> # QUESTION 11 ==========================
> # Naming Data file -------------------- 
> saplings <- read.csv("C:/Users/KatieTurpin/Downloads/SaplingStudy.csv")
> #
> # Omitting data ------------------------
> View(saplings)
> na.omit(saplings)
> sap_clean <- na.omit(saplings)
> #
> # QUESTION 12 ==========================
> # Moose browsing variation -------------
> sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
sap_reg_browse <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  +     print()
# A tibble: 9 × 2
Ecoregion                  AverageBrowsing
<chr>                                <dbl>
1 Avalon_Forests                        2   
2 Central_Forests                       4   
3 EasternHyperOceanicBarrens            2.4 
4 Long_Range_Barrens                    2.6 
5 Maritime_Barrens                      1.83
6 North_Shore_Forests                   4.38
7 Northern_Peninsula_Forests            4.57
8 StraitOfBelleIsleBarrens              1   
9 Western_Forests                       4.5 
> # 
> # Hghest/Lowest browsing----------------
> sap_reg_browse %>%
  +     arrange(AverageBrowsing)
> sap_reg_browse %>%
  +     arrange(-(AverageBrowsing))
                # A tibble: 9 × 2
  Ecoregion                  AverageBrowsing
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
> avg_browse_reg <- sap_reg_browse %>%
  +     arrange(-(AverageBrowsing)
> View(avg_browse_reg)
> # Northern_Peninsula_Forests:Highest, StraitOfBelleIsleBarrens:Lowest
> # 
> # QUESTION 13 ==========================
> Tree height variation-------------------
> sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageHeight = mean(Height)) %>%
  +     print()
# A tibble: 9 × 2
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
> sap_reg_height <- sap_clean %>%
  +   group_by(Ecoregion) %>%
  +   summarize(AverageHeight = mean(Height)) %>%
> #
> # Lowest average heights----------------
> sap_reg_height %>%
  +     filter(AverageHeight < 20) %>%
  +     print()
# A tibble: 2 × 2
Ecoregion                  AverageHeight
<chr>                              <dbl>
1 Northern_Peninsula_Forests          19.9
2 Western_Forests                     18.9
> sap_reg_height %>%
  +     filter(AverageHeight < 20) %>%
  +     print()
> # Northern_Peninsula_Forests and Western_Forests are < 20 cm
> #
> # QUESTION 14 ==========================
> sap_clean %>%
  +     group_by(Species) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  +     print()
# A tibble: 6 × 2
Species        AverageBrowsing
<chr>                    <dbl>
1 "Alder "                  4.25
2 "Balsam_Fir"              3.14
3 "Black_Ash"               5   
4 "Black_Spruce"            2.33
5 "White_Birch"             3.14
6 "Willow"                  4.31
> sap_spe_browse <- sap_clean %>%
  +     group_by(Species) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  +     print()  
> #
> # Highest/Lowest Browsing --------------
> arrange(sap_spe_browse, desc(AverageBrowsing))
# A tibble: 6 × 2
Species        AverageBrowsing
<chr>                    <dbl>
1 "Black_Ash"               5   
2 "Willow"                  4.31
3 "Alder "                  4.25
4 "White_Birch"             3.14
5 "Balsam_Fir"              3.14
6 "Black_Spruce"            2.33 
> avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
> #
> # Black_ash:Highest Black_spruce:Lowest
> #
> # QUESTION 15 ==========================  
> # Average browsing variation------------
> sap_clean %>%
  +     filter(Species == "Balsam_Fir") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore))
# A tibble: 7 × 2
Ecoregion                  AverageBrowsing
<chr>                                <dbl>
1 Avalon_Forests                        2   
2 Central_Forests                       3.5 
3 EasternHyperOceanicBarrens            2   
4 Long_Range_Barrens                    1   
5 Maritime_Barrens                      2   
6 North_Shore_Forests                   4.5 
7 Northern_Peninsula_Forests            4.25
> fir_reg_browse <- sap_clean %>%
  +     filter(Species == "Balsam_Fir") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore))
> #
> # QUESTION 16 ==========================
> # Making a barplot----------------------
> barplot(fir_reg_browse$AverageBrowsing, 
          +         names.arg = fir_reg_browse$Ecoregion, 
          +         xlab = "Ecoregion", 
          +         ylab = "Avg Browsing Score", 
          +         main = "Balsam Fir Browsing Intensity by Ecoregion", 
          +         col = "forestgreen", 
          +         cex.names = 0.6)
> #
> # QUESTION 17 ==========================
> # Spruce browsing ----------------------
> spruce_reg_browse <- sap_clean %>%
  +     filter(Species == "Black_Spruce") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore))
> #
> # Spruce barplot------------------------
> barplot(spruce_reg_browse$AverageBrowsing, 
          +         names.arg = spruce_reg_browse$Ecoregion, 
          +         xlab = "Ecoregion", 
          +         ylab = "Avg Browsing Score", 
          +         main = "Black Spruce Browsing Intensity by Ecoregion", 
          +         col = "steelblue", 
          +         cex.names = 0.6)
> #
> # Comparison----------------------------
> # Balsam fir had overall higher browsing rates. However Black Spruce had higher browsing rates in Long_Range_Barrens. 
> #
> # QUESTION 18 ========================== 
> # Tally --------------------------------
> sap_reg_tally<- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     tally() %>% 
  +     print()
# A tibble: 9 × 2
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
> #
> # QUESTION 19 ========================== 
> sap_spe_tally <- sap_clean %>%
  +     group_by(Species) %>%
  +     tally() %>%
  +     print()
# A tibble: 6 × 2
Species            n
<chr>          <int>
1 "Alder "           8
2 "Balsam_Fir"      11
3 "Black_Ash"        1
4 "Black_Spruce"     9
5 "White_Birch"      7
6 "Willow"           8
> #
> # QUESTION 20 ==========================
> # distribution -------------------------
> # No they were not as the two had different numbers of individuals sampled.
> # it is important to recognize bias in datasets because even a small bias can impact averages and information gathered greatly.
> #
> # QUESTION 21 ==========================
moose_sap <- left_join(moose_2020, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
> moose_2020b <- moose_clean %>%
  +     filter(Year == 2020) %>%
  +     mutate(MooseDensity = Estimated_Moose_Pop / Area)
> moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
> #
> # QUESTION 22 ==========================
> sum_spe_browse <- moose_sap %>%
  +     group_by(Species, Ecoregion) %>%
  +     summarize(AvgBrowsing = mean(BrowsingScore),
                  +               AvgDensity = mean(MooseDensity)) %>%
  +     print()
`summarise()` has regrouped the output.
ℹ Summaries were computed grouped by Species
and Ecoregion.
ℹ Output is grouped by Species.
ℹ Use `summarise(.groups = "drop_last")` to
silence this message.
ℹ Use `summarise(.by = c(Species, Ecoregion))`
for per-operation grouping instead.
# A tibble: 38 × 4
# Groups:   Species [6]
Species      Ecoregion AvgBrowsing AvgDensity
<chr>        <chr>           <dbl>      <dbl>
1 "Alder "     Avalon_F…         4        0.962
2 "Alder "     Central_…         4.5      2.01 
3 "Alder "     EasternH…         4        0.5  
4 "Alder "     Long_Ran…         3.5      0.602
5 "Alder "     Maritime…         3.5      0.960
6 "Alder "     North_Sh…         4.5      2.59 
7 "Alder "     Northern…         5        2.67 
8 "Alder "     Western_…         5        2.5  
9 "Balsam_Fir" Avalon_F…         2        0.962
10 "Balsam_Fir" Central_…         3.5      2.01 
# ℹ 28 more rows
# ℹ Use `print(n = ...)` to see more rows
> #
> # QUESTION 23 ==========================
> #Hypothesis Evidence: Yes, there is evidence. At lower densities, browsing scores vary wildly between species (selective), while at higher densities, the scores for all species tend to cluster higher and more closely together.
> #Preferences: Balsam Fir typically shows the highest browsing scores (most favored), while Black Spruce often shows the lowest (least favored).
> #Missing Species: Black Ash is often missing from these figures because it had a sample size of only 1, making it impossible to calculate a meaningful average for density correlation.
> #
> # QUESTION 24 ==========================
> collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
> human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
> study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
> #
> # QUESTION 25 ==========================
> moose_coll2 <- moose_coll %>% 
  +     rename(Ecoregion = study_sites)
> coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
> # 
> # QUESTION 26 ==========================
> plot(coll_merge$MooseDensity, coll_merge$collisions2020,
         +      xlab = "Moose Density", ylab = "Collisions (2020)",
         +      main = "Moose Density vs. Vehicle Collisions")
> # Yes there is an outlier where the moose density is 1 and the # of collisions is over 100. Other then that as moose density increases collisions increase.
> #
> # QUESTION 27 ==========================
> coll_merge_per_capita <- coll_merge %>%
  +     mutate(coll_per_capita = collisions2020 / human_pop)
> #
> # QUESTION 28 ==========================
> plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
       +      xlab = "Human Population", 
       +      ylab = "Collisions Per Person",
       +      main = "Human Population vs. Per Capita Collisions",
       +      pch = 16, 
       +      col = "darkred")
> #
> # QUESTION 29 ==========================
> # As the human population increases, collisions per person decreases. This makes sense because as populations increase we move towards more urbanized areas with less wildlife.


















