

R version 4.5.1 (2025-06-13 ucrt) -- "Great Square Root"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> moosedata <- read.csv("MoosePopulation.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") :
  cannot open file 'MoosePopulation.csv': No such file or directory
> moosedata <- read.csv("MoosePopulation.csv")
> View(moosedata)
> moose_clean <- na.omit(moosedata)
> View(moose_clean)
> View(moose_clean)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union

Warning message:
  package ‘dplyr’ was built under R version 4.5.2 
> moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
> View(moose_sel)
> save.image("~/MoosePop/moose.RData")
> Year_min <- min("Year")
> year_min <- min(moose_sel$Year)
> moose_max <- max(moose_sel$Estimated_Moose_Pop)
> moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
> plot(moosedata2$Year, moosedata2$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
> View(moose_clean)
> save.image("~/MoosePop/moose.RData")
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
> plot(moose_west$Year, moose_west$MooseDensity, type = "l", 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland western forests region over time")
> moose_2020 <- filter(moosedata2, Year == "2020")
> moose_2020_high <- filter(moose_2020, MooseDensity == ">2.0")
> View(moose_2020_high)
> View(moose_2020)
> moose_2020_high <- filter(moose_2020, MooseDensity == "above 2.0")
> moose_2020_high <- filter(moose_2020, MooseDensity == "above 2.0000000")
> moose_2020_high <- filter(moose_2020, MooseDensity == "2.0 and above")
> moose_2020_high <- filter(moose_2020, MooseDensity == "2.0")
> moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
> View(moose_2020_high)
> arrange(moose_2020_high, desc(MooseDensity))
Ecoregion Year  Area Estimated_Moose_Pop MooseDensity
1 Northern_Peninsula_Forests 2020 12000               32000     2.666667
2        North_Shore_Forests 2020  8400               21740     2.588095
3            Western_Forests 2020  6800               17000     2.500000
4            Central_Forests 2020 20500               41250     2.012195
> moosefinal <- moosedata2 %>%
  +     filter(Year == 2020) %>%
  +     filter(MooseDensity > 2.0) %>%
  +     arrange(desc(MooseDensity)) %>%
  +     print()
Ecoregion Year  Area Estimated_Moose_Pop MooseDensity
1 Northern_Peninsula_Forests 2020 12000               32000     2.666667
2        North_Shore_Forests 2020  8400               21740     2.588095
3            Western_Forests 2020  6800               17000     2.500000
4            Central_Forests 2020 20500               41250     2.012195
> library(readr)
Warning message:
  package ‘readr’ was built under R version 4.5.2 > SaplingStudy <- read_csv("SaplingStudy.csv")
Rows: 45 Columns: 5                                                                     
── Column specification ──────────────────────────────────────────────────────────────────
Delimiter: ","
chr (3): Ecoregion, Tree_ID, Species
dbl (2): Height, BrowsingScore

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> View(SaplingStudy)
> sap_clean <- na.omit(SaplingStudy)
> sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowse = mean(BrowsingScore)) %>% print()
# A tibble: 9 × 2
Ecoregion                  AverageBrowse
<chr>                              <dbl>
  1 Avalon_Forests                      2   
2 Central_Forests                     4   
3 EasternHyperOceanicBarrens          2.4 
4 Long_Range_Barrens                  2.6 
5 Maritime_Barrens                    1.83
6 North_Shore_Forests                 4.38
7 Northern_Peninsula_Forests          4.57
8 StraitOfBelleIsleBarrens            1   
9 Western_Forests                     4.5 
> avg_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowse)) %>% print()
# A tibble: 9 × 2
Ecoregion                  AverageBrowse
<chr>                              <dbl>
  1 Northern_Peninsula_Forests          4.57
2 Western_Forests                     4.5 
3 North_Shore_Forests                 4.38
4 Central_Forests                     4   
5 Long_Range_Barrens                  2.6 
6 EasternHyperOceanicBarrens          2.4 
7 Avalon_Forests                      2   
8 Maritime_Barrens                    1.83
9 StraitOfBelleIsleBarrens            1   
> #Nothern peninsula forests had the highest average browsing score, whil strait of belle isle barrens had the lowest average browse score
  > save.image("~/MoosePop/moose.RData")
sap_reg_heightg <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height)) %>% print()
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
> save.image("~/MoosePop/moose.RData")


> sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20.00)
> sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20.00) %>% print()
# A tibble: 2 × 2
Ecoregion                  AverageHeight
<chr>                              <dbl>
  1 Northern_Peninsula_Forests          19.9
2 Western_Forests                     18.9
> # northern peninsula forests and western forests are the only Ecoregions to contain saplings with an average height < 20.00cm.
  > View(sap_clean)
> sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsingScore = mean(BrowsingScore)) %>% print()
# A tibble: 6 × 2
Species      AverageBrowsingScore
<chr>                       <dbl>
  1 Alder                        4.25
2 Balsam_Fir                   3.14
3 Black_Ash                    5   
4 Black_Spruce                 2.33
5 White_Birch                  3.14
6 Willow                       4.31
> avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsingScore)) %>% print()
# A tibble: 6 × 2
Species      AverageBrowsingScore
<chr>                       <dbl>
  1 Black_Ash                    5   
2 Willow                       4.31
3 Alder                        4.25
4 White_Birch                  3.14
5 Balsam_Fir                   3.14
6 Black_Spruce                 2.33
> # Black ash has the highest average browsing score whereas black spruce has the lowest average browsing score.
  > Fir_Reg_Browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(MeanBrowsing = mean(BrowsingScore)) %>% print()
# A tibble: 7 × 2
Ecoregion                  MeanBrowsing
<chr>                             <dbl>
  1 Avalon_Forests                     2   
2 Central_Forests                    3.5 
3 EasternHyperOceanicBarrens         2   
4 Long_Range_Barrens                 1   
5 Maritime_Barrens                   2   
6 North_Shore_Forests                4.5 
7 Northern_Peninsula_Forests         4.25
> save.image("~/MoosePop/moose.RData")
> barplot(Fir_Reg_Browse$MeanBrowsing, names.arg = Fir_Reg_Browse$Ecoregion, xlab = "Ecoregion", ylab = "MeanBrowsing", main = "Average moose browsing of Balsam Fir by ecoregion", col = "pink", cex.names = 0.6) 
> barplot(Fir_Reg_Browse$MeanBrowsing, names.arg = Fir_Reg_Browse$Ecoregion, xlab = "Ecoregion", ylab = "MeanBrowsing", main = "Average moose browsing of Balsam Fir by ecoregion", col = "pink", cex.names = 0.4) 
> barplot(Fir_Reg_Browse$MeanBrowsing, names.arg = Fir_Reg_Browse$Ecoregion, xlab = "Ecoregion", ylab = "MeanBrowsing", main = "Average moose browsing of Balsam Fir by ecoregion", col = "pink", cex.names = 0.5) 
> barplot(Fir_Reg_Browse$MeanBrowsing, names.arg = Fir_Reg_Browse$Ecoregion, xlab = "Ecoregion", ylab = "MeanBrowsing", main = "Average moose browsing of Balsam Fir by ecoregion", col = "pink", cex.names = 0.4) 
> Spruce_Reg_Browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(MeanBrowsingSpruce = mean(BrowsingScore)) %>% print()
# A tibble: 8 × 2
Ecoregion                  MeanBrowsingSpruce
<chr>                                   <dbl>
  1 Avalon_Forests                            0.5
2 Central_Forests                           3  
3 EasternHyperOceanicBarrens                0  
4 Long_Range_Barrens                        2  
5 Maritime_Barrens                          0  
6 North_Shore_Forests                       4  
7 Northern_Peninsula_Forests                4  
8 Western_Forests                           3.5
> barplot(Spruce_Reg_Browse$MeanBrowsingSpruce, names.arg = Spruce_Reg_Browse$Ecoregion, xlab = "Ecoregion", ylab = "MeanBrowsingSpruce", main = "average moose browsing of Black Spruce by ecoregion", col = "blue", cex.names = 0.4) 
> barplot(Spruce_Reg_Browse$MeanBrowsingSpruce, names.arg = Spruce_Reg_Browse$Ecoregion, xlab = "Ecoregion", ylab = "MeanBrowsingSpruce", main = "average moose browsing of Black Spruce by ecoregion", col = "blue", cex.names = 0.3) 
> # Balsam Fir browsing has a higher mean browsing score Than Black Spruce in most ecoregions. In select ecoregions Black Spruce has a higher mean browsing score however, overall does not have as high a Mean browsing score as Balsam Fir.
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
> View(sap_reg_tally)
> save.image("~/MoosePop/moose.RData")
> sap_spe_tally<- sap_clean %>%
  +     group_by(Species) %>%
  +     tally() %>% 
  +     print()
# A tibble: 6 × 2
Species          n
<chr>        <int>
  1 Alder            8
2 Balsam_Fir      11
3 Black_Ash        1
4 Black_Spruce     9
5 White_Birch      7
6 Willow           8
> #I think that the dataset SaplingStudy is unevenly distributed with a overespresentation of the ecoregions "Northern peninsula forrests" and "North shore forests", and an underepresentation of the ecoregions "strait of belle isle barrens" and "maritime barrens". I also think there is an overrepresentation of the species "Balsam fir", and an underepresentation of the species "Black ash". 
  > # recognizing bias in ecological datasets is important because bias can produce a skewed image of the real ecological patterns and behaviours. Biased data can result in conclusions that are incorrect such as innaccurate moose browsing data due to the high volume of a certain sapling or ecoregion sampled. 
  
  > moose_2020b <- mutate(moose_2020, MooseDensity = Estimated_Moose_Pop / Area)
> moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
> sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize((mean(BrowsingScore)), mean(MooseDensity)) %>% print()
`summarise()` has regrouped the output.
ℹ Summaries were computed grouped by Species and Ecoregion.
ℹ Output is grouped by Species.
ℹ Use `summarise(.groups = "drop_last")` to silence this message.
ℹ Use `summarise(.by = c(Species, Ecoregion))` for per-operation grouping instead.
# A tibble: 38 × 4
# Groups:   Species [6]
Species    Ecoregion                  `(mean(BrowsingScore))` `mean(MooseDensity)`
<chr>      <chr>                                        <dbl>                <dbl>
  1 Alder      Avalon_Forests                                 4                  0.962
2 Alder      Central_Forests                                4.5                2.01 
3 Alder      EasternHyperOceanicBarrens                     4                  0.5  
4 Alder      Long_Range_Barrens                             3.5                0.602
5 Alder      Maritime_Barrens                               3.5                0.960
6 Alder      North_Shore_Forests                            4.5                2.59 
7 Alder      Northern_Peninsula_Forests                     5                  2.67 
8 Alder      Western_Forests                                5                  2.5  
9 Balsam_Fir Avalon_Forests                                 2                  0.962
10 Balsam_Fir Central_Forests                                3.5                2.01 
# ℹ 28 more rows
# ℹ Use `print(n = ...)` to see more rows

> save.image("~/MoosePop/moose.RData")

> collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

> human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)

> study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

> moose_coll <- data.frame(collisions2020, human_pop, study_sites)

> moose_coll2 <- moose_coll %>% rename_with(~ "Ecoregion", study_sites)

> coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")


> 
  
  
  
  
  