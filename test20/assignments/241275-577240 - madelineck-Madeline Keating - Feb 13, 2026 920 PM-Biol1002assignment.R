
R version 4.5.1 (2025-06-13) -- "Great Square Root"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: aarch64-apple-darwin20

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

> setwd("~/Documents/bio 1002 R")
> install.packages("dplyr")
trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.5/dplyr_1.2.0.tgz'
Content type 'application/x-gzip' length 1639723 bytes (1.6 MB)
==================================================
  downloaded 1.6 MB


The downloaded binary packages are in
/var/folders/7p/13yw9lfd7fj4hlnclflw8_vr0000gn/T//Rtmpdf4wfZ/downloaded_packages
> # Title: Biology 1002 R assignment
  > # Author: Madeline Keating
  > # Date: 02-07-2026
  > 
  > # Load libraries needed ---------------------------
> library(“dplyr”)
Error: unexpected input in "library(“"

> # Title: Biology 1002 R assignment
  > # Author: Madeline Keating
  > # Date: 02-07-2026
  > 
  > # Load libraries needed ---------------------------
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union

Warning message:
  package ‘dplyr’ was built under R version 4.5.2 
> 
  > library(readr)
Warning message:
  package ‘readr’ was built under R version 4.5.2 
> MoosePopulation <- read_csv("~/Downloads/MoosePopulation.csv")
Rows: 54 Columns: 6                                       
── Column specification ────────────────────────────────────
Delimiter: ","
chr (3): Ecoregion, Observation_Method, Data_Source
dbl (3): Year, Area, Estimated_Moose_Pop

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> View(MoosePopulation)
> moosedata <- read.csv("MoosePopulation")

> library(readr)
> moosedata <- read_csv("~/Downloads/MoosePopulation.csv")
Rows: 54 Columns: 6                                       
── Column specification ────────────────────────────────────
Delimiter: ","
chr (3): Ecoregion, Observation_Method, Data_Source
dbl (3): Year, Area, Estimated_Moose_Pop

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> View(moosedata)
> # remove N/A data ------------------------


> na.omit(moosedata)
# A tibble: 37 × 6
Ecoregion                  Year  Area Estimated_Moose_Pop
<chr>                     <dbl> <dbl>               <dbl>
  1 North_Shore_Forests        2020  8400               21740
2 Northern_Peninsula_Fores…  2020 12000               32000
3 Long_Range_Barrens         2020  8800                5300
4 Central_Forests            2020 20500               41250
5 Western_Forests            2020  6800               17000
6 EasternHyperOceanicBarre…  2020  4400                2200
7 Maritime_Barrens           2020 21300               20450
8 Avalon_Forests             2020  4200                4040
9 StraitOfBelleIsleBarrens   2020   500                   5
10 North_Shore_Forests        1992  8400               14000
# ℹ 27 more rows
# ℹ 2 more variables: Observation_Method <chr>,
#   Data_Source <chr>
# ℹ Use `print(n = ...)` to see more rows
> moose_clean <- na.omit(moosedata)
> moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
> min(moose_sel)



> year_min <- min(moose_sel$Year, na.rm = TRUE)
> moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
> moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
> plot(moosedata2$Year, moosedata2$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
> plot(moose_west$Year, moose_west$MooseDensity,
       +      type = "l",
       +      xlab = "Year",
       +      ylab = "Moose per sq km",
       +      main = "Moose density in the Western Forests ecoregion over time")
> moose_2020 <- filter(moosedata2, Year == 2020)
> moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
> moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
> moosefinal <- moosedata2 %>%
  +     filter(Year == 2020) %>%
  +     filter(MooseDensity > 2.0) %>%
  +     arrange(desc(MooseDensity)) %>%
  +     print()

Ecoregion      Year  Area Estimated_Moose_Pop MooseDensity
<chr>         <dbl> <dbl>               <dbl>        <dbl>
  1 Northern_Pen…  2020 12000               32000         2.67
2 North_Shore_…  2020  8400               21740         2.59
3 Western_Fore…  2020  6800               17000         2.5 
4 Central_Fore…  2020 20500               41250         2.01
> library(readr)
> saplings <- read_csv("~/Downloads/SaplingStudy.csv")
Rows: 45 Columns: 5                                       
── Column specification ────────────────────────────────────
Delimiter: ","
chr (3): Ecoregion, Tree_ID, Species
dbl (2): Height, BrowsingScore

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> View(saplings)
> na.omit(saplings)

Ecoregion            Tree_ID Species Height BrowsingScore
<chr>                <chr>   <chr>    <dbl>         <dbl>
  1 North_Shore_Forests  NorthS… Alder     17.4           4.5
2 North_Shore_Forests  NorthS… Black_…   26.5           4  
3 North_Shore_Forests  NorthS… Black_…   26.5           4  
4 North_Shore_Forests  NorthS… White_…   25.5           4  
5 North_Shore_Forests  NorthS… Balsam…   20.4           4.5
6 North_Shore_Forests  NorthS… Willow    18.5           5  
7 North_Shore_Forests  NorthS… Balsam…   21.2           4.5
8 North_Shore_Forests  NorthS… Balsam…   22.5           4.5
9 Northern_Peninsula_… NorthP… Alder     17.9           5  
10 Northern_Peninsula_… NorthP… Willow    17.6           5  
# ℹ 34 more rows
# ℹ Use `print(n = ...)` to see more rows
> sap_clean <- na.omit(saplings)
> sap_reg_browse <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore))
> print(sap_reg_browse)

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
> avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
> print(avg_browse_reg)

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
> # The ecoregion with the highest average browsing pressure is at the top of the table,
  > # while the ecoregion with the lowest average browsing pressure is at the bottom.
  > sap_reg_height <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageHeight = mean(Height))
> print(sap_reg_height)

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
> sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)
> print(sap_reg_height_low)
# A tibble: 2 × 2
Ecoregion                  AverageHeight
<chr>                              <dbl>
  1 Northern_Peninsula_Forests          19.9
2 Western_Forests                     18.9
> # Ecoregions with average tree heights below 20 cm are considered severely browsed by moose
  > # and are shown in the sap_reg_height_low dataset.
  > sap_spe_browse <- sap_clean %>%
  +     group_by(Species) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore))
> print(sap_spe_browse)
# A tibble: 6 × 2
Species      AverageBrowsing
<chr>                  <dbl>
  1 Alder                   4.25
2 Balsam_Fir              3.14
3 Black_Ash               5   
4 Black_Spruce            2.33
5 White_Birch             3.14
6 Willow                  4.31
> avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
> print(avg_browse_spe)
# A tibble: 6 × 2
Species      AverageBrowsing
<chr>                  <dbl>
  1 Black_Ash               5   
2 Willow                  4.31
3 Alder                   4.25
4 White_Birch             3.14
5 Balsam_Fir              3.14
6 Black_Spruce            2.33
> # The species with the highest average browsing score appears at the top of the table,
  > # while the species with the lowest average browsing score appears at the bottom.
  > fir_reg_browse <- sap_clean %>%
  +     filter(Species == "Balsam_Fir") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore))
> print(fir_reg_browse)
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
> barplot(fir_reg_browse$AverageBrowsing,
          +         names.arg = fir_reg_browse$Ecoregion,
          +         xlab = "Ecoregion",
          +         ylab = "Average moose browsing intensity",
          +         main = "Average Balsam Fir browsing intensity by ecoregion",
          +         col = "forestgreen",
          +         cex.names = 0.6)
> spruce_reg_browse <- sap_clean %>%
  +     filter(Species == "Black_Spruce") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore))
> print(spruce_reg_browse)
# A tibble: 8 × 2
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
> barplot(spruce_reg_browse$AverageBrowsing,
          +         names.arg = spruce_reg_browse$Ecoregion,
          +         xlab = "Ecoregion",
          +         ylab = "Average moose browsing intensity",
          +         main = "Average Black Spruce browsing intensity by ecoregion",
          +         col = "darkolivegreen",
          +         cex.names = 0.6)
> # Overall, Black Spruce shows lower average browsing intensity than Balsam Fir
  > # across most ecoregions, suggesting that moose prefer to browse Balsam Fir
  > # compared to Black Spruce.
  > sap_reg_tally <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     tally()
> 
  > print(sap_reg_tally)
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
> # The number of tree saplings counted was not the same across all ecoregions,
  > # as shown by the differing tallies in sap_reg_tally.
  > sap_spe_tally <- sap_clean %>%
  +     group_by(Species) %>%
  +     tally()
> print(sap_spe_tally)
# A tibble: 6 × 2
Species          n
<chr>        <int>
  1 Alder            8
2 Balsam_Fir      11
3 Black_Ash        1
4 Black_Spruce     9
5 White_Birch      7
6 Willow           8
> # The number of tree saplings counted was not the same for each species,
  > # as shown by the different tallies in sap_spe_tally.
  > # The SaplingStudy dataset is not evenly distributed.
  > # Some ecoregions and tree species are overrepresented, while others have
  > # noticeably fewer sampled saplings, indicating uneven sampling effort.
  > # Recognizing bias is important because uneven sampling can have different results
  > # and lead to incorrect conclusions about ecological patterns, such as
  > # overestimating or underestimating moose browsing pressure.
  > moose_2020b <- moose_clean %>%
  +     filter(Year == 2020) %>%
  +     mutate(MooseDensity = Estimated_Moose_Pop / Area)
> moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
> sum_spe_browse <- sap_clean %>%
  +     group_by(Species, Ecoregion) %>%
  +     summarize(
    +         AvgBrowsing = mean(BrowsingScore),
    +         AvgMooseDensity = mean(MooseDensity)
    +     )
Error in `summarize()`:
  ℹ In argument: `AvgMooseDensity =
  mean(MooseDensity)`.
ℹ In group 1: `Species = "Alder"`, `Ecoregion =
  "Avalon_Forests"`.
Caused by error:
  ! object 'MooseDensity' not found
Run `rlang::last_trace()` to see where the error occurred.

> moose_2020b <- moose_clean %>%
  +     filter(Year == 2020) %>%
  +     mutate(MooseDensity = Estimated_Moose_Pop / Area)
> sum_spe_browse <- sap_moose_2020 %>%
  +     group_by(Species, Ecoregion) %>%
  +     summarize(
    +         AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    +         AvgMooseDensity = mean(MooseDensity, na.rm = TRUE)
    +     )
Error: object 'sap_moose_2020' not found

> sum_spe_browse <- sap_moose_2020 %>%
  +     group_by(Species, Ecoregion) %>%
  +     summarize
Error: object 'sap_moose_2020' not found

> sum_spe_browse <- moose_sap %>%
  +     group_by(Species, Ecoregion) %>%
  +     summarize(
    +         AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    +         AvgMooseDensity = mean(MooseDensity, na.rm = TRUE)
    +     )
`summarise()` has regrouped the output.
ℹ Summaries were computed grouped by Species and Ecoregion.
ℹ Output is grouped by Species.
ℹ Use `summarise(.groups = "drop_last")` to silence this
message.
ℹ Use `summarise(.by = c(Species, Ecoregion))` for
per-operation grouping instead.
> print(sum_spe_browse)
# A tibble: 38 × 4
# Groups:   Species [6]
Species    Ecoregion          AvgBrowsing AvgMooseDensity
<chr>      <chr>                    <dbl>           <dbl>
  1 Alder      Avalon_Forests             4             0.962
2 Alder      Central_Forests            4.5           2.01 
3 Alder      EasternHyperOcean…         4             0.5  
4 Alder      Long_Range_Barrens         3.5           0.602
5 Alder      Maritime_Barrens           3.5           0.960
6 Alder      North_Shore_Fores…         4.5           2.59 
7 Alder      Northern_Peninsul…         5             2.67 
8 Alder      Western_Forests            5             2.5  
9 Balsam_Fir Avalon_Forests             2             0.962
10 Balsam_Fir Central_Forests            3.5           2.01 
# ℹ 28 more rows
# ℹ Use `print(n = ...)` to see more rows
> library(ggplot2)
> 
  > ggplot(sum_spe_browse,
           +        aes(x = AvgMooseDensity, y = AvgBrowsing, color = Species)) +
  +     geom_point(size = 3) +
  +     theme_minimal() +
  +     labs(title = "Browsing Intensity Across Moose Density by Species",
             +          x = "Average Moose Density",
             +          y = "Average Browsing Score")
> # Yes, the figure provides some support for the hypothesis. At lower moose densities,
  > # browsing intensity differs more among species, while at higher densities
  > # browsing scores become more similar, suggesting the moose shift toward more general browsing.
  > # 23. b)# Moose appear to favor Balsam Fir the most, as it consistently shows higher browsing scores.
  > # Black Spruce is browsed the least, with generally lower browsing intensity across densities.
  > # 23. c) # Some sapling species are not shown because they did not have enough data points
  > # across ecoregions and moose densities to calculate reliable average values.
  > # In my figure, the species not shown due to this reason is the black ash sapling 
  > 
  > collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
> human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
> study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens",
                   +                  "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                   +                  "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
> 
  > 
  > moose_coll <- data.frame(collisions2020, human_pop, study_sites)
> 
  > # Print the dataset to check
  > print(moose_coll)
collisions2020 human_pop                study_sites
1             56     18000        North_Shore_Forests
2             60     12000 Northern_Peninsula_Forests
3             14      4000         Long_Range_Barrens
4             36     75100            Central_Forests
5             48     24000            Western_Forests
6             10      3500 EasternHyperOceanicBarrens
7             40     32000           Maritime_Barrens
8            110    270000             Avalon_Forests
9              6      2300   StraitOfBelleIsleBarrens
> library(dplyr)
> 
  > moose_coll2 <- moose_coll %>%
  +     rename(Ecoregion = study_sites)
> 
  > coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion", relationship = "many-to-many")
> 
  > # Scatterplot of MooseDensity vs moose-vehicle collisions
  > plot(coll_merge$MooseDensity, coll_merge$collisions2020,
         +      xlab = "Moose Density (moose/km²)",
         +      ylab = "Number of Moose-Vehicle Collisions",
         +      main = "Moose Density vs Moose-Vehicle Collisions",
         +      pch = 19, col = "green")
> # There appears to be a positive trend: ecoregions with higher moose density
  > # tend to have more moose-vehicle collisions. Avalon_Forests are different though,
  > # with a very high number of collisions compared to its density, this could be due to there being more cars in the avalon
  > # Create a new column for collisions per person
  > coll_merge_per_capita <- coll_merge %>%
  +     mutate(coll_per_capita = collisions2020 / human_pop)
> 
  > View(coll_merge_per_capita)
> # after viewing, it looks like the Northern Peninsula Forests have the highest number of collisions per capita
  > # Scatterplot of collisions per capita vs human population
  > plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
         +      xlab = "Human Population",
         +      ylab = "Collisions per Person",
         +      main = "Moose Collisions per Capita vs Human Population",
         +      pch = 19, col = "pink")
> # In Newfoundland, ecoregions with smaller human populations, like Northern Peninsula Forests
  > # and North Shore Forest, have higher moose collisions per person. More human populated regions,
  > # such as Avalon Forests, have lower per-capita collisions despite high total collisions. 
  > # This makes sense because there are more moose in these sparsely human populated areas, meaning the moose are more likely to cross roads therefore, more collisions per capita