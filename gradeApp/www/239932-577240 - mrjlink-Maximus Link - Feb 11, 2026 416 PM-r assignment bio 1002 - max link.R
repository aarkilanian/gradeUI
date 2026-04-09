
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

> setwd("/Users/me/Documents/RStudio/")
Error in setwd("/Users/me/Documents/RStudio/") : 
  cannot change working directory

> install.packages("dplyr")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:
  
  https://cran.rstudio.com/bin/windows/Rtools/
  Installing package into ‘C:/Users/Jack/AppData/Local/R/win-library/4.5’
(as ‘lib’ is unspecified)
also installing the dependencies ‘utf8’, ‘pkgconfig’, ‘withr’, ‘cli’, ‘generics’, ‘glue’, ‘lifecycle’, ‘magrittr’, ‘pillar’, ‘R6’, ‘rlang’, ‘tibble’, ‘tidyselect’, ‘vctrs’
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/utf8_1.2.6.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/pkgconfig_2.0.3.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/withr_3.0.2.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/cli_3.6.5.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/generics_0.1.4.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/glue_1.8.0.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/lifecycle_1.0.5.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/magrittr_2.0.4.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/pillar_1.11.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/R6_2.6.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/rlang_1.1.7.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/tibble_3.3.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/tidyselect_1.2.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/vctrs_0.7.1.zip'
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/dplyr_1.2.0.zip'
package ‘utf8’ successfully unpacked and MD5 sums checked
package ‘pkgconfig’ successfully unpacked and MD5 sums checked
package ‘withr’ successfully unpacked and MD5 sums checked
package ‘cli’ successfully unpacked and MD5 sums checked
package ‘generics’ successfully unpacked and MD5 sums checked
package ‘glue’ successfully unpacked and MD5 sums checked
package ‘lifecycle’ successfully unpacked and MD5 sums checked
package ‘magrittr’ successfully unpacked and MD5 sums checked
package ‘pillar’ successfully unpacked and MD5 sums checked
package ‘R6’ successfully unpacked and MD5 sums checked
package ‘rlang’ successfully unpacked and MD5 sums checked
package ‘tibble’ successfully unpacked and MD5 sums checked
package ‘tidyselect’ successfully unpacked and MD5 sums checked
package ‘vctrs’ successfully unpacked and MD5 sums checked
package ‘dplyr’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
C:\Users\Jack\AppData\Local\Temp\Rtmp42raXs\downloaded_packages
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union

Warning message:
  package ‘dplyr’ was built under R version 4.5.2 

> moosedata <- read.csv("MoosePopulation.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") :
  cannot open file 'MoosePopulation.csv': No such file or directory

> MoosePopulation <- read.csv("C:/Users/Jack/Downloads/MoosePopulation.csv")
>   View(MoosePopulation)
> moose_clean <- na.omit(moosedata)
Error: object 'moosedata' not found

> moose_clean <- na.omit(MoosePopulation)
> moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
> moose_year <- select(moose_clean, Year)
> year_min <- min(moose_year)
> moose_pop <- select(moose_clean, Estimated_Moose_Pop)
> moose_max <- max(moose_pop)
> moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
> plot(moosedata2$Year, moosedata2$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
> plot(moose_west$Year, moose_west$MooseDensity, 
       +      type = "1",
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Western Newfoundland ecoregions over time")
Error in plot.xy(xy, type, ...) : invalid plot type '1'

> plot(moose_west$Year, moose_west$MooseDensity, type = "1", xlab = "year" ylab = "Moose per sq km", main = "Moose density in Western Newfoundland ecoregions over time")
Error: unexpected symbol in "plot(moose_west$Year, moose_west$MooseDensity, type = "1", xlab = "year" ylab"

> plot(moose_west$Year, moose_west$MooseDensity, xlab = "year" ylab = "Moose per sq km", main = "Moose density in Western Newfoundland ecoregions over time")
Error: unexpected symbol in "plot(moose_west$Year, moose_west$MooseDensity, xlab = "year" ylab"

> plot(moosedata2$Year, moosedata2$MooseDensity, 
       +      type = "1",
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
Error in plot.xy(xy, type, ...) : invalid plot type '1'

> plot(moosedata2$Year, moosedata2$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
> plot(moosedata2$Year, moosedata2$MooseDensity, type = "1",
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
Error in plot.xy(xy, type, ...) : invalid plot type '1'

> plot(moosedata2$Year, moosedata2$MooseDensity, type = "l",
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
> plot(moose_west$Year, moose_west$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
> plot(moose_west$Year, moose_west$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
> plot(moose_west$Year, moose_west$MooseDensity, 
       +      type = "l"
       +      xlab = "year", 
       Error: unexpected symbol in:
         "     type = "l"
     xlab"
       
       > plot(moose_west$Year, moose_west$MooseDensity, 
              +      type = "l"
              +      xlab = "year", 
              Error: unexpected symbol in:
                "     type = "l"
     xlab"
              
              > plot(moose_west$Year, moose_west$MooseDensity, 
                     +      type = "l",
                     +      xlab = "year", 
                     +      ylab = "Moose per sq km", 
                     +      main = "Moose density in Newfoundland ecoregions over time")
              > plot(moose_west$Year, moose_west$MooseDensity, 
                     +      type = "l",
                     +      xlab = "year", 
                     +      ylab = "Moose per sq km", 
                     +      main = "Moose density in Western Newfoundland ecoregions over time")
              > moose_2020 <- filter(moosedata2, year = "2020")
              Error in `filter()`:
                ! We detected a named input.
              ℹ This usually means that you've used `=` instead of `==`.
ℹ Did you mean `year == "2020"`?
Run `rlang::last_trace()` to see where the error occurred.

> moose_2020 <- filter(moosedata2, year == "2020")
Error in `filter()`:
ℹ In argument: `year == "2020"`.
Caused by error:
! object 'year' not found
Run `rlang::last_trace()` to see where the error occurred.

> moose_2020 <- filter(moosedata2, Year == "2020")
> View(moose_2020)
> moose_2020_high <- filter(moose2020, MooseDensity > "2.0")
Error: object 'moose2020' not found

> moose_2020_high <- filter(moose_2020, MooseDensity > "2.0")
> moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
> View(moose_2020_high_byD)
> View(moose_2020_high_byD)
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
> SaplingStudy <- read.csv("C:/Users/Jack/Downloads/SaplingStudy.csv")
>   View(SaplingStudy)
> View(SaplingStudy)
> sap_clean <- na.omit(SaplingStudy)
> View(sap_clean)
> sap_reg_browse <- sap_clean %>%
+     group_by(Ecoregion) %>%
+     summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
+     print()
# A tibble: 9 × 2
  Ecoregion                  mean_BrowsingScore
  <chr>                                   <dbl>
1 Avalon_Forests                           2   
2 Central_Forests                          4   
3 EasternHyperOceanicBarrens               2.4 
4 Long_Range_Barrens                       2.6 
5 Maritime_Barrens                         1.83
6 North_Shore_Forests                      4.38
7 Northern_Peninsula_Forests               4.57
8 StraitOfBelleIsleBarrens                 1   
9 Western_Forests                          4.5 
> View(sap_reg_browse)
> sap_reg_browse <- sap_clean %>%
+     group_by(Ecoregion) %>%
+     summarize(AverageBrowsingScore = mean(BrowsingScore)) %>%
+     print()
# A tibble: 9 × 2
  Ecoregion                  AverageBrowsingScore
  <chr>                                     <dbl>
1 Avalon_Forests                             2   
2 Central_Forests                            4   
3 EasternHyperOceanicBarrens                 2.4 
4 Long_Range_Barrens                         2.6 
5 Maritime_Barrens                           1.83
6 North_Shore_Forests                        4.38
7 Northern_Peninsula_Forests                 4.57
8 StraitOfBelleIsleBarrens                   1   
9 Western_Forests                            4.5 
> sap_reg_browse <- sap_clean %>%
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
> avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
> 
> View(avg_browse_reg)
> # Northern Forests had the highest score at 4.571429, and Strait of Belle Isle Barrens had the lowest score at 1
> sap_reg_browse <- sap_clean %>%
+     group_by(Ecoregion) %>%
+     summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
+     print()
# A tibble: 9 × 2
  Ecoregion                  mean_BrowsingScore
  <chr>                                   <dbl>
1 Avalon_Forests                           2   
2 Central_Forests                          4   
3 EasternHyperOceanicBarrens               2.4 
4 Long_Range_Barrens                       2.6 
5 Maritime_Barrens                         1.83
6 North_Shore_Forests                      4.38
7 Northern_Peninsula_Forests               4.57
8 StraitOfBelleIsleBarrens                 1   
9 Western_Forests                          4.5 
> sap_reg_height <- sap_clean %>%
+     group_by(Ecoregion) %>%
+     summarize(AvgTreeHeight = mean(Height)) %>%
+     print()
# A tibble: 9 × 2
  Ecoregion                  AvgTreeHeight
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
> View(sap_reg_height)
> filter(sap_reg_height, AvgTreeHeight < "20")
# A tibble: 2 × 2
  Ecoregion                  AvgTreeHeight
  <chr>                              <dbl>
1 Northern_Peninsula_Forests          19.9
2 Western_Forests                     18.9
> sap_reg_height_low <- filter(sap_reg_height, AvgTreeHeight < "20") %>%
+     print()
# A tibble: 2 × 2
  Ecoregion                  AvgTreeHeight
  <chr>                              <dbl>
1 Northern_Peninsula_Forests          19.9
2 Western_Forests                     18.9
> View(sap_reg_height_low)
> #Northern Peninsula Forests and Western Forests have heights below 20 cm
> sap_spe_browse <- sap_clean %>%
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
> View(sap_spe_browse)
> avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
> 
> View(avg_browse_spe)
> # Black Ash had the highest score at 5, while Black Spruce had the lowest at 2.3
> fir_reg_browse <- filter(sap_clean, Species = "Balsam_Fir") %>%
+     group_by(Ecoregion) %>%
+     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
+     print()
Error in `filter()`:
! We detected a named input.
ℹ This usually means that you've used `=` instead of `==`.
              ℹ Did you mean `Species == "Balsam_Fir"`?
                Run `rlang::last_trace()` to see where the error occurred.
              
              > fir_reg_browse <- filter(sap_clean, Species == "Balsam_Fir") %>%
                +     group_by(Ecoregion) %>%
                +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
                +     print()
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
              > View(fir_reg_browse)
              > barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Score of Balsam Fir by Region", col = "forestgreen", cex.names = 0.6)
              > barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Score of Balsam Fir by Region", col = "forestgreen", cex.names = 0.4)
              > barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Score of Balsam Fir by Region", col = "forestgreen", cex.names = 0.6)
              > spruce_reg_browse <- filter(sap_clean, Species == "Black_Spruce") %>%
                +     group_by(Ecoregion) %>%
                +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
                +     print()
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
              > View(spruce_reg_browse)
              > barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Score of Black Spruce by Region", col = "darkgreen", cex.names = 0.6)
              > # Black Spruce on average had lower browsing scores thab Balsam Fir. The difference was small on the North Shore and Northern Peninsula Forests, but larger on the Eastern Hyperoceanic and Maritime Barrens, where Black Spruce had a browsing score of 0.0
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
              > sap_spe_tally<- sap_clean %>%
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
              > #20a) I do not think the dataset is evenly distributed. The distribution of ecoregions tends to favor the north shore and northern peninsula forests, with 8 and 7 saplings counted respectively, while only 1 sapling was counted in the Strait of Bell Island Barrens. The rest of the regions had around 5 saplings counted each. The distribution of species is even more uneven. Most of the Species have around 7-9 species counted, with Balsam Fir having 11. In contrast, Black Ash saplings were only counted once.
                > #20b) It's important to recognize bias in ecological datasets because ignoring them can lead to innacurate results. It's hard to judge the data for Black Ash saplings for example, as there was only one sample taken, giving it nothing to compare itself to. This can result in the conclusions skewing heavily in favor of the overrepresented data samples, and leaving out the underrepresented data samples.
                > moose_2020b <- moose_clean %>%
                +     filter(moose_clean, Year == "2020") %>%
                +     mutate(moose_clean, MooseDensity = Estimated_Moose_Pop / Area)
              Error in `filter()`:
                ℹ In argument: `moose_clean`.
              Caused by error:
                ! `..1` must be a logical vector, not a <data.frame> object.
              ℹ If you used `across()` to generate this data frame, please use
              `if_any()` or `if_all()` instead.
              Run `rlang::last_trace()` to see where the error occurred.
              
              > rlang::last_trace()
              Error in loadNamespace(x) : there is no package called ‘crayon’
              
              > View(moose_2020)
              > moose_2020b <- moose_2020
              > View(moose_2020b)
              > moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
              > sum_spe_browse <- moose_sap %>%
                +     group_by(Species, Ecoregion) %>%
                +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
                +     summarize(AverageDensity = mean(MooseDensity)) %>%
                +     print()
              `summarise()` has regrouped the output.
              ℹ Summaries were computed grouped by Species and Ecoregion.
              ℹ Output is grouped by Species.
              ℹ Use `summarise(.groups = "drop_last")` to silence this message.
              ℹ Use `summarise(.by = c(Species, Ecoregion))` for per-operation
              grouping instead.
              Error in `summarize()`:
                ℹ In argument: `AverageDensity = mean(MooseDensity)`.
              ℹ In group 1: `Species = "Alder "`.
              Caused by error:
                ! object 'MooseDensity' not found
              Run `rlang::last_trace()` to see where the error occurred.
              
              > View(moose_sap)
              > moose_sap_clean <- na.omit(moose_sap)
              > sum_spe_browse <- moose_sap_clean %>%
                +     group_by(Species, Ecoregion) %>%
                +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
                +     summarize(AverageDensity = mean(MooseDensity)) %>%
                +     print()
              `summarise()` has regrouped the output.
              ℹ Summaries were computed grouped by Species and Ecoregion.
              ℹ Output is grouped by Species.
              ℹ Use `summarise(.groups = "drop_last")` to silence this message.
              ℹ Use `summarise(.by = c(Species, Ecoregion))` for per-operation
              grouping instead.
              Error in `summarize()`:
                ℹ In argument: `AverageDensity = mean(MooseDensity)`.
              ℹ In group 1: `Species = "Alder "`.
              Caused by error:
                ! object 'MooseDensity' not found
              Run `rlang::last_trace()` to see where the error occurred.
              
              > sum_spe_browse <- moose_sap_clean %>%
                +     group_by(c(Species, Ecoregion)) %>%
                +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
                +     summarize(AverageDensity = mean(MooseDensity)) %>%
                +     print()
              Error in `group_by()`:
                ℹ In argument: `c(Species, Ecoregion)`.
              Caused by error:
                ! `c(Species, Ecoregion)` must be size 44 or 1, not 88.
              Run `rlang::last_trace()` to see where the error occurred.
              
              > sum_spe_browse <- moose_sap %>%
                +     group_by(Species, Ecoregion) %>%
                +     summarize(
                  +         AverageBrowsing = mean(BrowsingScore),
                  +         AverageDensity = mean(MooseDensity)) %>%
                +     print()
              `summarise()` has regrouped the output.
              ℹ Summaries were computed grouped by Species and Ecoregion.
              ℹ Output is grouped by Species.
              ℹ Use `summarise(.groups = "drop_last")` to silence this message.
              ℹ Use `summarise(.by = c(Species, Ecoregion))` for per-operation
              grouping instead.
              # A tibble: 38 × 4
              # Groups:   Species [6]
              Species      Ecoregion                  AverageBrowsing AverageDensity
              <chr>        <chr>                                <dbl>          <dbl>
                1 "Alder "     Avalon_Forests                         4            0.962
              2 "Alder "     Central_Forests                        4.5          2.01 
              3 "Alder "     EasternHyperOceanicBarrens             4            0.5  
              4 "Alder "     Long_Range_Barrens                     3.5          0.602
              5 "Alder "     Maritime_Barrens                       3.5          0.960
              6 "Alder "     North_Shore_Forests                    4.5          2.59 
              7 "Alder "     Northern_Peninsula_Forests             5            2.67 
              8 "Alder "     Western_Forests                        5            2.5  
              9 "Balsam_Fir" Avalon_Forests                         2            0.962
              10 "Balsam_Fir" Central_Forests                        3.5          2.01 
              # ℹ 28 more rows
              # ℹ Use `print(n = ...)` to see more rows
              > View(sum_spe_browse)
              > library(ggplot2)
              Error in library(ggplot2) : there is no package called ‘ggplot2’
              
              > install.packages(ggplot2)
              Error: object 'ggplot2' not found
              
              > install.packages
              function (pkgs, lib, repos = getOption("repos"), contriburl = contrib.url(repos, 
                                                                                        type), method, available = NULL, destdir = NULL, dependencies = NA, 
                        type = getOption("pkgType"), configure.args = getOption("configure.args"), 
                        configure.vars = getOption("configure.vars"), clean = FALSE, 
                        Ncpus = getOption("Ncpus", 1L), verbose = getOption("verbose"), 
                        libs_only = FALSE, INSTALL_opts, quiet = FALSE, keep_outputs = FALSE, 
                        ...) 
              {
                ""
                "This is an RStudio hook."
                "Use `utils::install.packages()` to bypass this hook if necessary."
                ""
                if (interactive()) {
                  canInstallPackages <- .Call("rs_canInstallPackages", 
                                              PACKAGE = "(embedding)")
                  if (!canInstallPackages) {
                    msg <- "Package installation is disabled in this version of RStudio."
                    stop(msg, call. = FALSE)
                  }
                  if (.rs.installPackagesRequiresRestart(pkgs)) {
                    call <- sys.call()
                    call[[1L]] <- quote(install.packages)
                    command <- .rs.deparseCall(call)
                    .rs.enqueLoadedPackageUpdates(command)
                    invokeRestart("abort")
                  }
                  .rs.addRToolsToPath()
                  on.exit(.rs.restorePreviousPath(), add = TRUE)
                }
                if (missing(lib) || is.null(lib)) 
                  lib <- .libPaths()[1L]
                isLocal <- is.null(repos) || any(grepl("/", pkgs, fixed = TRUE))
                if (isLocal) {
                  call <- sys.call()
                  call[[1L]] <- quote(utils::install.packages)
                  result <- eval(call, envir = parent.frame())
                }
                else {
                  before <- .rs.installedPackagesFileInfo(lib)
                  call <- sys.call()
                  call[[1L]] <- quote(utils::install.packages)
                  result <- eval(call, envir = parent.frame())
                  after <- .rs.installedPackagesFileInfo(lib)
                  rows <- .rs.installedPackagesFileInfoDiff(before, after)
                  .rs.recordPackageSource(rows$path, local = FALSE)
                }
                if (interactive()) {
                  .rs.updatePackageEvents()
                  .Call("rs_packageLibraryMutated", PACKAGE = "(embedding)")
                }
                invisible(result)
              }
              <bytecode: 0x0000018889646a48>
                <environment: namespace:utils>
                > install.packages("ggplot2")
              WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:
                
                https://cran.rstudio.com/bin/windows/Rtools/
                Installing package into ‘C:/Users/Jack/AppData/Local/R/win-library/4.5’
              (as ‘lib’ is unspecified)
              also installing the dependencies ‘cpp11’, ‘farver’, ‘labeling’, ‘RColorBrewer’, ‘viridisLite’, ‘gtable’, ‘isoband’, ‘S7’, ‘scales’
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/cpp11_0.5.3.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/farver_2.1.2.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/labeling_0.4.3.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/RColorBrewer_1.1-3.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/viridisLite_0.4.3.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/gtable_0.3.6.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/isoband_0.3.0.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/S7_0.2.1.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/scales_1.4.0.zip'
              trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/ggplot2_4.0.2.zip'
              package ‘cpp11’ successfully unpacked and MD5 sums checked
              package ‘farver’ successfully unpacked and MD5 sums checked
              package ‘labeling’ successfully unpacked and MD5 sums checked
              package ‘RColorBrewer’ successfully unpacked and MD5 sums checked
              package ‘viridisLite’ successfully unpacked and MD5 sums checked
              package ‘gtable’ successfully unpacked and MD5 sums checked
              package ‘isoband’ successfully unpacked and MD5 sums checked
              package ‘S7’ successfully unpacked and MD5 sums checked
              package ‘scales’ successfully unpacked and MD5 sums checked
              package ‘ggplot2’ successfully unpacked and MD5 sums checked
              
              The downloaded binary packages are in
              C:\Users\Jack\AppData\Local\Temp\Rtmp42raXs\downloaded_packages
              > library(ggplot2)
              Warning message:
                package ‘ggplot2’ was built under R version 4.5.2 
              > 
                > ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
                +     geom_point(size = 3) +
                +     
                +     theme_minimal() +
                +     labs(title = "Browsing Intensity Across Moose Density by Species",
                           +          x = "Average Moose Density",
                           +          y = "Average Browsing Score")
              Error in `geom_point()`:
                ! Problem while computing aesthetics.
              ℹ Error occurred in the 1st layer.
              Caused by error:
                ! object 'AvgDensity' not found
              Run `rlang::last_trace()` to see where the error occurred.
              
              > library(ggplot2)
              > 
                > ggplot(sum_spe_browse, aes(x = AverageDensity, y = AverageBrowsing, color= Species)) +
                +     geom_point(size = 3) +
                +     
                +     theme_minimal() +
                +     labs(title = "Browsing Intensity Across Moose Density by Species",
                           +          x = "Average Moose Density",
                           +          y = "Average Browsing Score")
              > #23a) The evidence shown in the graph does indeed support the hypothesis. It shows that in lower moose densities, moose tend to prefer alder and willow trees to Balsam fir, black ash, black spruce, and white birch. In comparison, with higher densities, while the moose do still prefer willow trees overall, the different species they browse becomes much more varied, with balsam fir, white birch, and black spruce being browsed at high density just as much as alder and willow in low density.
                > #23b) moose tend to favor willow saplings overalll, as shown by the fact that they show up with the highest browsing intensity score in every different moose density. Their least favorite seems to be black spruce, as (with the exception of one data point), it shows up with the lowest browsing intensity score across all moose densities
                > #23c) Black ash is not shown on the graph. This is because the dataset only included one sample of black ash saplings, which is not enough data to accurately determine it's place on the graph.
                > collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
              > human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
              > study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
              > moose_coll <- data.frame(collisions2020, human_pop, study_sites)
              > View(moose_coll)
              > moose_coll2 <- moose_coll %>%
                +     renamewith(Ecoregion = study_sites)
              Error in renamewith(., Ecoregion = study_sites) : 
                could not find function "renamewith"
              
              > moose_coll2 <- moose_coll %>%
                +     rename_with(Ecoregion = study_sites)
              Error in rename_with.data.frame(., Ecoregion = study_sites) : 
                argument ".fn" is missing, with no default
              
              > moose_coll2 <- moose_coll %>%
                +     rename(Ecoregion = study_sites)
              > View(moose_coll2)
              > coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
              > View(coll_merge)
              > plot(coll_merge$MooseDensity, coll_merge$collisions2020)
              > #on average, a higher moose density correlates to a higher number of vehicle collissions. The one outlier is the region with the highest amount of vehicle accidents (110). This region had a lower moose density than some others, but had a higher amount of collisions than any other.
                > coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
              > plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop)
              > 
                > plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita)
              > 
                > #The trend shows that a lower population results in a higher amount of accidents per capita. this makes sense, as the areas of Newfoundland with a higher density of moose are more rural, and rural areas have a smaller population. The only reason the number of accidens in the Avalon Forests are so high are because despite the low moose population density, the areas human population is very high, which means more vehicles drive through the area at any given time, which increases the chance for a collission with a moose 