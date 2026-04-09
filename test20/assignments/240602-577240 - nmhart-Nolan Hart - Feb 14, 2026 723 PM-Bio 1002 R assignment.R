R version 4.5.2 (2025-10-31) -- "[Not] Part in a Rumble"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> getwd()
[1] "/cloud/project"
> setwd("bio2")
> getwb()
Error in getwb() : could not find function "getwb"
> getwd()
[1] "/cloud/project/bio2"
> setwd("/cloud/project/bio2")
> install.packages("dplyr")
Installing package into ‘/cloud/lib/x86_64-pc-linux-gnu-library/4.5’
(as ‘lib’ is unspecified)
also installing the dependencies ‘utf8’, ‘pkgconfig’, ‘withr’, ‘cli’, ‘generics’, ‘glue’, ‘lifecycle’, ‘magrittr’, ‘pillar’, ‘R6’, ‘rlang’, ‘tibble’, ‘tidyselect’, ‘vctrs’
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/utf8_1.2.6.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/pkgconfig_2.0.3.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/withr_3.0.2.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/cli_3.6.5.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/generics_0.1.4.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/glue_1.8.0.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/lifecycle_1.0.4.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/magrittr_2.0.4.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/pillar_1.11.1.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/R6_2.6.1.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/rlang_1.1.6.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/tibble_3.3.0.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/tidyselect_1.2.1.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/vctrs_0.6.5.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/dplyr_1.1.4.tar.gz'
* installing *binary* package ‘utf8’ ...
* DONE (utf8)
* installing *binary* package ‘pkgconfig’ ...
* DONE (pkgconfig)
* installing *binary* package ‘withr’ ...
* DONE (withr)
* installing *binary* package ‘cli’ ...
* DONE (cli)
* installing *binary* package ‘generics’ ...
* DONE (generics)
* installing *binary* package ‘glue’ ...
* DONE (glue)
* installing *binary* package ‘magrittr’ ...
* DONE (magrittr)
* installing *binary* package ‘R6’ ...
* DONE (R6)
* installing *binary* package ‘rlang’ ...
* DONE (rlang)
* installing *binary* package ‘lifecycle’ ...
* DONE (lifecycle)
* installing *binary* package ‘vctrs’ ...
* DONE (vctrs)
* installing *binary* package ‘pillar’ ...
* DONE (pillar)
* installing *binary* package ‘tidyselect’ ...
* DONE (tidyselect)
* installing *binary* package ‘tibble’ ...
* DONE (tibble)
* installing *binary* package ‘dplyr’ ...
* DONE (dplyr)

The downloaded source packages are in
‘/tmp/RtmpcegoLX/downloaded_packages’
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union
> Moosedata <- read.csv("MoosePopulation (1).csv")
> Moose_clean <- na.omit(moosedata)
Error: object 'moosedata' not found
> Moose_clean <- na.omit(Moosedata)
> Moose_sel <- select(Moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
> year_min <- min(Moose_sel$Year)
> moose_max <- max(Moose_sel$Estimated_Moose_Pop)
> Moosedata2 <- mutate(Moose_sel, MooseDensity = Estimated_Moose_pop / Area)
Error in `mutate()`:
  ℹ In argument: `MooseDensity = Estimated_Moose_pop/Area`.
Caused by error:
  ! object 'Estimated_Moose_pop' not found
Run `rlang::last_trace()` to see where the error occurred.
> Moosedata2 <- Mutate(Moose_sel, MooseDensity = Estimated_Moose_pop / Area)
Error in Mutate(Moose_sel, MooseDensity = Estimated_Moose_pop/Area) : 
  could not find function "Mutate"
> Moosedata2 <- mutate(Moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
> plot(moosedata2$Year, moosedata2$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
Error: object 'moosedata2' not found
> plot(Moosedata2$Year, Moosedata2$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
Error: object 'moosedata2' not found
> moose_west <- filter(Moosedata2, Ecoregion == "Western_Forests")
> plot(moose_west$Year, moose_west2$MooseDensity, 
       +      type = “1”,
       Error: unexpected input in:
         "plot(moose_west$Year, moose_west2$MooseDensity, 
     type = “"
       > plot(moose_west$Year,
              +      moose_west$Moose_Density,
              +      type = "l",
              +      xlab = "Year",
              +      ylab = "Moose Density (moose/km^2)",
              +      main = "Moose Density Over Time in Western Forests")
       > > moose_2020 <- filter(moosedata2, Year == 2020)
       Error: object 'moosedata2' not found
       > moose_2020 <- filter(Moosedata2, Year == 2020)
       > > moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
       > moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
       > moosefinal <- moosedata2 %>%
         +     filter(Year == 2020) %>%
         +     filter(MooseDensity > 2.0) %>%
         +     arrange(desc(MooseDensity)) %>%
         +     print()
       Error: object 'moosedata2' not found
       > moosefinal <- Moosedata2 %>%
         +     filter(Year == 2020) %>%
         +     filter(MooseDensity > 2.0) %>%
         +     arrange(desc(MooseDensity)) %>%
         +     print()
       Ecoregion Year  Area Estimated_Moose_Pop
       1 Northern_Peninsula_Forests 2020 12000               32000
       2        North_Shore_Forests 2020  8400               21740
       3            Western_Forests 2020  6800               17000
       4            Central_Forests 2020 20500               41250
       MooseDensity
       1     2.666667
       2     2.588095
       3     2.500000
       4     2.012195
       > saplins <- read.csv("SaplingStudy (1).csv")
       > sap_clean <- na.omit(saplings)
       Error: object 'saplings' not found
       > sap_clean <- na.omit(Saplings)
       Error: object 'Saplings' not found
       > sap_clean <- na.omit(saplins)
       > sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>%
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
       > avg_browse_reg <- sap_reg_browse %>%
         +     arrange(desc(AverageBrowsing))
       > > sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>%
         +     summarize(AverageHeight = mean(Height))
       > %>%
         Error: unexpected SPECIAL in "%>%"
       > sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>%
         +     summarize(AverageHeight = mean(Height))
       > %>%  print ()
       Error: unexpected SPECIAL in "%>%"
       > sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>%
         +     summarize(AverageHeight = mean(Height))%>%
         +     print ()
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
       > sap_reg_height_low <- sap_reg_height %>% filter (AverageHeight < 20) %>%
         +     print()
       # A tibble: 2 × 2
       Ecoregion                  AverageHeight
       <chr>                              <dbl>
         1 Northern_Peninsula_Forests          19.9
       2 Western_Forests                     18.9
       > sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>%
         +     print ()
       # A tibble: 6 × 2
       Species        AverageBrowsing
       <chr>                    <dbl>
         1 "Alder "                  4.25
       2 "Balsam_Fir"              3.14
       3 "Black_Ash"               5   
       4 "Black_Spruce"            2.33
       5 "White_Birch"             3.14
       6 "Willow"                  4.31
       > avg_browse_spe <- sap_spe_browse %>%
         +     arrange(desc(AverageBrowsing))
       > fir_reg_browse <- sap_clean %>%
         +     filter (Species == "Balsam_Fir") %>%
         +     group_by(Ecoregion) %>%
         +     summarize(AvgBrowsing = 
                           +     mean(Browsing Score)) %>%
         Error: unexpected symbol in:
         "    summarize(AvgBrowsing = 
    mean(Browsing Score"
       > fir_reg_browse <- your_dataset_name %>%
         +     filter(Species == "Balsam_Fir") %>%
         +     group_by(Ecoregion) %>%
         +     summarise(mean_browse = mean(BrowsingScore, na.rm = TRUE))
       Error: object 'your_dataset_name' not found
       > fir_reg_browse <- sap_clean%>%
         +     filter(Species == "Balsam_Fir") %>%
         +     group_by(Ecoregion) %>%
         +     summarise(mean_browse = mean(BrowsingScore, na.rm = TRUE))
       > > print(fir_reg_browse)
       # A tibble: 7 × 2
       Ecoregion                  mean_browse
       <chr>                            <dbl>
         1 Avalon_Forests                    2   
       2 Central_Forests                   3.5 
       3 EasternHyperOceanicBarrens        2   
       4 Long_Range_Barrens                1   
       5 Maritime_Barrens                  2   
       6 North_Shore_Forests               4.5 
       7 Northern_Peninsula_Forests        4.25
       > > barplot(fir_reg_browse$mean_browse,
                   +         names.arg = fir_reg_browse$Ecoregion,
                   +         xlab = "Ecoregion",
                   +         ylab = "Average Browsing Intensity",
                   +         main = "Average Balsam Fir Browsing by Ecoregion",
                   +         col = "forestgreen")
       > > spruce_reg_browse <- sap_clean %>% filter (Species == "Black_Spruce") %>% group_by(Ecoregion) %>%
         +     summarize(AvgBrowsing =
                           +                   mean (BrowsingScore)) %>%
         +     print()
       # A tibble: 8 × 2
       Ecoregion                  AvgBrowsing
       <chr>                            <dbl>
         1 Avalon_Forests                     0.5
       2 Central_Forests                    3  
       3 EasternHyperOceanicBarrens         0  
       4 Long_Range_Barrens                 2  
       5 Maritime_Barrens                   0  
       6 North_Shore_Forests                4  
       7 Northern_Peninsula_Forests         4  
       8 Western_Forests                    3.5
       > > barplot(spruce_reg_browse$mean_browse,
                   +         names.arg = spruce_reg_browse$Ecoregion,
                   +         xlab = "Ecoregion",
                   +         ylab = "Average Browsing Intensity",
                   +         main = "Average Black Spruce Browsing by Ecoregion",
                   +         col = "darkgreen")
       Error in barplot.default(spruce_reg_browse$mean_browse, names.arg = spruce_reg_browse$Ecoregion,  : 
                                  'height' must be a vector or a matrix
                                In addition: Warning message:
                                  Unknown or uninitialised column: `mean_browse`. 
                                > barplot(spruce_reg_browse$AvgBrowsing,
                                          +         names.arg = spruce_reg_browse$Ecoregion,
                                          +         xlab = "Ecoregion",
                                          +         ylab = "Average Browsing Score",
                                          +         main = "Black Spruce Browsing Intensity by Ecoregion",
                                          +         col = "darkblue",
                                          +         cex.names = 0.6)
                                > > # Black spurce has a lower average of browsing intensity than balsam fir  # across most ecoregions, which shows that moose prefer Balsam Fir.
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
                                > > # Some tree species and ecoregions are represented better in the data sets than others, and this may be evidence of uneven sampling (norther forest).
                                  > # If the bias is not taken into consideration, it may cause the misrepresentation of the data regarding the difference in the browsing of the moose based on the species
                                  > moose_2020b <- moose_clean %>%
                                  +     filter (Year == 2020) %>%
                                  +     mutate(MooseDensity = Estimated_Moose_Pop 
                                               +            / Area)
                                Error: object 'moose_clean' not found
                                > moose_2020b <- Moose_clean %>%
                                  +     filter (Year == 2020) %>%
                                  +     mutate(MooseDensity = Estimated_Moose_Pop 
                                               +            / Area)
                                > > moose_sap <- left_join(moose_2020b,                                  sap_clean, by = "Ecoregion",
                                                           + "relationship = "many-to-many")
Error: unexpected symbol in:
"moose_sap <- left_join(moose_2020b,                                  sap_clean, by = "Ecoregion",
                        "relationship = "many"
> moose_sap <- left_join(moose_2020b,
+                        sap_clean, by = "Ecoregion",
+                        "relationship = "many-to-many")
                        Error: unexpected symbol in:
                          "                       sap_clean, by = "Ecoregion",
                       "relationship = "many"
                        > moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion" "relationship = "many-to-many")
Error: unexpected string constant in "moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion" "relationship = ""
> moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
> sum_spe_browse <- moose_sap %>%
+     group_by(Species, Ecoregion) %>%
+     summarize(AvgBrowsing = mean (BrowsingScore), AgDensity = mean(MooseDensity)
+     ) %>%
+     print ()
`summarise()` has grouped output by 'Species'. You can override using
the `.groups` argument.
# A tibble: 38 × 4
# Groups:   Species [6]
   Species      Ecoregion                  AvgBrowsing AgDensity
   <chr>        <chr>                            <dbl>     <dbl>
 1 "Alder "     Avalon_Forests                     4       0.962
 2 "Alder "     Central_Forests                    4.5     2.01 
 3 "Alder "     EasternHyperOceanicBarrens         4       0.5  
 4 "Alder "     Long_Range_Barrens                 3.5     0.602
 5 "Alder "     Maritime_Barrens                   3.5     0.960
 6 "Alder "     North_Shore_Forests                4.5     2.59 
 7 "Alder "     Northern_Peninsula_Forests         5       2.67 
 8 "Alder "     Western_Forests                    5       2.5  
 9 "Balsam_Fir" Avalon_Forests                     2       0.962
10 "Balsam_Fir" Central_Forests                    3.5     2.01 
# ℹ 28 more rows
# ℹ Use `print(n = ...)` to see more rows
> > library(ggplot2)
Error in library(ggplot2) : there is no package called ‘ggplot2’
> install.packages("ggplot2")
Installing package into ‘/cloud/lib/x86_64-pc-linux-gnu-library/4.5’
(as ‘lib’ is unspecified)
also installing the dependencies ‘cpp11’, ‘farver’, ‘labeling’, ‘RColorBrewer’, ‘viridisLite’, ‘gtable’, ‘isoband’, ‘S7’, ‘scales’
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/cpp11_0.5.2.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/farver_2.1.2.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/labeling_0.4.3.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/RColorBrewer_1.1-3.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/viridisLite_0.4.2.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/gtable_0.3.6.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/isoband_0.3.0.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/S7_0.2.1.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/scales_1.4.0.tar.gz'
trying URL 'http://rspm/default/__linux__/noble/latest/src/contrib/ggplot2_4.0.1.tar.gz'
* installing *binary* package ‘cpp11’ ...
* DONE (cpp11)
* installing *binary* package ‘farver’ ...
* DONE (farver)
* installing *binary* package ‘labeling’ ...
* DONE (labeling)
* installing *binary* package ‘RColorBrewer’ ...
* DONE (RColorBrewer)
* installing *binary* package ‘viridisLite’ ...
* DONE (viridisLite)
* installing *binary* package ‘gtable’ ...
* DONE (gtable)
* installing *binary* package ‘S7’ ...
* DONE (S7)
* installing *binary* package ‘isoband’ ...
* DONE (isoband)
* installing *binary* package ‘scales’ ...
* DONE (scales)
* installing *binary* package ‘ggplot2’ ...
* DONE (ggplot2)

The downloaded source packages are in
	‘/tmp/RtmpcegoLX/downloaded_packages’
> library(ggplot2)
Posit Community (formerly RStudio Community) is a great place
to get help: https://forum.posit.co/c/tidyverse
> > ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
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
> > ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
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
> > ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color = Species)) +
+     geom_point(size = 3) +
+     theme_minimal() +
+     labs(
+         title = "Browsing Intensity Across Moose Density by Species",
+         x = "Average Moose Density",
+         y = "Average Browsing Score"
+     )
Error in `geom_point()`:
! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
! object 'AvgDensity' not found
Run `rlang::last_trace()` to see where the error occurred.
> sum_spe_browse <- saplins%>%
+     group_by(Species, Ecoregion) %>%
+     summarize(
+         AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
+         AvgDensity = mean(MooseDensity, na.rm = TRUE)
+     )
Error in `summarize()`:
ℹ In argument: `AvgDensity = mean(MooseDensity, na.rm = TRUE)`.
ℹ In group 1: `Species = ""` `Ecoregion = ""`.
Caused by error:
! object 'MooseDensity' not found
Run `rlang::last_trace()` to see where the error occurred.
> sum_spe_browse <- saplins %>%
+     group_by(Species, Ecoregion) %>%
+     summarize(
+         AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
+         AvgDensity = mean(MooseDensity, na.rm = TRUE)
+     )
Error in `summarize()`:
ℹ In argument: `AvgDensity = mean(MooseDensity, na.rm = TRUE)`.
ℹ In group 1: `Species = ""` `Ecoregion = ""`.
Caused by error:
! object 'MooseDensity' not found
Run `rlang::last_trace()` to see where the error occurred.
> library(ggplot2)
> > ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
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
> names(sum_spe_browse)
[1] "Species"     "Ecoregion"   "AvgBrowsing" "AgDensity"  
> ggplot(sum_spe_browse, aes(x = AgDensity, y = AvgBrowsing, color = Species)) +
+     geom_point(size = 3) +
+     theme_minimal() +
+     labs(
+         title = "Browsing Intensity Across Moose Density by Species",
+         x = "Average Moose Density",
+         y = "Average Browsing Score"
+     )
> > # Yes, there is some support. At higher densities of moose, the browsing scores do increase for most of the species, indicating that the moose browse more widely rather than strongly preferring a few of the species at lower densities.
> # Moose favour willow the most while they browse them while, they least favour black spurce (based on graph).
> # The missing species is the black ash, which has no browsing data, indicating that the average browsing score could be zero or less than the threshold to be included in the data summary.
> collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
> human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
> study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
> > moose_coll <- data.frame(collisions2020, human_pop, study_sites)
> > collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
> > human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
> > study_sites <- c("North_Shore_Forests",
+                  "Northern_Peninsula_Forests",
+                  "Long_Range_Barrens",
+                  "Central_Newfoundland_Forests",
+                  "Western_Newfoundland_Forests",
+                  "Avalon_Forests",
+                  "Burin_Peninsula_Forests",
+                  "Northeast_Coast_Forests",
+                  "South_Coast_Forests")
> > moose_coll <- data.frame(collisions2020, human_pop, study_sites)
> > collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
> > human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
> > study_sites <- c("North_Shore_Forests",
+                  "Northern_Peninsula_Forests",
+                  "Long_Range_Barrens",
+                  "Central_Newfoundland_Forests",
+                  "Western_Newfoundland_Forests",
+                  "Avalon_Forests",
+                  "Burin_Peninsula_Forests",
+                  "Northeast_Coast_Forests",
+                  "South_Coast_Forests")
> > moose_coll <- data.frame(collisions2020, human_pop, study_sites)
> plot(coll_merge$moosedensity,
+      coll_merge$collisions2020,
+      xlab = "Moose Density",
+      ylab = "Moose-Vehicle Collisions (2020)",
+      main = "Moose Density vs Collisions")
Error: object 'coll_merge' not found
> moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
> coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
> plot(coll_merge$moosedensity,
+      coll_merge$collisions2020)
Error in xy.coords(x, y, xlabel, ylabel, log) : 
  'x' and 'y' lengths differ
> plot(coll_merge$moosedensity,
+      coll_merge$collisions2020)
Error in xy.coords(x, y, xlabel, ylabel, log) : 
  'x' and 'y' lengths differ
> coll_merge_per_capita <- coll_merge %>%
+     mutate(coll_per_capita = collisions2020 / human_pop)
> > plot(coll_merge_per_capita$human_pop,
+      coll_merge_per_capita$coll_per_capita,
+      xlab = "Human Population",
+      ylab = "Collisions Per Person",
+      main = "Collisions Per Capita vs Human Population")
> > # There is a negative relationship between the human population and       # collisions per capita. In areas with smaller populations, there are more # collisions per capita, and this is logical, as rural areas probably have # a larger moose population per capita in Newfoundland.