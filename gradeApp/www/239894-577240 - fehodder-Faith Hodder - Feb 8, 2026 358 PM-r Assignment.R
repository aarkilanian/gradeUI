> install.packages("dplyr")
--- Please select a CRAN mirror for use in this session ---
trying URL 'https://muug.ca/mirror/cran/bin/macosx/big-sur-x86_64/contrib/4.5/dplyr_1.2.0.tgz'
Content type 'application/octet-stream' length 1644094 bytes (1.6 MB)
==================================================
downloaded 1.6 MB


The downloaded binary packages are in
	/var/folders/_r/h6kmllsn1ns_hdtpfytgp0z00000gn/T//Rtmpn363S5/downloaded_packages
> 
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> moosedata <- read.csv("~/Downloads/MoosePopulation (1).csv")
> 
> head(moosedata)
                   Ecoregion Year  Area Estimated_Moose_Pop Observation_Method
1        North_Shore_Forests 2020  8400               21740      Aerial_Survey
2 Northern_Peninsula_Forests 2020 12000               32000      Aerial_Survey
3         Long_Range_Barrens 2020  8800                5300        Camera_Trap
4            Central_Forests 2020 20500               41250      Aerial_Survey
5            Western_Forests 2020  6800               17000   Expert_Estimate 
6 EasternHyperOceanicBarrens 2020  4400                2200   Expert_Estimate 
         Data_Source
1  GovOfNewfoundland
2     NGO_Monitoring
3 MemorialUniversity
4  GovOfNewfoundland
5        ParksCanada
6 MemorialUniversity
> 
> moose_clean <- na.omit(moosedata)
> 
> moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
> 
> year_min <- min(moose_sel$Year)
> moose_max <- max(moose_sel$Estimated_Moose_Pop)
> 
> moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
> 
> plot(moosedata2$Year, moosedata2$MooseDensity,
+      xlab = "Year",
+      ylab = "Moose per sq km",
+      main = "Moose density in Newfoundland ecoregions over time")
> 
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
> 
> plot(moose_west$Year, moose_west$MooseDensity,
+      type = "l",
+      xlab = "Year",
+      ylab = "Moose per sq km",
+      main = "Moose Density Over Time in Western Forests")
> 
> moose_2020 <- filter(moosedata2, Year == 2020)
> 
> moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
> 
> moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
> 
> moosefinal <- moosedata2 %>%
+   filter(Year == 2020) %>%
+   filter(MooseDensity > 2.0) %>%
+   arrange(desc(MooseDensity)) %>%
+   print()
                   Ecoregion Year  Area Estimated_Moose_Pop MooseDensity
1 Northern_Peninsula_Forests 2020 12000               32000     2.666667
2        North_Shore_Forests 2020  8400               21740     2.588095
3            Western_Forests 2020  6800               17000     2.500000
4            Central_Forests 2020 20500               41250     2.012195
> 

> saplings <- read.csv("~/Downloads/SaplingStudy.csv")
> 
> head(saplings)
            Ecoregion  Tree_ID      Species Height BrowsingScore
1 North_Shore_Forests NorthS_1       Alder    17.4           4.5
2 North_Shore_Forests NorthS_2 Black_Spruce   26.5           4.0
3 North_Shore_Forests NorthS_3 Black_Spruce   26.5           4.0
4 North_Shore_Forests NorthS_4  White_Birch   25.5           4.0
5 North_Shore_Forests NorthS_5   Balsam_Fir   20.4           4.5
6 North_Shore_Forests NorthS_6       Willow   18.5           5.0
> 
> sap_clean <- na.omit(saplings)
> 
> sap_reg_browse <- sap_clean %>%
+   group_by(Ecoregion) %>%
+   summarize(AverageBrowsing = mean(BrowsingScore)) %>%
+   print()
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
> 
> avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
> # Highest browsing = first row
> # Lowest browsing = last row
> 
> sap_reg_height <- sap_clean %>%
+   group_by(Ecoregion) %>%
+   summarize(AverageHeight = mean(Height)) %>%
+   print()
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
> 
> sap_reg_height_low <- sap_reg_height %>%
+   filter(AverageHeight < 20) %>%
+   print()
# A tibble: 2 × 2
  Ecoregion                  AverageHeight
  <chr>                              <dbl>
1 Northern_Peninsula_Forests          19.9
2 Western_Forests                     18.9
> # These regions are severely browsed
> 
> sap_spe_browse <- sap_clean %>%
+   group_by(Species) %>%
+   summarize(AvgBrowse = mean(BrowsingScore)) %>%
+   print()
# A tibble: 6 × 2
  Species        AvgBrowse
  <chr>              <dbl>
1 "Alder "            4.25
2 "Balsam_Fir"        3.14
3 "Black_Ash"         5   
4 "Black_Spruce"      2.33
5 "White_Birch"       3.14
6 "Willow"            4.31
> 
> avg_browse_spe <- arrange(sap_spe_browse, desc(AvgBrowse))
> # Highest = first
> # Lowest = last
> 
> fir_reg_browse <- sap_clean %>%
+   filter(Species == "Balsam_Fir") %>%
+   group_by(Ecoregion) %>%
+   summarize(AvgBrowse = mean(BrowsingScore))
> 
> barplot(fir_reg_browse$AvgBrowse,
+         names.arg = fir_reg_browse$Ecoregion,
+         xlab = "Ecoregion",
+         ylab = "Average Browsing Score",
+         main = "Balsam Fir Browsing Intensity",
+         col = "forestgreen",
+         cex.names = 0.6)
> 
> 
> spruce_reg_browse <- sap_clean %>%
+   filter(Species == "Black_Spruce") %>%
+   group_by(Ecoregion) %>%
+   summarize(AvgBrowse = mean(BrowsingScore))
> 
> barplot(spruce_reg_browse$AvgBrowse,
+         names.arg = spruce_reg_browse$Ecoregion,
+         xlab = "Ecoregion",
+         ylab = "Average Browsing Score",
+         main = "Black Spruce Browsing Intensity",
+         col = "darkolivegreen",
+         cex.names = 0.6)
> 
> # Compare: Write 1–2 sentences after viewing graphs
> 
> sap_reg_tally <- sap_clean %>%
+   group_by(Ecoregion) %>%
+   tally() %>%
+   print()
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
> 
> sap_spe_tally <- sap_clean %>%
+   group_by(Species) %>%
+   tally() %>%
+   print()
# A tibble: 6 × 2
  Species            n
  <chr>          <int>
1 "Alder "           8
2 "Balsam_Fir"      11
3 "Black_Ash"        1
4 "Black_Spruce"     9
5 "White_Birch"      7
6 "Willow"           8
> 
> moose_2020b <- moose_clean %>%
+   filter(Year == 2020) %>%
+   mutate(MooseDensity = Estimated_Moose_Pop / Area)
> 
> moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
> 
> sum_spe_browse <- moose_sap %>%
+   group_by(Species, Ecoregion) %>%
+   summarize(
+     AvgBrowsing = mean(BrowsingScore),
+     AvgDensity = mean(MooseDensity)
+   ) %>%
+   print()
`summarise()` has regrouped the output.
ℹ Summaries were computed grouped by Species and Ecoregion.
ℹ Output is grouped by Species.
ℹ Use `summarise(.groups = "drop_last")` to silence this message.
ℹ Use `summarise(.by = c(Species, Ecoregion))` for per-operation grouping
  (`?dplyr::dplyr_by`) instead.
# A tibble: 38 × 4
# Groups:   Species [6]
   Species      Ecoregion                  AvgBrowsing AvgDensity
   <chr>        <chr>                            <dbl>      <dbl>
 1 "Alder "     Avalon_Forests                     4        0.962
 2 "Alder "     Central_Forests                    4.5      2.01 
 3 "Alder "     EasternHyperOceanicBarrens         4        0.5  
 4 "Alder "     Long_Range_Barrens                 3.5      0.602
 5 "Alder "     Maritime_Barrens                   3.5      0.960
 6 "Alder "     North_Shore_Forests                4.5      2.59 
 7 "Alder "     Northern_Peninsula_Forests         5        2.67 
 8 "Alder "     Western_Forests                    5        2.5  
 9 "Balsam_Fir" Avalon_Forests                     2        0.962
10 "Balsam_Fir" Central_Forests                    3.5      2.01 
# ℹ 28 more rows
# ℹ Use `print(n = ...)` to see more rows
> 
> sum_spe_browse <- moose_sap %>%
+   group_by(Species, Ecoregion) %>%
+   summarize(
+     AvgBrowsing = mean(BrowsingScore),
+     AvgDensity = mean(MooseDensity)
+   ) %>%
+   print()
`summarise()` has regrouped the output.
ℹ Summaries were computed grouped by Species and Ecoregion.
ℹ Output is grouped by Species.
ℹ Use `summarise(.groups = "drop_last")` to silence this message.
ℹ Use `summarise(.by = c(Species, Ecoregion))` for per-operation grouping
  (`?dplyr::dplyr_by`) instead.
# A tibble: 38 × 4
# Groups:   Species [6]
   Species      Ecoregion                  AvgBrowsing AvgDensity
   <chr>        <chr>                            <dbl>      <dbl>
 1 "Alder "     Avalon_Forests                     4        0.962
 2 "Alder "     Central_Forests                    4.5      2.01 
 3 "Alder "     EasternHyperOceanicBarrens         4        0.5  
 4 "Alder "     Long_Range_Barrens                 3.5      0.602
 5 "Alder "     Maritime_Barrens                   3.5      0.960
 6 "Alder "     North_Shore_Forests                4.5      2.59 
 7 "Alder "     Northern_Peninsula_Forests         5        2.67 
 8 "Alder "     Western_Forests                    5        2.5  
 9 "Balsam_Fir" Avalon_Forests                     2        0.962
10 "Balsam_Fir" Central_Forests                    3.5      2.01 
# ℹ 28 more rows
# ℹ Use `print(n = ...)` to see more rows
> 
> collisions2020 <- c(56,60,14,36,48,10,40,110,6)
> human_pop <- c(18000,12000,4000,75100,24000,3500,32000,270000,2300)
> study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
> 
> moose_coll <- data.frame(collisions2020, human_pop, study_sites)
> 
> moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
> 
> coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
> 
> plot(coll_merge$MooseDensity, coll_merge$collisions2020,
+      xlab = "Moose Density",
+      ylab = "Collisions (2020)",
+      main = "Moose Density vs Collisions")
> 
> coll_merge_per_capita <- mutate(coll_merge,
+                                 coll_per_capita = collisions2020 / human_pop)
> 
> plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
+      xlab = "Human Population",
+      ylab = "Collisions per Person",
+      main = "Collisions per Capita vs Population")
> 
# 8.)Moose density generally increases over time, especially after the mid-1900s, suggesting successful population expansion after introduction.
# 8b.)Moose density in Western Forests increases over time, indicating favourable habitat and strong population growth in this region.
# 9.)Ecoregions with density > 2.0 represent areas with the highest moose concentration and likely strongest browsing pressure.
# 12.)The first ecoregion in avg_browse_reg has the highest browsing pressure.
# The last ecoregion has the lowest browsing pressure.
#13.) Ecoregions with average height < 20 cm show severe browsing because moose repeatedly eat young growth and prevent saplings from growing tall.
#14.) Moose prefer species with the highest average browsing score and avoid the species with the lowest score, indicating selective feeding behaviour.
# 17.)Balsam Fir generally shows higher browsing intensity than Black Spruce, suggesting moose prefer fir due to higher palatability and nutritional value.
# 18.)The number of trees counted differs among ecoregions, indicating uneven sampling effort.
#19.) Some species were sampled more frequently than others, meaning the dataset is not evenly distributed across species.
# 20.a) The dataset is not evenly distributed because some ecoregions and species are overrepresented while others have fewer observations.
# 20.b)Recognizing bias is important because uneven sampling can misrepresent true ecological patterns and lead to incorrect conclusions about moose preferences.
#23 Yes, the pattern supports the hypothesis: at low moose density browsing is selective, but at higher density browsing becomes more uniform across species due to competition for food.
# Moose favour the most palatable species (typically Balsam Fir) and browse least on less palatable species such as Black Spruce.
# One species may not appear because it had insufficient observations after grouping or missing values during summarization.
#26 Moose collisions generally increase with moose density, although some regions deviate due to road density and human population differences.
#29 Regions with smaller human populations often have higher collisions per person because moose habitat overlaps more with roads, which matches expectations in rural Newfoundland.

