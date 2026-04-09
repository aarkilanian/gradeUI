library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal,
union

Warning message:
  package ‘dplyr’ was built under R version 4.5.2 

> MoosePopulation <- read.csv("C:/Users/trist/Downloads/MoosePopulation.csv")
>   View(MoosePopulation)
> moose_clean <- na.omit(MoosePopulation)
> 
  > moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
> 
  > year_min <- min(moose_sel$Year)
> year_max <- max(moose_sel$Estimated_Moose_Pop)
> moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
> 
  > plot(moosedata2$Year, moosedata2$MooseDensity), xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time"
Error: unexpected ',' in "plot(moosedata2$Year, moosedata2$MooseDensity),"

> plot(moosedata2$Year, moosedata2$MooseDensity), xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time"
Error: unexpected ',' in "plot(moosedata2$Year, moosedata2$MooseDensity),"

> plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
> 
  > moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
> plot(type = "l", moose_west$Year, moose_west$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland's Western Forests over time")
> moose_2020 <- filter(moosedata2, Year == "2020")
> moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
> 
  > moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
> 
  > moosefinal <- moosedata2 $>$ filter(Year == 2020) $>$ arrange(desc(MooseDensity))
Error: unexpected '>' in "moosefinal <- moosedata2 $>"

> moosefinal <- moosedata2 %>% filter(Year == 2020) %>% arrange(desc(MooseDensity))
> print(moosefinal)
Ecoregion Year  Area
1 Northern_Peninsula_Forests 2020 12000
2        North_Shore_Forests 2020  8400
3            Western_Forests 2020  6800
4            Central_Forests 2020 20500
5             Avalon_Forests 2020  4200
6           Maritime_Barrens 2020 21300
7         Long_Range_Barrens 2020  8800
8 EasternHyperOceanicBarrens 2020  4400
9   StraitOfBelleIsleBarrens 2020   500
Estimated_Moose_Pop MooseDensity
1               32000    2.6666667
2               21740    2.5880952
3               17000    2.5000000
4               41250    2.0121951
5                4040    0.9619048
6               20450    0.9600939
7                5300    0.6022727
8                2200    0.5000000
9                   5    0.0100000
> 
  > SaplingStudy <- read.csv("C:/Users/trist/Downloads/SaplingStudy.csv")
>   View(SaplingStudy)
> saplings <- SaplingStudy
> 
  > sap_clean <- na.omit(saplings)
> 
  > sap_reg_browse <- sap-clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
Error: object 'sap' not found

> sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
> print(sap_reg_browse)
# A tibble: 9 × 2
Ecoregion                AverageBrowsing
<chr>                              <dbl>
  1 Avalon_Forests                      2   
2 Central_Forests                     4   
3 EasternHyperOceanicBarr…            2.4 
4 Long_Range_Barrens                  2.6 
5 Maritime_Barrens                    1.83
6 North_Shore_Forests                 4.38
7 Northern_Peninsula_Fore…            4.57
8 StraitOfBelleIsleBarrens            1   
9 Western_Forests                     4.5 
> avg_broswing_reg <arrange(sap_reg_browse, desc(AverageBrowsing))
Error: object 'avg_broswing_reg' not found

> avg_broswing_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
> #Northern Peninsula Forests have the highest Average Browsing, while Strait Of Bell Isle Barrens had the lowest.
  > 
  > sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height))
> print(sap_reg_height)
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
> sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)
> View(sap_reg_height_low)
> View(sap_reg_height_low)
> The Northern Peninsula Forests (19.91429) and Western Forests (18.94000) have the lowest average heights under 20
Error: unexpected symbol in "The Northern"

> # The Northern Peninsula Forests (19.91429) and Western Forests (18.94000) have the lowest average heights under 20
  > 
  > sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BroswingScore))
Error in `summarize()`:
  ℹ In argument: `AverageBrowsing =
  mean(BroswingScore)`.
ℹ In group 1: `Species = "Alder "`.
Caused by error:
  ! object 'BroswingScore' not found
Run `rlang::last_trace()` to see where the error occurred.

> sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore))
> print(sap_spe_browse)
# A tibble: 6 × 2
Species        AverageBrowsing
<chr>                    <dbl>
  1 "Alder "                  4.25
2 "Balsam_Fir"              3.14
3 "Black_Ash"               5   
4 "Black_Spruce"            2.33
5 "White_Birch"             3.14
6 "Willow"                  4.31
> avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
> View(avg_browse_spe)
> # Black Ash has the highest average browsing at 5.000000, while Black Spruce has the lowest at 2.333333.
  > 
  > fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
> View(fir_reg_browse)
> barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "average browsing intensity", main = "Average Browsing intensity of Balsam Fir per Ecoregion", col = "forestgreen", cex.names = 0.3)
> barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "average browsing intensity", main = "Average Browsing intensity of Balsam Fir per Ecoregion", col = "forestgreen", cex.names = 0.45)
> 
  > spruce_reg_browse <- sap-clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(Browsingscore))
Error: object 'sap' not found

> spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(Browsingscore))
Error in `summarize()`:
  ℹ In argument: `AverageBrowsing
= mean(Browsingscore)`.
ℹ In group 1: `Ecoregion =
  "Avalon_Forests"`.
Caused by error:
  ! object 'Browsingscore' not found
Run `rlang::last_trace()` to see where the error occurred.

> spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
> barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "average browsing intensity", main = "Average Browsing intensity of Balsam Fir per Ecoregion", col = "pink", cex.names = 0.45)
> # Black spruce and Fir differ, as black spruce has the average browsing intensity as 0.0 for two Ecoregions, while the Balsam fir have at least an average browsing intensity of one or more per Ecoregion.
  > 
  > sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally()
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
> 
  > sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally()
> View(sap_spe_tally)
> # the Strait Of Belle Isle Barrens were underrepresented, as well as Maritime Barrens. The North Shore Forests were overrepresented. I feel this may be realistic though because of the sheer density of saplings in a forest in comparison to barrens. Alder and Willow are underrepresented.
  > 
  > # It is important to recognize bias in ecological datasets to ensure that the data is well rounded, realistic, correst and covers important data from all areas, sample types etc.
  > 
  > moose_2020b <- moose_clean %>% filter(Year == "2020") %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
> moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
> sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(AvgBrowsing = mean(BrowsingScore), AvgDensity = mean(MooseDensity))
`summarise()` has regrouped the
output.
ℹ Summaries were computed grouped by
Species and Ecoregion.
ℹ Output is grouped by Species.
ℹ Use `summarise(.groups =
                   "drop_last")` to silence this
message.
ℹ Use `summarise(.by = c(Species,
                         Ecoregion))` for per-operation
grouping instead.
> 
  > # The hypothesis is supported by the data/figure. At lower moose density the moose browse at a high score for just a few species. At high densities the browsing scores increase, which suggests more competition in feeding and having to select at more of a variety.
  > 
  > # It seems the moose prefers the willow and alder the most. They seem to prefer black spruce and black ash the least
  > 
  > #it does not appear to me that any species are not shown or missing
  > 
  > collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
> > human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
Error: unexpected '>' in ">"

> human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
> study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
> 
  > moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
Error: object 'moose_coll' not found

> moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
Error: object 'moose_coll' not found

> moose_coll <- data.frame(collisions2020, human_pop, study_sites)
> moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
> coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
> plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density (moose/km^2)",ylab = "Moose-Vehicle Collisions (2020)", main = "Moose Density vs Vehicle Collisions (2020)")
> 
  > #with greater moose population density there are greater moose vehicle collisions. there are a few outliers but not with great significance
  > coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
> arrange(coll_merge_per_capita, desc(coll_per_capita))
Ecoregion Year
1 Northern_Peninsula_Forests 2020
2         Long_Range_Barrens 2020
3        North_Shore_Forests 2020
4 EasternHyperOceanicBarrens 2020
5   StraitOfBelleIsleBarrens 2020
6            Western_Forests 2020
7           Maritime_Barrens 2020
8            Central_Forests 2020
9             Avalon_Forests 2020
Area Estimated_Moose_Pop
1 12000               32000
2  8800                5300
3  8400               21740
4  4400                2200
5   500                   5
6  6800               17000
7 21300               20450
8 20500               41250
9  4200                4040
MooseDensity collisions2020
1    2.6666667             60
2    0.6022727             14
3    2.5880952             56
4    0.5000000             10
5    0.0100000              6
6    2.5000000             48
7    0.9600939             40
8    2.0121951             36
9    0.9619048            110
human_pop coll_per_capita
1     12000    0.0050000000
2      4000    0.0035000000
3     18000    0.0031111111
4      3500    0.0028571429
5      2300    0.0026086957
6     24000    0.0020000000
7     32000    0.0012500000
8     75100    0.0004793609
9    270000    0.0004074074
> 
  > plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Moose Collisions per Capita", main = "Moose Collisions per Capita vs Human Population")
> 
  > # with smaller human populations there is a higher risk of moose collisions per capita. When there are higher human population there is less risk of collision per each person.
  > 
  > save.image("~/R-assign BIO1002 FINAL/R-assign BIO1002 FINAL.RData")