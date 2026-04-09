####Lauren Hollett Biol1002 R-Assignment

#q1
library(dplyr)
#Uploaded through import dataset button
moosedata <- read_csv("C:/Users/holle/Downloads/MoosePopulation (1).csv")
View(moosedata)
## Q3
moose_clean <- na.omit(moosedata)
## Q4: Select columns of interest
> moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
> 
  > #q5   year_min  <- min(moose_sel$Year, na.rm = TRUE)
  > moose_max <- max(moose_sel$Estimated_Moose_Pop)
> year_min  <- min(moose_sel$Year)
> moose_max
[1] 41250
>year_min
[1] 1904
## Q6: Create MooseDensity
> moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
## Q7: Plot MooseDensity over time
  plot(moosedata2$Year, moosedata2$MooseDensity,
         +      xlab = "Year",
         +      ylab = "Moose per sq km",
         +      main = "Moose density in Newfoundland ecoregions over time",
         +      type = "l")
## Q8: Western_Forests dataset + plot
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
> 
  > plot(moose_west$Year, moose_west$MooseDensity,
           +      xlab = "Year",
           +      ylab = "Moose per sq km",
           +      main = "Moose density in Western Forests over time",
           +      type = "l")
## Q9: 2020 
  moose_2020 <- filter(moosedata2, Year == 2020)
  > moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
  > moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
## Q10  one pipe chain
  > moosefinal <- moosedata2 %>%
    +     filter(Year == 2020) %>%
    +     filter(MooseDensity > 2.0) %>%
    +     arrange(desc(MooseDensity)) %>%
    +     print()
#q11
  > saplings <- read_csv("C:/Users/holle/Downloads/SaplingStudy.csv")
  View(saplings)
  > sap_clean <- na.omit(saplings)
  
#q12
  > sap_reg_browse <- sap_clean %>%
    +     group_by(Ecoregion) %>%
    +     summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
    +     print()
 
#q13
  avg_browse_reg <- sap_reg_browse %>%
    +     arrange(desc(AverageBrowsing)) %>%
    +     print()
  #highest norther penn lowest strait of belle isle barrens
sap_reg_height <- sap_clean %>%
    +     group_by(Ecoregion) %>%
    +     summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>%
    +     print()
# below 20cm northern pen & western
#q14
> sap_spe_browse <- sap_clean %>%
  +     group_by(Species) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  +     print()
avg_browse_spe <- sap_spe_browse %>%
  +     arrange(desc(AverageBrowsing)) %>%
  +     print()
#highest black ash lowest black spruce

#q15
> fir_reg_browse <- sap_clean %>%
  +     filter(Species == "Balsam_Fir") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  +     print()

#q16
> barplot(fir_reg_browse$AverageBrowsing,
          +         names.arg = fir_reg_browse$Ecoregion,
          +         xlab = "Ecoregion",
          +         ylab = "Average Browsing Score",
          +         main = "Balsam Fir Browsing Intensity by Ecoregion",
          +         col = "pink",
          +         cex.names = 0.6)

#q17
> spruce_reg_browse <- sap_clean %>%
  +     filter(Species == "Black_Spruce") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  +     print()

#q18
barplot(spruce_reg_browse$AverageBrowsing,
        +         names.arg = spruce_reg_browse$Ecoregion,
        +         xlab = "Ecoregion",
        +         ylab = "Average Browsing Score",
        +         main = "Black Spruce Browsing Intensity by Ecoregion",
        +         col = "purple",
        +         cex.names = 0.6)
#Black Spruse has a higher standard deviation than that of Balsam Fir, there is a greater range of values for BS. however the overall mean of the two will be similar.

> sap_reg_tally <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     tally() %>%
  +     print()

#q19
> sap_spe_tally <- sap_clean %>%
  +     group_by(Species) %>%
  +     tally() %>%
  +     print()

#q20 The SaplingStudy dataset is not evenly distributed. Some ecoregions have many more samples (ex North_Shore_Forests n=8),while others are underrepresented (ex StraitOfBelleIsleBarrens n=1).
> #Species are also uneven, with Balsam_Fir most common (n=11) and Black_Ash rare (n=1). Recognizing bias is important because uneven sampling can distort statistical significance. Small sample sizes may not accurately represent true trends, leading to misleading ecological conclusions.
  
  #q21
  > moose_2020b <- moose_clean %>%
  +     filter(Year == 2020) %>%
  +     mutate(MooseDensity = Estimated_Moose_Pop / Area)
> moose_sap <- left_join(moose_2020b, sap_clean,
                         +                        by = "Ecoregion",
                         +                        relationship = "many-to-many")
 #q22
  > sum_spe_browse <- moose_sap %>%
  +     group_by(Species, Ecoregion) %>%
  +     summarize(
    +         AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    +         AvgDensity  = mean(MooseDensity, na.rm = TRUE)
    +     ) %>%
  +     print()

#q23 Yes. When moose density is low, they prefer certain species, but when density is high,they browse almost all species more evenly. Moose browse Willow the most and Black Ash the least. Black Ash has very few samples, so there is not enough data to show it clearly on the plot.

#q24
> collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
> 
  > human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
> 
  > study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                     +     "Long_Range_Barrens","Central_Forests","Western_Forests",
                     +     "EasternHyperOceanicBarrens","Maritime_Barrens",
                     +     "Avalon_Forests","StraitOfBelleIsleBarrens")
> moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#q25
moose_coll2 <- moose_coll %>%
  +     rename(Ecoregion = study_sites)
> coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")

#q26
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     +      xlab = "Moose Density",
     +      ylab = "Collisions (2020)",
     +      main = "Moose Density vs Vehicle Collisions",
     +      pch = 19)
# The regions with the highest moose density seem to have the highest collision rates.  An outlyer here is the point at 1, where there are more than 120 collisions in that area.  COuld be due to a bad road, or the ending of a moose fense.

#q27
> coll_merge_per_capita <- coll_merge %>%
  +     mutate(coll_per_capita = collisions2020 / human_pop)

#q28
> plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
       +      xlab = "Human Population",
       +      ylab = "Collisions per Person",
       +      main = "Collisions per Capita vs Human Population",
       +      pch = 19)
> 
#q29
  # Smaller human populations often show higher collisions per capita,likely because rural areas overlap more with moose habitat and highways. This makes ecological sense given Newfoundland’s rural areas surronded by Moose habitats versus somewhere like St. Johnès and the lack of forest covered areas.