# BIOL1002_R Assignment
# Jesse Whalen 
# 11-02-2026
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
library(dplyer)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
str(moose_sel)
moose_sel$Year <- as.numeric(moose_sel$Year)
year_min <- min(moose_sel$Year)
year_min
# The oldest observation in the dataset is 1904
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
#The highest estimated moose population recorded was 41250
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
    +     xlab = "year",
    +     ylab = "Moose Per sq km",
    +     main = "Moose density in Newfoundland Ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_west$MooseDensity <- moose_west$Estimated_Moose_Pop / moose_west$Area
plot(
    +    moose_west$Year, 
    +    moose_west$Moose_Density,
    +    type = "l",
    +    xlab = "year",
    +    ylab = "Moose Density (moose per unit area)",
    +    main = "Moose Density Over Time in the Western Forests Ecoregion")
moose_2020 <- filter(moose_sel, Year = 2020)
moose_2020$MooseDensity <- moose_2020$Estimated_Moose_Pop / moose_2020$Area
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
    +   filter(Year == 2020) %>%
    +   filter(MooseDensity > 2.0) %>%
    +   arrange(desc(MooseDensity)) %>%
    +   print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
library(dplyer)
+
    + sap_reg_browse <- sap_clean %>%
    +   group_by(Ecoregion)
    +   summarize(AverageBrowsing = mean(BrowsingScore))
+
    + print(sap_reg_browse)
# Northern_Peninsula_Forests has the highest browsing score (4.57), while Straight_of_Belle_Isle_Barrens has the lowest browsing score (1.00).
+ avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
+
    + sap_reg_height <- sap_clean %>%
    +   group_by(Ecoregion) %>%
    +   Summarize(AverageBrowsing = mean(Height))
+
    + print(sap_reg_height)
# Filter ecoregions with the average tree height being less than 20cm (severly browsed)
+ sap_reg_height_low <- sap_reg_height %>%
    +   filter(AverageHeight < 20)
+
    + print(sap_reg_height_low)
# Northern_Peninsula_Forests and Western_Forests both have severly browsed saplings (average heights < 20 cm)
sap_spe_browse <- sap_clean %>%
    +   group_by(Species) %>%
    +   summarize(AverageBrowsing = mean(BrowsingScore))
+
    + print(sap_reg_browse)
    +avg_browse_spe <- arrange(sap_spe_browse, desc(averageBrowsing))
# Black_Ash has the highest browsing score (5.0), Black_Spruce has the lowest browsing score (2.33)
+ Fir_reg_browse <- sap_clean %>%
    +   filter(Species == "Balsam_Fir") %>%
    +   group_by(Ecoregion) %>%
    +   summarize(AverageBrowsing = mean(BrowsingScore))
+
    + print(fir_reg_browse)
# North_Shore_Forests has the lowest (1.0)
+ barplot(
    +   fir_reg_browse$AverageBrowsing,
    +   names.arg = fir_neg_browse$Ecoregion
    +   xlab = "Ecoregion", 
    +   ylab = "Average Browsing Intensity",
    +   main = "Average Moose Browsing Intensity on Balsam Fir by Ecoregion",
    +   col = "forestgreen",
    +   cex.names = 0.6)
+ spruce_reg_browse <- sap_clean %>%
    +   filter(species == "Black Spruce") %>%
    +   group_by(Ecoregion) %>%
    +   summareize(AverageBrowsing = mean(BrowsingScore))
+ print(spruce_reg_browse)
# Black Spruce generally has lower browsing intensity compared to Balsam Fir across ecoregions, meaning  that moose show a stronger browsing preference for Balsam Fir Compared to Black Spruce.
+ sap_reg_tally <- sap_clean %>%
    +   group_by(Ecoregion) %>%
    +   tally() %>%
    +   print()
+ sap_spe_tally <- sap_clean %>%
    +   group_by(Species) %>%
    +   Tally() %>%
    +   print()
# The SaplingStudy dataset is not evenly distributed, North_shore_Forests (8) and Northern_Peninsula_Forests (7) are overrepresented, while Strait_of_Belle_Isle_Barrens (1) and Maritime_Barrens (3) are both underrepresented. Similarly, Balsam_Fir (11) and Black_Spruce (9) are sampled more often than Black_Ash (1), meaning uneven sampling across both ecoregions and species.

# Understanding bias in ecological datasets is very important because uneven sampling can cause incorrect conclusions about ecological Patterns. When some groups are sampled more than others, the results could be misleading or incomplete.
+ library(dplyer)
+ moose_2020b <- moose_clean %>%
    +   filter(Year == 2020) %>%
    +   mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
+ sum_spe_browse <- sap_clean %>%
    +   group_by(Species, Ecoregion) %>%
    +   summarize(AverageBrowsing = mean(BrowsingScore))
+ ggplot(sum_spe_browse,
        +   aes(x = Species, y = AverageBrowsing, color = Species)) +
    +   geom_point(size = 3) +
    +   theme_minimal() +
    +   labs(title = "Average Moose Browsing Score by Species") + 
        + x = "Species"
          y = "Average Browsing Score"

# Moose show a higher browsing on preferred species (e.g., Black Ash and Balsam Fir) even at low densities, and usually browse less preferred species more often when moose density is higher, reinforcing the hypothesis that moose are selective at low density and generalist at high density.
          
# Moose prefer Black Ash and Balsam Fir the most, with the highest average browsing scores. Black Spruce is browsed the least, with consistantly lower scores.
          
# Black Ash might not appear on the figure if it was sampled few times, (n-1 B/c it does not have enough data to plot reliably.
+ collisons2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
+ human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 27000, 2300)
+ study_sites <- C("North_shore_forests", "Northern_Peninsula_Forests", "Long_Range_Barrens",

"Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "STraitOfBElleISleBarrens")

    + moose_coll <- data.frame(collisons2020, human_pop, Study_sites)
+ moose_coll2 <- moose_coll%>%
    +   rename_with(-"Ecoregion", study_sites)
+ coll_merge <- moose_2020 %>%
    +   left_join(moose_coll2, by = "Ecoregion")
+
    + print(coll_merge)
+ plot(coll_merge$MooseDensity, coll_Merge$Collisions2020,
        +   xlab = "Moose Density", 
        +   ylab = "Number of Moose-Vehicle collisions (2020)"
        +   main = "Relationship between Moose Density and Vehicle Collisions")
# The scatter plot represents a positive relationship between moose density and the number of moose vehicle collisions, with collisions generally increasing as moose density increases. One obvious outlier has a very high number of collisions despite only moderate density, recommending that other factors such as human population or traffic volume also impact collision rates.
+ coll_merge_per_capita <- coll_merge 
    +   mutate(coll_per_capita = collisions2020 / human_pop) 
    +   print()
+ plot(coll_merge_per_capita$human_pop,
        +   coll_merge_per_capita$coll_per_capita
        +   xlab = "Human Population"
        +   ylab = "Moose Collisions per capita"
        +   main = "Moose Collisions per Capita vs Human Population"
        +   pch = 16)
# The plot represents the moose collisions per capita decreases as human population increases, with the highest per-person collision rates rates occurring in sparsley populated regions. This makes sense in NL b/c rural areas have more moose habitat and fewer people, increasing the likelihood that individuals encounter moose while driving.