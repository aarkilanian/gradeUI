Moosedata <- read.csv("~/Desktop/BIOL 1002 R Assignment/MoosePopulation.csv")
na.omit(Moosedata)
moose_clean <- na.omit(Moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# the oldest observation is 1904
min(1904)
year_min <- min(1904)
# The highest estimated_moose_pop recorded is 41250
max(41250)
moose_max <- max(41250)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     +      xlab = "year", 
     +      ylab = "Moose per sq km", 
     +      main = "Moose density in Newfoundland ecoregions over time")
moose_2020 <- filter(moose_clean, Year == "2020")
moose_2020_high <- filter(moosedata2, moosedata2$MooseDensity >= 2.)
moose_2020_high_byD <- arrange(moosedata2, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  +     filter(Year == 2020) %>%
  +     filter(MooseDensity > 2.0) %>%
  +     arrange(desc(MooseDensity)) %>%
  +     print()
Saplings <- read.csv("~/Desktop/BIOL 1002 R Assignment/SaplingStudy.csv")
na.omit(Saplings)
sap_clean <- na.omit(Saplings)
group_by(sap_clean, Ecoregion)
sap_reg_browse <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
avg_browse_reg <- sap_reg_browse %>%
  +     arrange(desc(mean_browsing))
print(avg_browse_reg)
# Highest amount of browsing is 4.57 in Northern Peninsula Forests, Lowest is 1 in Strait of Belle Island Barrens 
sap_reg_height <- sap_clean%>%
  +     group_by(Ecoregion) %>%
  +     summarize(mean_height = mean(Height, na.rm = TRUE))
 print(sap_reg_height)
 sap_reg_height_low <- sap_reg_height %>%
   +     filter(mean_height < 20)
print(sap_reg_height_low)
#Ecoregions with height below 20cm are Northern Peninsula Forests and Western Forests 
sap_spe_browse <- sap_clean %>%
  +     group_by(Species) %>%
  +     summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
avg_browse_spe <- sap_spe_browse %>%
   +     arrange(desc(mean_browsing))
print(avg_browse_spe)
# Highest browsing score is Black Ash, lowest is Black Spruce
fir_reg_browse <- sap_clean %>%
  +     filter(Species == "Balsam_Fir") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(fir_reg_browse)
barplot(fir_reg_browse$mean_browsing,
          +         names.arg = fir_reg_browse$Ecoregion,
          +         xlab = "Ecoregion",
          +         ylab = "Average browsing intensity",
          +         main = "Average Balsam Fir browsing intensity by ecoregion",
          +         col = "forestgreen",
          +         cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  +     filter(Species == "Black_Spruce") %>%
  +     group_by(Ecoregion) %>%
  +     summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(spruce_reg_browse)
barplot(spruce_reg_browse$mean_browsing,
          +         names.arg = spruce_reg_browse$Ecoregion,
          +         xlab = "Ecoregion",
          +         ylab = "Average browsing intensity",
          +         main = "Average Black Spruce browsing intensity by ecoregion",
          +         col = "green",
          +         cex.names = 0.6)
# Balsam Fir appears to be browsed more consistent compared to Black Spruce. Both Blck spruce and Balsam Fir have high browsing in the Northern Peninsula Forests
sap_reg_tally <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     tally()
print(sap_reg_tally)
sap_spe_tally <- sap_clean %>%
  +     group_by(Species) %>%
  +     tally()
print(sap_spe_tally)
# The SaplingStudy dataset is not evenly distributed because some ecoregions and some species have way more sampled saplings than others, meaning certain regions and species are overrepresented while others are underrepresented.
# It's important to recognize bias in ecological datasets because uneven sampling can lead to misleading conclusions about patterns like the browsing intensity and species vulnerability. Biased data may reflect sampling effort rather than true ecological differences.
moose_2020b <- moose_clean %>%
  +     filter(Year == 2020) %>%
  +     mutate(MooseDensity = Estimated_Moose_Pop / Area) 
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many") 
sum_spe_browse <- moose_sap %>%
+     group_by(Species, Ecoregion) %>%
+     summarize(AvgBrowsingScore = mean(BrowsingScore, na.rm = TRUE))
                (AvgMooseDensity = mean(MooseDensity, na.rm = TRUE))
print(sum_spe_browse)
#Yes, there is some evidence supporting the hypothesis. At low moose density, species like Willow and Alder are heavily browsed, while less prefered species like Black Spruce have low browsing. At high density, moose appear to browse a wider range of species (more colorful plots).
# Moose favour Willow the most, showing high browsing scores, and browse Black Spruce the least, with very low brwosing scores.
# Black Ash is not shown in the figure, likely because there were very few or no saplings of this species in the sampled ecoregions.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  + rename_with(.fn = ~ "Ecoregion", .cols = study_sites)
coll_merge <- moose_coll2 %>%
  + left_join(moose_2020, by = "Ecoregion")
plot(coll_merge$Estimated_Moose_Pop, coll_merge$collisions2020,
     + xlab = "Moose Density (moose per km²)",
     + ylab = "Number of Collisions in 2020",
     + main = "Moose Density vs Moose-Vehicle Collisions",
     + pch = 17, col = "forestgreen")
#The trend looks generally positive. An outlier is the point at the top left, high collisons despote low moose density 
 coll_merge_per_capita <- coll_merge %>%
  + mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
       +      xlab = "Human Population",
       +      ylab = "Collisons Per Capita",
       +      main = "Human Population vs. Collisons per Capita",
     +      pch = 17, col = "blue")
#This plot has a negative trend, meaning that there is more collisions in lesser populated areas. Which makes sense because i know that most moose stay out of large cities, which means they cant even be apart of collisons in highly populated areas.