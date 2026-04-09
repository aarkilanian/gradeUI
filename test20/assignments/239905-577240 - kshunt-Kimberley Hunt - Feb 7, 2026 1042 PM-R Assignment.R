# Title: Bio 1002 R Assignment, Kimberley Hunt, February 2026
#Part 1 Q1
library(dplyr)
#Part 1 Q2 
MoosePopulation <- read.csv("MoosePopulation.csv")
#Part 1 Q3 
View(MoosePopulation)
moose_clean <- na.omit(MoosePopulation)
#Part 1 Q4 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Part 1 Q5 a) 
year_min <- min(moose_sel$Year)
#Part 1 Q5 b)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#Part 1 Q6
moosedata2 <- dplyr::mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Part 1 Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
+      xlab = "year", 
+      ylab = "Moose per sq km", 
+      main = "Moose density in Newfoundland ecoregions over time")
#Part 1 Q8 a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Part 1 Q8 b)
plot(moose_west$Year, moose_west$MooseDensity, 
+      xlab = "year", 
+      ylab = "Moose per sq km", 
+      main = "Moose density in Newfoundland over time in Western Forests")
#Part 1 Q9 a)
moose_2020 <- filter(moosedata2, Year == 2020)
#Part 1 Q9 b)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#Part 1 Q9 c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Part 1 Q10
moosefinal <- moosedata2 %>%
  + filter(Year == 2020) %>%
  + filter(MooseDensity > 2.0) %>%
  + arrange(desc(MooseDensity)) %>%
  + print()

#Part 2 Q11 a)
saplings <-read.csv("~/SaplingStudy.csv")
#Part 2 Q11 b)
sap_clean <- na.omit(saplings)
#Part 2 Q12 a)
sap_reg_browse <- sap_clean %>%
  + group_by(Ecoregion) %>%
  + summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  + print()
#Part 2 Q12 b)
avg_browse_reg <- sap_reg_browse %>%
  + arrange(desc(AverageBrowsing))
  #Highest=Northern Peninsula Forests, Lowest=Strait of Belle Isle Barrens
#Part 2 Q13 a)
sap_reg_height <- sap_clean %>%
  + group_by(Ecoregion) %>%
  + summarise(AverageHeight = mean(Height)) %>%
  + print()
#Part 2 Q13 b)
sap_reg_height_low <- sap_reg_height %>%
  + filter(AverageHeight < 20) %>%
  + print()
  #This code shows us which ecoregions have height<20. Northern Peninsula Forests and Western Forests are <20(severely browsed)
#Part 2 Q14 a)
sap_spe_browse <- sap_clean %>%
  + group_by(Species) %>%
  + summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  + print()
#Part 2 Q14 b)
avg_browse_spe <- sap_spe_browse %>%
  + arrange(desc(AverageBrowsing))
  #Highest=Black Ash, Lowest=Black Spruce
#Part 2 Q15
fir_reg_browse <- sap_clean %>%
  + filter(Species == "Balsam_Fir") %>%
  + group_by(Ecoregion) %>%
  + summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  + print()
#Part 2 Q16
barplot(fir_reg_browse$AverageBrowsing,
        + names.arg = fir_reg_browse$Ecoregion,
        + xlab = "Ecoregion",
        + ylab = "Average Browsing Score",
        + main = "Balsam Fir Browsing Intensity by Ecoregion",
        + col = "forestgreen",
        + cex.names = 0.6)
#Part 2 Q17 a)
spruce_reg_browse <- sap_clean %>%
  + filter(Species == "Black_Spruce") %>%
  + group_by(Ecoregion) %>%
  + summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  + print()
#Part 2 Q17 b)
barplot(spruce_reg_browse$AverageBrowsing,
        + names.arg = spruce_reg_browse$Ecoregion,
        + xlab = "Ecoregion",
        + ylab = "Average Browsing Score",
        + main = "Black Spruce Browsing Intensity by Ecoregion",
        + col = "darkolivegreen",
        + cex.names = 0.6)
#Part 2 Q17 c)
  #Black Spruce are shown to be generally lower than Balsam Fir. This indicates that Balsam Fir are more preferred by moose, therefore more heavily browsed.
#Part 2 Q18
sap_reg_tally <- sap_clean %>%
  + group_by(Ecoregion) %>%
  + tally() %>%
  + print()
#Part 2 Q19
sap_reg_tally <- sap_clean %>%
  + group_by(Ecoregion) %>%
  + tally() %>%
  + print()
#Part 2 Q20 a)
  #The sapling dataset isn't evenly distributed. Some ecoregions and species have many more samples than others. With uneven sampling, browsing comparisons could be altered.
#Part 2 Q20 b)
  #Recognizing bias in sampling for datasets is important because it can distort the true ecological patterns. Having more samples for a given region could make it seem like browsing is higher than what it really is

#Part 3 Q21 a)
moose_2020b <- moose_clean %>%
  + filter(Year == 2020) %>%
  + mutate(MooseDensity = Estimated_Moose_Pop / Area)
#Part 3 Q21 b)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
#Part 3 Q22
sum_spe_browse <- moose_sap %>%
  + group_by(Species, Ecoregion) %>%
  + summarise(AvgBrowsing = mean(BrowsingScore),
              + AvgDensity = mean(MooseDensity)) %>%
  + print() 
#Part 3 Q23 a)
  #Yes. At lower density, browsing scores vary more as the moose have strong preferences. At higher density, the browsing scores cluster together. This reveals that competition is higher, just as predicted in the hypothesis.
#Part 3 Q23 b)
  #From the chart, we can see that Willow holds the highest browsing score across densities.
  # Black Ash aren't on the graph so this one was definitely the least favourite. Of those graphed, Black Spruce are the lowest across densities.
#Part 3 Q23 c)
  #There were no Black Ash recorded on the graph because there were no samples of this sapling collected in the given year(2020) that also had data for density. When the datasets were combined, this one was led to be omitted.
#Part 3 Q24
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Part 3 Q25 a)
moose_coll2 <- moose_coll %>%
  + rename(Ecoregion = study_sites)
#Part 3 Q25 b)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
#Part 3 Q26 a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     + xlab = "Moose Density",
     + ylab = "Moose-Vehicle Collisions (2020)",
     + main = "Moose Density vs. Moose-Vehicle Collisions")
#Part 3 Q26 b)
  #Collisions generally increase as moose density does. There is an outlier at 1.0 density where there is a significant spike in collisions compared to the gradual increase of the other points.
#Part 3 Q 27
coll_merge_per_capita <- coll_merge %>%
  + mutate(coll_per_capita = collisions2020 / human_pop)
#Part 3 Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     + xlab = "Human Population",
     + ylab = "Collisions Per Capita",
     + main = "Collisions Per Capita vs. Human Population")
#Part 3 Q29
  #This graph shows that there are more collisions in areas with lower populations. This makes sense because moose tend to gather around areas that are less populated by humans as there is less infrastructure and more natural resources preserved.