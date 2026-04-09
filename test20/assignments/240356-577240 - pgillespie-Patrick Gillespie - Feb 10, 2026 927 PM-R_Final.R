# Title: Part_I
# Author: Patrick Gillespie
# Date: 03-02-2026

# Question 1 --------------------
library(dplyr)

# Question 2 --------------------
moosedata <- read.csv("MoosePopulation.csv")

# Question 3 --------------------
moose_clean <- na.omit(moosedata)

# Question 4 --------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5 --------------------
year_min <- min(select(moose_sel, Year))

moose_max <- max(select(moose_sel, Estimated_Moose_Pop))

# Question 6 --------------------
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7 -------------------
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq KM", 
     main = "Moose density in Newfoundland ecoregions over time",
     type = "l")

# Question 8 -------------------
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity,
     type = "l", 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests of Newfoundland Over Time")

# Question 9 -------------------
moose_2020 <- filter(moosedata2, Year == "2020")

moose_2020_high <- filter(moose_2020, MooseDensity > 2)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
# Question 10 ------------------

moosefinal <- moosedata2 %>%
   filter(Year == "2020") %>%
   filter(MooseDensity > 2) %>%
   arrange(desc(MooseDensity)) %>%
   print()



# Title: Part_II
# Author: Patrick Gillespie
# Date: 03-02-2026

# Question 11 --------------------
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

# Question 12 --------------------
sap_reg_browse <- sap_clean %>%
   group_by(Ecoregion) %>%
   summarize(average_bs = mean(BrowsingScore)) %>%
   print()

avg_browse_reg <- sap_reg_browse %>%
   arrange(desc(average_bs))
# Highest was Northern (4.57) & Western (4.5), 
# Lowest was Maritime (1.83) & Strait of Belle Isle Barrens (1)

# Question 13 --------------------
sap_reg_height <- sap_clean %>%
   group_by(Ecoregion) %>%
   summarize(average_h = mean(Height)) %>%
   print()

sap_reg_height_low <- filter(sap_reg_height, average_h <20)
print(sap_reg_height_low)

#Northern Peninsula and Western forests had average heights below 20cm.

# Question 14 --------------------

sap_spe_browse <- sap_clean %>%
   group_by(Species) %>%
   summarize(average_bs = mean(BrowsingScore)) %>%
   print()

avg_browse_spe <- sap_spe_browse %>%
   arrange(desc(average_bs)) %>%
   print()

# Black Ash has the highest* (5) (*only one sample collected), Black Spruce has
# the lowest (2.33)
# Question 15 --------------------
fir_reg_browse <- sap_clean %>%
   filter(Species == "Balsam_Fir") %>%
   group_by(Ecoregion) %>%
   summarise(avg_balsam_browse = mean(BrowsingScore)) %>%
   print()

# Question 16 --------------------

barplot(fir_reg_browse$avg_balsam_browse, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Eco Region", 
        ylab = "Average Browsing Score", 
        main = "Average Balsam Fir Browsing by Ecoregion", 
        col = "green", 
        cex.names = 0.6)

# Question 17 --------------------

spruce_reg_browse <- sap_clean %>%
   filter(Species == "Black_Spruce") %>%
   group_by(Ecoregion) %>%
   summarize(avg_spruce_browse = mean(BrowsingScore)) %>%
   print()

barplot(spruce_reg_browse$avg_spruce_browse, 
           names.arg = spruce_reg_browse$Ecoregion, 
            xlab = "Eco Region", 
            ylab = "Average Browsing Score", 
            main = "Average Spruce Browsing by Ecoregion", 
            col = "green", 
            cex.names = 0.6)

# The data suggests moose tend to prefer, across all regions where both species 
# inhabit, browsing of the Balsam Fir to the Black Spruce

# Question 18 --------------------

sap_reg_tally<- sap_clean %>%
       group_by(Ecoregion) %>%
       tally() %>% 
       print()

# Question 19 --------------------

sap_spe_tally <- sap_clean %>%
   group_by(Species) %>%
   tally() %>%
   print()

# Question 20 --------------------

# a) The sapling study is not evenly distributed. North shore (8) and northern
# peninsula (7) forests are overrepresented, while Strait of Belle Isle Barrens
# (1) and Maritime (3) are underrepresented. Balsam Fir (11) and Alder (8)
# are also heavily overrepresented compared to Black Ash, only shown once.

# b) Recognizing bias is important in ecological datasets because uneven
# sampling can lead to incorrect or misleading conclusions about the species
# or ecosystem. Conclusions such as Strait of Belle Isle Barren's lowest Browsing 
# score and Black Ash's lowest height could be products of sampling efforts 
# rather than actually representative of the broader system.

# Title: Part_III
# Author: Patrick Gillespie
# Date: 03-02-2026

# Question 21 --------------------

moose_2020b <- moose_clean %>%
   filter(Year == "2020") %>%
   mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, 
                       by = 'Ecoregion', 
                       relationship = "many-to-many")

# Question 22 --------------------

sum_spe_browse <- moose_sap %>%
   group_by(Species, Ecoregion) %>%
   summarize(avg_bs = mean(BrowsingScore), avg_md = mean(MooseDensity)) %>%
   print()

# Question 23 --------------------

# a) Yes, the data supports the researcher's hypothesis. At low moose densities, 
# there is high variability between tree species, indicating selective browsing;
# at high densities there is less variability between species, indicating less 
# selective more uniform browsing.

# b) Moose favour Alder & Willow most, as they show the highest browsing scores.
# Black spruce is browsed the lowest across all densities.

# c) Black ash is not shown in the figure, because only one sample was taken,
# making it unsuitable for any averages or trends across different densities.

# Question 24 --------------------

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens",
                 "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                 "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25 --------------------

moose_coll2 <- moose_coll %>%
   rename(Ecoregion = study_sites)
#I could not successfully use rename_with(), I had to use rename()
coll_merge <- left_join(moose_coll2, moose_2020b, by = 'Ecoregion')

# Question 26 --------------------

plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density", 
     ylab= "# of collisions",
     main = "Collisions vs Moose Density")

# As the moose density increases, the number of moose-vehicle collisions increases.
# One outlier is present, at 110 collisions (very high) and 1.0 (relatively low) density.

# Question 27 --------------------

coll_merge_per_capita <- coll_merge %>%
   mutate(coll_per_capita = collisions2020 / human_pop)

# Question 28 --------------------

plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population", 
     ylab = "# of Collisions per Capita", 
     main = "Moose Collisions per Capita vs Human Population", 
     xlim = c(0, 80000))
#added x limit to zoom in and exclude outlier, squashing data.

# Question 29 --------------------

# The trend shows higher collision rates per capita in lower population (rural),
# and lower rates in higher populations. This trend makes sense considering rural
# areas have high moose densities yet few people, increasing collision likelihood.