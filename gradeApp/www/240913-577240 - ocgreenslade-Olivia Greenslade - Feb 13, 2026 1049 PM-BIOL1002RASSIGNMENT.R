# PART I: MOOSE POPULATIONS IN NL -----------------------------------------

#QUESTION 1:

library (dplyr)
library (dplyrAssist)

#QUESTION 2: 

datamoose = data.frame(read.csv
("C:/Users/O_Greenslade/Downloads/MUNYEAR12025-2026/WINTERSEMESTER/BIOLOGY1002/RASSIGNMENTBIOL1002/CoreDATAdownloads/MoosePopulation.csv"))

#QUESTION 3:

datamoose_minusNA <- na.omit(datamoose)

#QUESTION 4: 

moose_select <- select(datamoose_minusNA, Ecoregion, Year, Area, Estimated_Moose_Pop)

saveRDS(moose_select, file = "datamooseSELECT.rds")

#QUESTION 5 A: 

moose_select$Year <- as.numeric(as.character(moose_select$Year))
min_year <- min(moose_select$Year)

#QUESTION 5 B: 

moose_select$Estimated_Moose_Pop <- as.numeric(as.character(moose_select$Estimated_Moose_Pop))
max_population <- max(moose_select$Estimated_Moose_Pop)

#QUESTION 6: 

datamoose2 <- mutate(moose_select, MooseDensity = Estimated_Moose_Pop / Area)

#QUESTION 7: 

plot(datamoose2$Year,datamoose2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per (km^2)", 
     main = "Moose density in Newfoundland Ecoregions Over Time")

#QUESTION 8 A: 

moose_west <- filter(datamoose2, Ecoregion == "Western_Forests")

#QUESTION 8 B:

plot(moose_west$Year, moose_west$MooseDensity, 
     pch = 17, type ="l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in NL Western Forests over time")

#QUESTION 9 A:

moose_2020 <- filter(datamoose2, Year == "2020")

#QUESTION 9 B:

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

#QUESTION 9 C:

arrange(moose_2020_high, desc(MooseDensity))

#QUESTION 10: 

moosefinal <- datamoose2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# PART II: SAPLING DATA ---------------------------------------------------

#QUESTION 11 A:

saplings = data.frame(read.csv
                      ("C:/Users/O_Greenslade/Downloads/MUNYEAR12025-2026/WINTERSEMESTER/BIOLOGY1002/RASSIGNMENTBIOL1002/CoreDATAdownloads/SaplingStudy.csv"))

#QUESTION 11 B: 

sap_clean <- na.omit(saplings)

#QUESTION 12 A:

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore))%>%
  print()

#QUESTION 12 B:

avg_browse_reg <- arrange(sap_reg_browse, desc(AvgBrowsing))
# HIGHEST value = 4.57 (Northern_Peninsula_Forests), 
# LOWEST value = 1 (StraitOfBelleIsleBarrens).

#QUESTION 13 A:

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AvgHeight = mean(Height))%>%
  print()

#QUESTION 13 B:
sap_reg_height_low <- filter(sap_reg_height, AvgHeight < 20)%>%
  print()
# Northern_Peninsula_Forests and Western_Forests have average sapling heights below 20cm.

#QUESTION 14 A: 

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AvgSpeciesBrowsing = mean(BrowsingScore))%>%
  print()

#QUESTION 14 B: 

avg_browse_spe <- arrange(sap_spe_browse, desc(AvgSpeciesBrowsing))%>%
  print()

# Black_Ash has the HIGHEST browsing score, Black_Spruce has the LOWEST.

#QUESTION 15:

fir_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  filter(Species == "Balsam_Fir")%>%
  summarize(AvgFirBrowsing = mean(BrowsingScore))%>%
  arrange(desc(AvgFirBrowsing))%>%
  print()

#QUESTION 16:

barplot(fir_reg_browse$AvgFirBrowsing, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Region in Newfoundland", 
        ylab = "Average Moose Browsing", 
        main = "Average Moose Browsing of Basalm Firs by Ecoregion", 
        col = "brown",
        cex.names = 0.6,
        ylim=c(0,5)
        )

#QUESTION 17 A:

spruce_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  filter(Species == "Black_Spruce")%>%
  summarize(AvgSpruceBrowsing = mean(BrowsingScore))%>%
  arrange(desc(AvgSpruceBrowsing))%>%
  print()

#QUESTION 17 B:

barplot(spruce_reg_browse$AvgSpruceBrowsing, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Region in Newfoundland", 
        ylab = "Average Moose Browsing", 
        main = "Average Moose Browsing of Black Spruces by Ecoregion", 
        col = "purple4",
        cex.names = 0.6,
        ylim=c(0,5)
)

#QUESTION 18:

sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  arrange(desc(n))%>%
  print()

#QUESTION 19:

sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  arrange(desc(n))%>%
  print()

#QUESTION 20 A:

# Basalm firs in particular were overrepresented in the species tally; 11 samples of basalm firs were sampled, 
# a much larger sample size compared to an underrepresented group such as the black ash plants.

#QUESTION 20 B:

# Recognizing bias in ecological datasets is very important when it comes to preventing 
# the spread of skewed, unreliable information within the scientific community.
# Without measures against biased datasets, any data can be manipulated or misrepresented to
# support theories that are factually untrue.

# PART III: CREATING AND JOINING DATASETS ---------------------------------

#QUESTION 21 A:

moose_2020b <- filter(datamoose_minusNA, Year == "2020")%>%
mutate(moose_2020b, MooseDensity = Estimated_Moose_Pop / Area)%>%
arrange(desc(MooseDensity))%>%
  print()

#QUESTION 21 B:

moose_sap <- left_join(moose_2020b, sap_clean, 
                       by = 'Ecoregion', 
                       relationship = "many-to-many")
#QUESTION 22:

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity))%>%
  print()

#QUESTION 23 A:

# There is some evidence supporting the researchers' hypothesis regarding the behavior of browsing in 
# high and low densities of moose. In areas with lower moose density, and therefore less competition for food,
# moose browse species like black spruce or white birch at a low rate; in areas with higher moose
# density, however, the distribution of browsing by species becomes much more uniform.

#QUESTION 23 B:

# Moose most prefer to browse willow plants. They least prefer black spruce plants.

#QUESTION 23 C:

# Black ash plants are not shown on the plot. This is because a very limited amount of 
# black ash samples were taken by the researchers.

#QUESTION 24:

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#QUESTION 25 A:

moose_coll2 <- rename(moose_coll, "Ecoregion" = "study_sites")

#QUESTION 25 B:

coll_merge <- left_join(moose_coll2, moose_2020, 
                        by = 'Ecoregion', 
                        relationship = "many-to-many")

#QUESTION 26 A:

plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Density of Moose Population (squared km)", 
     ylab = "Collisions in 2020", 
     main = "Moose Population Density versus Amount of Moose-Vehicle Collisions 2020", 
     col = "red4",
     xlim=c(0,3.0),
     ylim=c(0,140))

#QUESTION 26 B:

# Generally, the number of moose-vehicle collisions increases with an increase in 
# moose population density. There is an outlier when considering the number of collisions recorded
# in the Avalon forests due to the exceptionally high population of humans in that area.

#QUESTION 27:

coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)%>%
  arrange(desc(coll_per_capita))

#QUESTION 28:
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop,
     xlab = "Collisions per Capita", 
     ylab = "Human Population", 
     main = "Moose-Vehicle Collisions per Capita", 
     col = "blue4",
     xlim=c(0,0.00500),
     ylim=c(0,300000))

#QUESTION 29:

# Generally, as the human population decreases, the collisions per capita increases.
# This trend makes sense considering that the areas with a lower human population often have a much higher
# moose density, leading to an exponential increase in moose-vehicle collisions per capita.