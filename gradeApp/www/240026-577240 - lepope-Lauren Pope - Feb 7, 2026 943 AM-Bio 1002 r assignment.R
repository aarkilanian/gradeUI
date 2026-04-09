data1 <- read.csv(file = "MoosePopulation.csv")
data2 <- read.csv(file = "SaplingSample.csv")
MyData <- read.csv("c:/BIOL/quantifies.csv")
MyData <- read.csv("MoosePopulation.csv")
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_sel[,2])
Year_min <-min(moose_sel[,2])
max(moose_sel[,4])
moose_max <-max(moose_sel[,4])
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l",
     xlab = "year",
    ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
      xlab = "year",
      ylab = "Moose per sq km",
      main = "Moose density in Western Newfoundland ecoregions over time", type = "l")
moosedata2 <- filter(moosedata2, Year == "2020")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020 <- moosedata2
moose_2020 <- filter(moose_2020, MooseDensity > 2)
moose_2020_high <- filter(moose_2020, MooseDensity > 2)
arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
    filter(Year == 2020) %>%
    filter(MooseDensity > 2.0) %>%
    arrange(desc(MooseDensity)) %>%
    print()
sapplings <- SaplingStudy
sap_clean <- na.omit(sapplings)
sap_reg_browse <- sap_clean %>%
    group_by(Ecoregion) %>%
   summarize(mean_browse = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
avg_browse_reg <- sap_clean %>%
    group_by(Ecoregion) %>%
    summarize(
         AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)
       ) %>%
    arrange(desc(AverageBrowsing))
print(avg_browse_reg)
#North Peninsula Forests has the highest average moose browsing
#Strait of Belle Isle Barrens has the lowest average moose browsing.
sap_reg_height <- sap_clean %>%
   group_by (Ecoregion) %>%
   summarize(
       mean_Height = mean (Height, na.rm = TRUE))
print (sap_reg_height)
sap_reg_height_low <- sap_reg_height %>%
   filter (mean_Height < 20)
print (sap_reg_height_low)
# both Northern Peninsula Forests and Western Forests have average tree heights below 20cm and are therefore considered severely browsed by moose.
sap_spe_browse <- sap_clean %>%
    group_by(Species) %>%
   summarize(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
print (avg_browse_spe)
# Black Ash has the highest average browsing score and Black Spruce has the lowest average browsing score.
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>% group_by (Ecoregion) %>%
    summarize(
         mean_BrowsingScore = mean (BrowsingScore, na.rm = TRUE)
       )
print (fir_reg_browse)
barplot (
    fir_reg_browse$mean_BrowsingScore,
    names.arg = fir_reg_browse$Ecoregion,
    xlab = "Ecoregion",
    ylab = "Average Moose Browsing Score",
    main = "Average Balsam Fir Browsing Intensity by Ecoregion",
    col = "forestgreen",
   cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  
filter(Species == "Black_Spruce") %>%
   
 group_by (Ecoregion) %>%
  
  summarize(mean_BrowsingScore = mean (BrowsingScore, na.rm = TRUE))
print (spruce_reg_browse)
barplot(
   spruce_reg_browse$mean_BrowsingScore,
   names.arg = spruce_reg_browse$Ecoregion,
   xlab = "Ecoregion",
   ylab = "Average Moose Browsing Score",
   main = "Average Black Spruce Browsing Intensity by Ecoregion",
   col = "darkolivegreen", cex.names = 0.6)
# In general, Balsam Fir tends to have higher average browsing scores than Black spruce across most ecoregions, indicating that moose prefer Balsam Fir over Black Spruce. Differences are most pronounced in ecoregions X and Y.
sap_reg_tally <- sap_clean %>% group_by (Ecoregion) %>%
    tally()
print (sap_reg_tally)
print (sap_spe_tally)
# The SaplingStudy dataset does not appear to be evenly distributed.
# Some ecoregions and tree species are overrespresented, while others are underrepresented.
# This uneven sampling could bias interpretations of moose browsing patterns across ecoregions or species.
# Recognizing bias in ecological datasets is important because uneven sampling can misrepresent true patterns in the ecosystem.
# If certain species or ecoregions are over, or underrepresented, conclusions about things like moose browsing intensity may be misleading.
moose_2020b <- moose_clean %>%
     filter(Year == 2020) %>%
     mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <-moose_sap %>%
   group_by(Species, Ecoregion) %>%
   summarize(avg_browsing_score = mean(BrowsingScore, na.rm = TRUE), avg_moose_density = mean(MooseDensity, na.rm = TRUE))
print(sum_spe_browse)
#Yes, there is evidence that supports the researchers hypothesis. With higher average moose density, the moose are less picky about the sapling species that they consume. Moose show stronger preferences at low density where there is a lot of species to choose form whereas when there’s more moose in the area, the moose mist eat the less proffered species and. Have a more generalist browsing. We can see that with moose density of 1, there is little amount (0-0.5 average browsing score) of black spruce being eaten. When moose density is high, the average browsing rate if black spruce increases to 3-4.
#The willow is the most favoured species of sapling, reaching an average browsing density of 5. The moose show the least preference towards the black spruce.
#The sapling species that isnt shown on the figure is balck ash. This could be beause black ash has a higher average browsing score than 5, which is the highest number on the image.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", study_sites)
coll_merge <- moose_coll2 %>%
  left_join(moose_2020, by = "Ecoregion")
plot(coll_merge$MooseDensity,
      coll_merge$collisions2020,
      xlab = "Moose Density",
      ylab = "Number of Moose-vechile Collisions (2020)",
      main = "Moose Density Vs. Moose-Vehicle Collisions (2020)")
# There is a linear increase in Moose-Vehicle Collisions when moose density increases. There aren’t any particular outliers according to this graph as the lower density area has the lowest number of accidents (~40) and highest moose density has the highest number of accidents (~60).
coll_merge_per_capita <- coll_merge %>%
   mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop,
      coll_merge_per_capita$coll_per_capita,
      xlab = "Human Population",
      ylab = "Moose Collisions per Capita",
      main = "Moose Collisions per Capita Vs. Human Population")
# as populations in an area increases, moose collisions per capita decreases. This is because moose line in the woods in more rural areas. There ares often have lower populations. In the city, where population is higher, there are much less frequent moose collisions because there is nowhere for them to live.
