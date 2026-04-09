#Ava Kelly
#BIOL 1002
#q1
library(dplyr)
#q2
moosedata <- read.csv("MoosePopulation.csv")
#q3
moose_clean <- na.omit(moosedata)
#q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#q5. a)
year_min <- min(moose_sel$Year)
#year_min: 1904
#q5. b)
moose_max <- max(moose_clean$Estimated_Moose_Pop)
#moose_max: 41250
#q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type = "p", 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose Density in Newfoundland Ecoregions Over Time")
#q8. a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#q8. b)
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l", 
     xlab = "Year", 
     ylab = "Moose Density (per sq km)", 
     main = "Moose Density Over Time: Western Forests")
#q9. a)
moose_2020 <- filter(moosedata2, Year == 2020)
#q9. b)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#q9. c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#q11. a) 
saplings<-read.csv("SaplingStudy.csv")
#q11. b)
na.omit(saplings)
sap_clean<-na.omit(saplings)
#q12. a)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#q12. b)
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Lowest: StraitOfBelleIsleBarrens
#Highest: Northern_Peninsula_Forests
#q13. a)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
#q13. b) 
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
#Northern_Peninsula_Forests, Western_Forests
#q14. a)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#q14. b)
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Highest: "Black_Ash"
#Lowest: "Black_Spruce"
#q15
fir_reg_browse <- saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(MeanBrowseScore = mean(BrowsingScore)) %>%
  print()
#q16. a)
barplot(fir_reg_browse$MeanBrowseScore, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Intensity", 
        main = "Average Balsam Fir Browsing Intensity by Ecoregion", 
        col = "purple", 
        cex.names = 0.6)
#q17. a)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowseScore = mean(BrowsingScore)) %>%
  print()
#q17. b)
barplot(spruce_reg_browse$MeanBrowseScore, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Intensity", 
        main = "Average Black Spruce Browsing Intensity by Ecoregion", 
        col = "yellow", 
        cex.names = 0.6)
#q17. c)
#The Black Spruce has a lower browsing intensity for all Ecoregions compared to Balsam Fir.
#Balsam Fir scores are above 3, while Black Spruce scores stay around 2, meaning it is less favored by moose. 
#q18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#q19
sap_reg_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#q20. a)
#The dataset is not evenly distributed. The Avalon Forest has one group that appears in the data more then other remote regions such as Northern Peninsula. Balsam Fir also appear more often then other species. 
#q20. b)
#It is important to notice bias' because if the data comes from mainly one location, the results wont represent the whole provice. It prevents us from making accurate assumptions for the entire provice rather than just a few specific areas.
#q21. a)
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
#q21. b)
moose_sap<-left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#q22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore), 
            MeanDensity = mean(MooseDensity)) %>%
  print()
#q23
ggplot(sum_spe_browse, aes(x = MeanDensity, y = MeanBrowsing, color = Species)) + 
  geom_point(size = 3) + 
  theme_minimal() + 
  labs(title = "Browsing Intensity Across Moose Density by Species", 
       x = "Average Moose Density", 
       y = "Average Browsing Score")
#q23. a)
#Yes, there is evidence for the hypothesis. At the lower densities, moose pay more attention to the species they prefer like White Birch, but as density increases, the browsing pressure spreads out more to the species that are less wanted like Balsam Fir. 
#q23. b)
#Moose favor the sampling species like White Birch and Mountain Maple the most. They look at Black Spruce the least amount, as it is unappealing to them. 
#q23. c)
#Black Spruce is often missing because it has zero or very close to zero browsing recorded. Since there was no variation of data, it does not show a relationship with moose density and was excluded from the patterns.
#q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#q25. a)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
#q25. b)
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")
#q26. a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose/km2)",
     ylab = "Number of Collisions (2020)",
     main = "Moose Density vs. Vehicle Collisions",
     pch = 19, 
     col = "blue")
#q26. b)
#The plot shows a positive trend, as moose density increases, the number of collisions also rises. The biggest value is the region with 1.0 moose/km2 that has over 100 collisions. This suggests that a higher human traffic volume can cause high accident rates even in places with less mooses. 
#q27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

print(coll_merge_per_capita)
#q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per Capita",
     main = "Human Population vs. Individual Collision Risk",
     pch = 17, 
     col = "pink")
#q29
#The trend is showing that as human population increases, the collision risk per capita decreases. This makes sense for Newfoundland because in the urban areas there are more buildings to keep moose away, where as in rural communities have a higher risk due to more travel through moose habitats.
