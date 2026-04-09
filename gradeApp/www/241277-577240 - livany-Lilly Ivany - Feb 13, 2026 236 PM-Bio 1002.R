library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
year_max <- max(moose_sel$year)              
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)               
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l" ,
     xlab = "year", 
     ylab = "Moose Density", 
     main = "Moose density in Western Forests over time")
moose_2020 <- moosedata2 %>% filter(Year == 2020)
moose_2020_high <- moose_2020 %>% 
  filter(MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
###Part2
saplings <- read.csv(file = "SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))
print(sap_reg_browse)
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc('AverageBrowsing'))
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>% summarize(mean(Height))
print(sap_reg_height)
# These ecoregions have average heights less than 20 cm
sap_reg_height_low <- sap_reg_height %>%
  filter(`mean(Height)`<20) %>%
  print()

sap_spe_browse <- saplings %>%
  group_by(Species) %>%
  summarize(MeanBrowsingScore = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(MeanBrowsingScore))
print('avg_browse_spe')
fir_reg_browse <- saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore))
barplot(fir_reg_browse$`mean(BrowsingScore), names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intesnity", main = "Average Balsam Fir Browsing Intesnity by Ecoregion", col = "forestgreen", cex.names = 0.6) # Reduces x-axis label size for readability
spruce_reg_browse <- sap_spe_browse %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore))
barplot(spruce_reg_browse$mean_browsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intesnity", main = "Average Black Spruce Browsing Intensity by Ecoregion", col = "steelblue", cex.names = 0.6) # Reduces x-axis label size for readability
# Balsam Fir experiences higher browsing intensity in most ecoregions compared to black spruce
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_reg_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#The Sapling Study data set is unevenly distributed.Balsam Fir is over presented.
# it's important to recognize bias in ecological data sets because you need to ensure ecological conclusions accurately reflect the natural system
moose_2020b <- moose_clean %>% filter(Year == "2020") %>% mutate(MooseDensity)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean(BrowsingScore), mean(MooseDensity))
print(sum_spe_browse)
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# 23A yes there is evidence supporting their hypothesis.There is such a wide range in browsing scores, some species having high density while others having low, showing that with all the dispersion it supports the hypothesis
# 23B Moose favor willow and white birch. They least browse alder and black spruce, you can see this through the colored dots on the graph
#23C Black ash is missing in the graph because the moose don't like the black ash
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#25.a) 
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
#25.b)
col_merge <- moose_coll2 %>% left_join(moose_2020, by = "Ecoregion")
#26.)
plot(col_merge$MooseDensity, collisions2020, 
     xlab = "MooseDensity", 
     ylab = "collisons2020", 
     main = "Moose Vechile Collisions 2020")
#26.b) In this graph we can see it's linear and there is a high outlier in the graph because 0.75 there is a plot higher than the points indicating an outlier
#27.)
coll_merge_per_capita <- col_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)
#28.) 
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita)
#29.) Based on the plot it shows a negative correlation where moose vehicle collisions decrease over time. meaning the area becomes more urban and deters moose collisons and making them more preventative.
