install.packages("dplyr"
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv") 
<- read.csv("MoosePopulation.csv")
moosedata <- read.csv("MoosePopulation 3.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type="l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > "2.0")
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#part2
saplings <- read.csv("SaplingStudy 3.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageBrowsing= mean(BrowsingScore)) %>% print()
avg_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowsing))
#lowest(strait0fBelleIsleBarrens)
#Higest(Northern_peninsula_forrests)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height))  %>% print()
 
sap_reg_height_low <- sap_reg_heightsap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>% print()
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%

print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%

print()                                                              
# Species with highest average browsing: (top of table)
# Species with lowest average browsing: (bottom of table)
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Balsam Fir Browsing Intensity by Ecoregion", col = "forestgreen", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce")  %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "darkolivegreen", 
        cex.names = 0.6)

barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "darkolivegreen", 
        cex.names = 0.6
        
        barplot(
          spruce_reg_browse$AverageBrowsing,
          names.arg = spruce_reg_browse$Ecoregion,
          xlab = "Ecoregion",
          ylab = "Average Browsing Intensity",
          main = "Black Spruce Browsing Intensity by Ecoregion",
          col = "darkolivegreen",
          cex.names = 0.6)
        
      
barplot (spruce_reg browseSAverageBrows ing,
                  names. arg = spruce_reg browseSEcoregion,
                  xlab = "Ecoregion",
                  ylab = "Average Browsing Intensity",
                  main = "Black Spruce Browsing Intensity by Ecoregion",
                  col = "darkolivegreen",
                  cex.names = 0.6)
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg= spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "darkolivegreen",
        cex.names = 0.6)
barplot(spruce_reg_browse$AverageBrowsing,names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "black spruce browsing intensity by Ecoregion",col = "forestgreen",cex.names = 0.6)
        
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()      

barplot(spruce_reg_browse$mean_browsing,
        names.arg = spruce_reg_browse$Ecoregion,
        main = "Average Black Spruce Browsing Score by Ecoregion",
        ylab = "Mean Browsing Score",
        col = "darkgreen")
#Compared to balsam fir, black spruce generally shows lower browsing intensity across
#most ecoregions, as moose tend to prefer the more palatable balsam fir over spruce.
#Consequently, the bar heights for black spruce are typically lower than those of balsam fir
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# 20a: The SaplingStudy dataset is not evenly distributed. Certain species like Balsam Fir are overrepresented, while others may be underrepresented, and sampling effort varies by ecoregion

# 20b: Recognizing bias is important because overrepresented species or uneven spatial coverage can lead to skewed conclusions, overestimating browsing on common trees and missing patterns in underrepresented areas, leading to inaccurate conclusions.

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE),
    mean_MooseDensity = mean(MooseDensity, na.rm = TRUE))

print(sum_spe_browse)
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#Yes, the figure supports this hypothesis if the lines representing preferred species start high but level off, while less preferred species show lower, increasing, or flatter lines at low density that rise as total browsing intensity increases at higher moose densities. This suggests that as preferred food becomes scarce at higher moose densities, they broaden their diet to include less palatable species.

#The species with the highest color coded points indicates the primary food source, while the lowest points represent avoided or less browsed food.

#Species not shown are likely those that are highly toxic, not edible, or not found within the specific ecoregions sampled in the data.

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020,human_pop, study_sites)

moose_coll2 <- moose_coll %>% 
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")

plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density", ylab = "Collisions (2020)", 
     main = "Moose Density vs. Collisions")
# Trend: There is a general positive trend where higher moose density correlates with more collisions. 
# Outliers: Some regions may show high collisions despite low density, likely due to higher traffic volume

coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population", ylab = "Collisions Per Capita",
     main = "Human Population vs. Per Capita Collisions")
#In the per capita plot, you will see a negative relationship. As the human population increases, the collisions per capita tend to decrease. This trend makes sense because ecoregions with very small human populations are often rural or wilderness areas (like newfoundland) with higher moose-to-human ratios, resulting in a higher likelihood that any single person will experience a collision compared to someone in a densely populated place. 