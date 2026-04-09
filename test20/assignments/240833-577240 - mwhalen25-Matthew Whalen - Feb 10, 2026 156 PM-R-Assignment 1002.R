install.packages("dplyr")
library("dplyr")
MoosePopulation
moose_clean <- na.omit(MoosePopulation)
moose_clean
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(year(MoosePopulation))
year_min <- min(dataset$Year)
year_min <- min(MoosePopulation$Year)
year_min <- min(moose_clean$Year, na.rm = TRUE)
moose_max <- max(moose_clean$Estimated_Moose_Pop, na.rm = TRUE)
year_min <- min(dataset$Year, na.rm = TRUE)
moose_max <- max(MoosePopulation$Estimated_Moose_Pop, na.rm = TRUE)
year_min
moose_max
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot( moosedata2$Year, moosedata2$MooseDensity, type = l,
      xlab = "year", 
      ylab = "Moose per sq km", 
      main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",                         
     xlab = "Year",                      
     ylab = "Moose per sq km",           
     main = "Moose Density Over Time in Western Forests Ecoregion")
moose_2020 <- filter(moosedata2, 2020)
names(moosedata2)
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020
view(moose_2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%             
  filter(MooseDensity > 2.0) %>%       
  arrange(desc(MooseDensity)) %>%      
  print()                              
view(moosefinal)
view(moose_2020_high_byD)
saplings <- SaplingStudy
sap_clean <- na.omit(saplings)
view(sap_clean)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%                                  
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%  
  print()                                                  
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%                       
  print()                                                  
# Highest browsing score is Northern Peninsula Forest and lowest is strait of belle isle barrens
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%                        
  summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>%  
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%     
  print()    
#These ecoregions have saplings with an average height less than 20cm, therefore, severely browsed by moose
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%                               
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print() 
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%   
  print()
# Black Ash has the most browsing and Black Spruce has the least browsing 
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%                
  group_by(Ecoregion) %>%                            
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%  
  print()  
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Moose Browsing Score",
        main = "Average Moose Browsing on Balsam Fir Across Ecoregions",
        col = "forestgreen",    
        cex.names = 0.6)       
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%                 
  group_by(Ecoregion) %>%                               
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%  
  print()
par(las = 2)  # rotate x-axis labels vertically
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Moose Browsing Score",
        main = "Average Moose Browsing on Black Spruce Across Ecoregions",
        col = "steelblue",
        cex.names = 0.6)
#Balsam fir generally experiences higher browsing intensity compared to the Black Spruce. This indicates that the Balsam fir is preferred by moose compared to the Black Spruce
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%     
  tally() %>%               
  print()
#the Sampling Study is not evenly distributed
#Some ecoregions are overrepresetned like North Shore Forests while other are underrepresented like Straight of belle isle barrens
#it's important to recognize bias in ecological datasets because uneven sampling can misrepresent moose browsing patterns or lead to inaccurate conclusions about which species or ecoregions are most affected
moose_sap <- left_join(moose_2020, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%                       
  summarize(
    AverageBrowsing = mean(BrowsingScore, na.rm = TRUE),  
    AverageMooseDensity = mean(MooseDensity, na.rm = TRUE) 
  ) %>%
  print()  
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AverageMooseDensity, y = AverageBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#At low moose populations, the browsing intensity is higher for certain preferred species
#At high moose populations, the browsing intensity is more even, showing how they go from preferred eating to general eating
#Moose appear to favour species with the highest average browsing scores, such as Alder and Willow, and they browse species like Black Spruce the least 
#Black Ash is not shown on the figure because there wasnt enough observatons recorded in 2020
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
view(moose_coll)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion", relationship = "many-to-many")
print(coll_merge)
moose_coll2
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (per sq km)",
     ylab = "Number of Moose-Vehicle Collisions (2020)",
     main = "Relationship Between Moose Density and Vehicle Collisions",
     pch = 19,           
     col = "red")    
#Higher moose densities, generally tend to be associated with more collisions
#One outlier would be at 1 moose per sq km with over 100 collisions
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop) %>%   # collisions per person
  print()
view(coll_merge_per_capita)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population",
     pch = 19,        
     col = "blue") 
# The scatterplot shows that regions with smaller human populations tend to have higher moose collisions per capita and more densely populated areas have lower collision rates. 
# This makes sense because in sparsely populated areas, moose are more plentiful compared to humans, increasing the likelihood of collisions
