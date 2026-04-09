install.packages("dplr")
library(dplyr)
Moosedata <- read.csv("C:/Users/sierr/Downloads/MoosePopulation.csv")
View(Moosedata)
na.omit(Moosedata)
Moosedata <- na.omit(Moosedata)
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(Moosedata$Year) #1904
max(Moosedata$Estimated_Moose_Pop) #41250
mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)
plot(Moosedata$Year, Moosedata$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")
View(MooseDataWest)
plot(MooseDataWest$Year, MooseDataWest$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in the Western Forests ecoregion over time") 
MooseData_2020 <- filter(Moosedata, Year == 2020)
View(MooseData_2020)
MooseData_2020_A <- filter(MooseData_2020, Estimated_Moose_Pop >=2.0)
Moosedata <- arrange(Moosedata, desc(Estimated_Moose_Pop))
Moosedata_Final <- Moosedata %>%
  print()

MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(Estimated_Moose_Pop > 2.0) %>%
  arrange(desc(Estimated_Moose_Pop)) %>%
  print()
saplings <- read.csv("C:/Users/sierr/Downloads/SaplingStudy.csv")
View(saplings)
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_browse = mean(BrowsingScore))

print(sap_reg_browse)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height))
# Ecoregions with severe browsing (average height < 20 cm)
sap_reg_height_low <- sap_reg_height %>%
  filter(mean_height < 20)
print(sap_reg_height_low)
# Average browsing score by species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_browse = mean(BrowsingScore))

print(sap_spe_browse)
# Rearrange species by decreasing mean browsing score
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(mean_browse))

print(avg_browse_spe)
# "Black_Ash" species has the highest browsing score
# "Black_Spruce" species has the lowest browsing score
library(dplyr)

# Average browsing score for Balsam Fir by Ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browse = mean(BrowsingScore))

print(fir_reg_browse)
# Barplot of Balsam Fir browsing by ecoregion
barplot(fir_reg_browse$mean_browse,              # Y-axis: average browsing
        names.arg = fir_reg_browse$Ecoregion,    # X-axis: ecoregions
        xlab = "Ecoregion",                      # Label for X-axis
        ylab = "average browsing intensity",   # Label for Y-axis
        main = "Average Balsam Fir Browsing by Ecoregion", # Plot title
        col = "pink",                      # Color of the bars
        cex.names = 0.6)                          # Reduce X-axis label size for readability
# Average browsing score for Black Spruce by Ecoregion
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browse = mean(BrowsingScore))
# Barplot of Black Spruce browsing by ecoregion
barplot(spruce_reg_browse$mean_browse,          # Y-axis: average browsing
        names.arg = spruce_reg_browse$Ecoregion, # X-axis: ecoregions
        xlab = "Ecoregion",                      # Label for X-axis
        ylab = "average browsing intensity",   # Label for Y-axis
        main = "Average Black Spruce Browsing by Ecoregion", # Plot title
        col = "blue",                        # Color of the bars
        cex.names = 0.6)                          # Reduce X-axis label size
#The Black Spruce ecoregions are lower in average browsing intensity than the Balsam Fur ecoregions.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#question20
#North_Shore_Forests species has 8 samples where as the StraitOfBelleIsleBarrens species only has 1 sample. The North_Shore_Forests are over represented and the StraitOfBelleIsleBarrens are under represented. Other Species have 5,3 or 7 so no the dataset is not evenly distributed.
#it important to recognize bias in ecological datasets because it can lead to incorrect conclusions which will provide wrong information learned from the study. This could lead to further research or conservation on the wrong species or area.

moose_2020b <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
head(moose_2020b)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
View(moose_sap)
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE),
    mean_MooseDensity = mean(MooseDensity, na.rm = TRUE))

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#Question23
#Yes, there is evidence supporting the researchers hypothesis. At low moose density, browsing scores are high for only a few species like Willow and Alder, but at higher densities browsing intensity increases in more species. 
#Moose favour Black_Ash and Alder the least, while favoring Balsam_Fur, Black_Spruce, White_Burch and Willow the most. 
#Black_Ash is missing from the figure because it's score was 0.

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
View(moose_coll)

moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", .cols = "study_sites")  
coll_merge <- left_join(MooseData_2020, moose_coll2, by = "Ecoregion")


moose_2020b <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")
plot_data <- coll_merge %>%
  
  filter(!is.na(MooseDensity) & !is.na(collisions2020))
plot(plot_data$MooseDensity, plot_data$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Scatterplot of Moose Density vs Collisions (2020)",
     pch = 19,       # solid circle points
     col = "lightpink")
abline(lm(collisions2020 ~ MooseDensity, data = plot_data), 
       col = "yellow", lwd = 2)
#There is a  outlier where the moose density is 1. There is a higher number of collisions than when the moose density is greater than 1.
#The trend is higher moose density leads to more moose-vehicle collisions, meaning that as moose population increases, there are more collisions. 

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Scatterplot of Collisions per Capita vs Human Population",
     pch = 19,       
     col = "purple")
#The scatterplot shows there tends to be more collisions per capita when the population is lower. This doesn't make sense according to human and moose populations in Newfoundland because if there is more humans there is a higher risk of human-moose collisions. 

