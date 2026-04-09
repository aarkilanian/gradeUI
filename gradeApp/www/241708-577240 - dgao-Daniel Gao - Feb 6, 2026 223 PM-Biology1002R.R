install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("C:/Users/Daniel/OneDrive/Desktop/BIOL1002_Rassignment/MoosePopulation.csv")
View(moosedata)
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min = min(moose_clean$Year) #1904 
moose_max = max(moose_clean$Estimated_Moose_Pop) #41250
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time",
     type = "l")
moose_2020 <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()





saplings <- read.csv("C:/Users/Daniel/OneDrive/Desktop/BIOL1002_Rassignment/SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_ecoregion <- sap_clean %>% 
  group_by(Ecoregion)%>%
  summarize(mean_browsing_score = mean(BrowsingScore, na.rm = TRUE))%>%
  print()
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion)%>%
  summarize(AverageBrowsing = mean(BrowsingScore))%>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  group_by(Ecoregion) %>%
  arrange(desc(AverageBrowsing)) 
print(avg_browse_reg) 
# min. height: 1 
# max. height: 4.57
sap_reg_height <- sap_clean %>% 
  group_by(Ecoregion)%>%
  summarize(AverageHeight = mean(Height))%>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  group_by(Ecoregion)%>%
  filter(AverageHeight < 20)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))%>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  group_by(Species) %>%
  arrange(desc(AverageBrowsing))
# Highest browsing: Black_Ash 
# Lowest browsing: Black_Spruce
fir_reg_browse <- sap_clean %>% 
  filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_browsing = mean(BrowsingScore))
barplot(fir_reg_browse$mean_browsing, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "mean_browsing", 
        main = "Balsam Fir browsing intensity in each Ecoregion", 
        col = "forestgreen") 
spruce_reg_browse <- sap_clean %>% 
  filter(Species == "Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_browsing = mean(BrowsingScore))
barplot(spruce_reg_browse$mean_browsing, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "mean_browsing", 
        main = "Black Spruce browsing intensity in each Ecoregion", 
        col = "forestgreen") 
# Black spruce have a higher mean browsing in the Avalon forests and Northern Peninsula forests than balsam fir.   
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
# The tree species are unevenly distributed. Black_Ash is underrepresented and Balsam_Fir is over represented.
# It's important to recognize bias in ecological data sets because it ensures more accuracy and reliability to help us draw scientific conclusions.




moose_2020b <- moose_clean %>% 
  filter(Year == 2020) %>% 
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE),
    mean_MooseDensity  = mean(MooseDensity, na.rm = TRUE)
  )

print(sum_spe_browse)

install.packages("ggplot2")
library(ggplot2)

ggplot(sum_spe_browse, aes(x = mean_MooseDensity, y = mean_BrowsingScore, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#There's evidence shown in the graph that supports the researcher's hypothesis, since average browsing increases as moose density increases.Thus, the range of browsing scores narrows which shows more moose browsing more generally.
#The moose favor Willow trees the most. They favor Black_Spruce the least.
#Black_Ash trees aren't shown in the graph because it has similar point values with and are covered by Willow tree points.  
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density",
     ylab = "Number of Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions (2020)")
# The number of collisions are relatively the same as moose density increases. There are no  outliers. 
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Human Population vs Collisions per Capita (2020)")
# The Human Population vs Collisions per Capita (2020) graph shows that smaller human populations experience more collisions per capita. Thus, moose are responsible for more car crashes in larger human populations.