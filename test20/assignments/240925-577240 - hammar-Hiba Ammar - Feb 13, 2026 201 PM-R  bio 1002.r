#hiba
#Q1
install.packages("dplyr")
library(dplyr)
#Q2
moosedata <- read.csv("MoosePopulation.csv")
#Q3
moose_clean <- na.omit(moosedata)
#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Q5
min('Year')
#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#Q8 a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#b
plot(moose_west$Year,moose_west$MooseDensity, 
      
      xlab = "year", 
          ylab = "Moose per sq km", 
          main = "Moose density in Newfoundland ecoregions over time")
#Q9 a
moose_2020<- moosedata2 %>%  filter(Year== "2020")
#b
moose_2020_high <- moosedata2 %>%
  filter(MooseDensity > 2.0)                   #c           
moose_2020_high_byD <-moosedata2 %>%  arrange(desc(MooseDensity))
#Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Q11 a
saplings <- read.csv("SaplingStudy.csv")
#b
sap_clean<- na.omit(saplings)
#Q12 a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
  print(avg_browse_reg)
#Q13 a
sap_reg_height <- sap_clean %>%
    group_by(Ecoregion) %>%
    summarize(AverageHeight = mean(Height)) %>%
    print()
b  
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
#14 a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
b
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Q15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%       
  group_by(Ecoregion) %>%                   
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#Q16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Balsam Fir Browsing Score",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
#Q17 a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%   
  group_by(Ecoregion) %>%                 
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
b
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion, las=2,
        xlab = "Ecoregion",
        ylab = "Average Black Spruce Browsing Score",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
c
# Black Spruce generally has lower browsing scores than Balsam Fir across most ecoregions, suggesting moose prefer browsing Balsam Fir. Some ecoregions show similar browsing intensity for both species.
#Q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#Q19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#Q20 a
# The SaplingStudy dataset is not evenly distributed. Some ecoregions and tree species are overrepresented, while others have fewer samples, which may bias the interpretation of moose browsing patterns.
b
# Recognizing bias is important because uneven sampling can lead to incorrect conclusions about moose browsing patterns. If some regions or species are sampled more than others, the results may not accurately represent the true ecosystem.
#Q21 a
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
#Q22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()
#Q23
# The data suggest that at low moose density, browsing is concentrated on preferred species, while at higher density, moose feed more evenly across species, supporting the hypothesis that browsing shifts from selective to generalist with increasing density.
b
# Moose favour Balsam Fir the most, as it consistently has the highest average browsing scores, and browse Black Spruce the least, which has the lowest scores across ecoregions.
c
# Moose favour Balsam Fir the most, as it consistently has the highest average browsing scores, and browse Black Spruce the least, which has the lowest scores across ecoregions.
#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c(
  "North_Shore_Forests",
  "Northern_Peninsula_Forests",
  "Long_Range_Barrens",
  "Central_Forests",
  "Western_Forests",
  "EasternHyperOceanicBarrens",
  "Maritime_Barrens",
  "Avalon_Forests",
  "StraitOfBelleIsleBarrens"
)
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25 a 
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
b
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion", relationship = "many-to-many")
#Q26 a
plot(coll_merge$MooseDensity,coll_merge$collisions2020,xlab = "Density of moose",ylab = "collisions")
b
# The scatter plot suggests that as moose density increases, the number of collisions generally increases. This indicates a positive relationship between moose density and collision frequency across the study sites.
#Q27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop) %>%
  print()
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita)) %>%
  select(Ecoregion, coll_per_capita)
#Q28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Moose Collisions per Person vs Human Population",
     pch = 19,         # solid circle points
     col = "darkblue") # optional color for points
#Q29
# The scatterplot shows that collisions per capita tend to be higher in areas with smaller human populations, while regions with larger populations have lower collisions per person. This makes sense because moose are more abundant in rural and less populated areas, increasing the likelihood of collisions relative to the number of people.


