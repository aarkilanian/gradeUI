# Name: Mya Lundrigan
# Bio R Asiignment 1002
# Feb 9th 2026
# Part 1 Moose population in Newfoundand and Labrador
# Question 1:
install.packages("dplyr")
library(dplyr)
#Question 2:  
moosedata <- read.csv("Moosepopulation.csv")
#question 3:
View(moosedata)
moose_clean <- na.omit(moosedata)
View(moose_clean)
#question 4:
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_clean <- seperate(moose_clean, 1, into = c("Ecoregion", "Year", "Area", "Estimated_Moose_Pop", "Observation_Method", "Data_Source"), sep = "")
#Question 5A
year_min <- min(moose_sel$Year)
#The oldest year is 1904
#Question 5B
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#The highest estimated moose population is 4125
#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
#Question 8A
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Question 8B
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests over time")
#Question 9A 
moose_2020 <- filter(moosedata2, Year == 2020)
#Question 9B
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#Question 9C
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity))
#Part 2: Tree Sapling Study
#Question 11A
saplings <- read.csv("saplingStudy-2.csv")
#Question 11B
sap_clean <- na.omit(saplings)
#Question 12A
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_reg_browse)
#Question 12B
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
print(avg_browse_reg)
#Highest Browsing: Northern_Peninsula_Forests
#Lowest Browsing: traitOfBelleIsleBarrens
#Question 13A
sap_reg_height <- sap_clean %>% 
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height))
print(sap_reg_height)
#Question 13B
sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20)
print(sap_reg_height_low)
# Northern_Peninsula_Forests and Western_Forests have (avg height < 20 cm)
#Question 14A
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_spe_browse)
#Question 14B
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
print(avg_browse_spe)
#Highest Browsing: "Black_Ash"
#Lowest Browsing: "Black_Spruce"
#Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
print(fir_reg_browse)
#Question 16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing intensity",
        main = "Balsam Fir browsing by ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
#Question 17A
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
           group_by(Ecoregion) %>%
           summarize(AverageBrowsing = mean(BrowsingScore))
#Question 17B
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing intensity",
        main = "Black Spruce browsing by ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
#Question 17C
#Black Spruce browsing varies strongly by ecoregion. 
#It is lowest in Avalon Forests and highest in the Northern Peninsula
#Compared to Balsam Fir, Black Spruce shows lower browsing overall
#Question 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally()
print(sap_reg_tally)
#Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()
print(sap_spe_tally)
#Question 20A
#This dataset is not evenly distributed.
#Balsam Fir is overrepresented while Black Ash is heavily underrepresented.
#Which could bias interprtations of browsing patterns 
#Question 20B
#Recognizing Bias is important because uneven sampling can lead to misleading ecological conclusions
#Results may reflect sampling effort rather than true browsing pressure.
#Part 3: Creating and Joining datasets
#Question 21A
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
#Question 21B
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
print(moose_sap)
#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  )
print(sum_spe_browse)
#Question 23
install.packages("ggplot2")
library(ggplot2)
ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Browsing Intensity Across Moose Density by Species",
    x = "Average Moose Density",
    y = "Average Browsing Score"
  )
 #Question 23A 
#Yes, the figure supports the hypothesis. 
#At low moose density browsing varies more by species, but at higher density browsing becomes more uniform.
#Question 23B
#Moose appear to favor Willow and Balsam Fir the most, while Black Spruce is browsed the least.
#Question 23C
#Black Ash is not clearly shown because there are very few samples and with limited data it does not appear on the figure.
#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 27000, 5000)
study_sites <- c(
  "North_Shore_Forests",
  "Northern_Peninsula_Forests",
  "Avalon_Forests",
  "Eastern_Hyper_Oceanic_Barrens",
  "Maritime_Barrens",
  "Western_Forests",
  "Central_Newfoundland_Forests",
  "Long_Range_Mountains",
  "Southern_Coastal_Barrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
print(moose_coll)
#Question 25A
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
#Question 25B
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
print(coll_merge)
#Question 26A
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions",
     main = "Moose Density vs Collisions")
#Question 26B
#Overall, there is a general positive relationship: higher moose density tends to be associated with more collisions, collisions increase as moose density increases.
#There is an outlier where moose density is high(~2.5) but collisions are low (~10) and a couple points a high density with very high collisions.
#Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)
arrange(coll_merge_per_capita, desc(coll_per_capita))
#The Ecoregions with the highest moose collisions are Northern Peninusula and Avalon Forests.
#Question 28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Population")
#Question 29
#Regions with smaller populations have higher collisions per person.
#This makes sense because rural areas have more moose exposure. 