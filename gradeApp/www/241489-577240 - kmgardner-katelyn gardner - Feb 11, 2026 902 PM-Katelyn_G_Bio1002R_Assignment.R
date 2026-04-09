moose_data <- read.csv("MoosePopulation.csv")
sapling_data <- read.csv("SaplingStudy.csv")
install.packages("dplyr")
library(dplyr)
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
str(moose_clean)
year_min<-min(moose_clean$Year, na.rm = TRUE)
moose_max<-max(moose_clean$Year, na.rm = TRUE)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_data$Year, moose_data$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
moose_2020<-filter(moosedata2, Year == 2020)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
names(moosedata2)
moosedata2<- moosedata2 %>% 
  mutate(MooseDensity = as.numeric(Estimated_Moose_Pop)/ as.numeric(Area))
head(moosedata2$MooseDensity)
moose2020_high<-filter(moose_2020, MooseDensity >2.0)
moose2020_high_byD <-arrange(moose2020_high, desc(MooseDensity))
moosefinal<-moosedata2 %>%
  filter(Year==2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
sapling<-read.csv("SaplingStudy.csv")
na.omit(sapling)
sap_clean<-na.omit(sapling)
library(dplyr)
 names(sap_clean)
sap_ref_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
avg_browse_reg <-sap_clean%>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
avg_browse_reg<- sap_reg_browse %>%
  arrange(desc(Averagebrowsing))
print(avg_browse_reg)
#Nothern_Peninsula_forests has the highest average browsing
#StraightOfBelleIslebarrens has the lowest average browsing
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion)%>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE))
print(sap_reg_height)
sap_reg_height_low <-sap_reg_height %>%
  filter(AverageHeight <20)
print(sap_reg_height_low)
#Ecoregions in sap_reg_height_low have average tree heights  < 20cm (severly broswed)
library(dplyr)
names(sap_clean)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(Browsingscore, na.rm = TRUE))
print(sap_spe_browse)
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrwosing))
print(avg_browse_spe)
unique(sap_clean$Species)
fir_reg_browse <-sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
print(fir_reg_browse)
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Balsam Fir browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
spruce_reg_browse <- sap_clean%>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(Averagebrowsing = mean(BrowsingScore, na.rm = TRUE))
print(spruce_reg_browse)
barplot(spruce_reg_browse$Averagebrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
#Black spruce generally has lower browsing intesnity than Balsam Fir across most ecoregions 
#Blasam Fir appears to be more heavily browsed, especially in North_Shore_Forests
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally()
print(sap_reg_tally)
#The number of saplings counted differa among ecoregions, so the dataset is not evenly distributed. 
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()
print(sap_spe_tally)
# The SaplingStudy dataset is not evenly distributed
#some ecoregions and species have more sampled saplings than others 
moose2020b <- moose_clean %>%
  filter(Year == 2020)
names(moose_clean)
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop /Area)
print(moose_2020b)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
print(moose_sap)
sum_spe_browse <- moose_sap %>%
  group_by (Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
    AvgDensity = mean(MooseDensity, na.rm = TRUE)
  )
  print(sum_spe_browse)
#There is partil evidence supporting the hypothesis. At lower moose densities, browsing, varies more among species, whileat higher densities browsing scores become more uniformlyhigh, suggesting less selective feeding. 
  #willow and alder appear to be the most heavilybrowsed species, while black spruce generally has the lowest browsing intensity across ecoregions 
  #black ash is not shown because it is likely had insufficient data across ecoregions or did not appear in the summarized dataset used for the plot
  collisions2020 <- c(56, 60, 14, 36, 48, 18, 40, 110, 6)
  human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
  study_sites <- c("North_Shore_Forests",
                   "Northern_Peninsula_Forests",
                   "Long_Range_Barrens",
                   "Avalon_Forests",
                   "Central_Forests",
                   "Eastern_HyperOceanicBarrens",
                   "Maritime_Barrens",
                  "Western_Forests",
                  "StraightOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)  
print(moose_coll)
moose_coll <- moose_coll %>%
  rename(Ecoregion = study_sites)
print(moose_coll)
moose_coll_joined <- left_join(moose_coll, moose_2020b, by = "Ecoregion")
print(moose_coll_joined)
plot(moose_coll_joined$MooseDensity,
     moose_coll_joined$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Moose-vehicle Collisions (2020)",
     main = "Moose density vs Collisions",
     col = "darkblue")
# there appears to be a positive relationship between moose desnity and the number of moose-vehicle collisions
# regions with higher moose desnsity tend to have more collisions with one region showing especially high collisions that may be an outlier
coll_merge_per_capita <- moose_coll_joined %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions Per capita",
     main = "Collisions Per Capita vs Human Population",
     col = "darkred",
     pch = 19) 
# regions with smaller human populations generally have higher collisions per capita
# this aligns with higher moose density in rural areas of Newfoundland