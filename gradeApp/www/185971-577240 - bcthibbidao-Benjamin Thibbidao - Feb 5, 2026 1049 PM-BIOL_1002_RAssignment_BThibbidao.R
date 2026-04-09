#Part 1: Moose Populations
#Question 1:
library(dplyr)

#Question 2:
moosedata <- read.csv("MoosePopulation.csv")

#Question 3:
moose_clean <- na.omit(moosedata)

#Question 4:
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5:
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#Question 6:
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7:
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8:
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland western forests over time")

#Question 9:
moose_2020 <- filter(moosedata2, Year==2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10:
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Part 2: Tree Sapling Study
#Question 11:
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

#Question 12:
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
#Highest: Northern Peninsula Forests, Lowest: Strait of Bell Isle Barrens

#Question 13:
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- filter(sap_reg_height, AverageHeight < 20.0)
#Regions less than 20cm: Northern Peninsula Forests and Western Forests

#Question 14: 
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(BrowsingScore = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(BrowsingScore))
#Species with highest browsing:Black Ash, lowest:Black Spruce

#Question 15:
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore = mean(BrowsingScore)) %>%
  print()

#Question 16:
barplot(fir_reg_browse$BrowsingScore, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score",
        main = "Average Browsing Score of Balsam Fir for each Ecoregion in Newfoundland", 
        col = "forestgreen", cex.names = 0.6)

#Question 17:
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore = mean(BrowsingScore)) %>%
  print()

barplot(spruce_reg_browse$BrowsingScore, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score",
        main = "Average Browsing Score of Black Spruce for each Ecoregion in Newfoundland", 
        col = "forestgreen", cex.names = 0.5)
# Black Spruce has a lower average browsing score than Balsam Fir in all Ecoregions
# apart from the Long Range Barrens. Black Spruce browsing does not occur in the Eastern
# Hyper Oceanic Barrens or Maritime Barrens

#Question 18:
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Question 19:
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Question 20:
# a) The strait of Bell Isle and Maritime Barrens are underrepresented Ecoregions
# and the Black Ash tree was an underrepresented Species
# b) Bias is important to be recognized because all ecoregions are connected with each
# other and we must get the same amount of data from all of them to understand the full picture

# Part 3: Creating and Joining Datasets

#Question 21:
moose_2020b <- filter(moose_clean, Year==2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', 
                       relationship = "many-to-many")

#Question 22:
sum_spe_browse <- moose_sap %>%
  group_by(Ecoregion, Species) %>%
  summarize(BrowsingScore = mean(BrowsingScore),
            MooseDensity = mean(MooseDensity)) %>%
  print()

#Question 23:
library(ggplot2)

ggplot(sum_spe_browse, aes(x = MooseDensity, y = BrowsingScore, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#a) The researchers' hypothesis appears correct. The areas with higher moose population
# density appear to have a wider variety of plants being eaten
#b) Moose seem to favour willow and least favour black spruce
#c) The sapling not shown is Black Ash because it has the same moose density and 
# browsing score as alder in the western forests

#Question 24:
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests",
                 "Western_Forests","EasternHyperOceanicBarrens",
                 "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25:
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)

coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', 
                       relationship = "many-to-many")

#Question 26: 
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose per sq km", 
     ylab = "Collsions in 2020", 
     main = "Traffic collisions involving moose and their respective population density")
# In general, a higher moose population density leads to more collisions. One outlier
# is the Avalon forests. The higher collisions are likely due to the high human population

#Question 27:
coll_merge_per_capita <- mutate(coll_merge, 
                                coll_per_capita = collisions2020 / human_pop)

#Question 28: 
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop,
     xlab = "Collisions per capita", 
     ylab = "Human Population", 
     main = "Collisions per capita compared to local human population" )

#Question 29: 
# This data does make sense. Because the Avalon has such a large population, it will
# still have a smaller number of collisions per capita than more sparsely populated areas.
