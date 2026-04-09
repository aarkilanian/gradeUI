# BIOL 1002 R ASSIGNMENT
# Jade Hodder
# Student number: 202412489
# Date: 2026-02-09
# Part I: Moose Populations in Newfoundland

#[Q.1]
library(dplyr)

#[Q.2]
moosedata <- read.csv("MoosePopulation.csv")

#[Q.3]
View(moosedata)
moose_clean <- na.omit(moosedata)
View(moose_clean)

#[Q.4]
moose_sel <- select(moose_clean,
                    Ecoregion,
                    Year,
                    Area,
                    Estimated_Moose_Pop)

#[Q.5a]
year_min <- min(moose_sel$Year)

#[Q.5b]
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#[Q.6]
moosedata2 <- mutate(moose_sel,
                     MooseDensity = Estimated_Moose_Pop / Area)

#[Q.7]
plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")

#[Q.8a]
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

View(moose_west)

#[Q.8b]
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Western Forests Over Time")

#[Q.9a]
moose_2020 <- filter(moosedata2, Year == 2020)

View(moose_2020)

#[Q.9b]
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

View(moose_2020_high)

#[Q.9c]
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

View(moose_2020_high_byD)

#[Q.10]

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Part II: Tree Sapling Study
#[Q.11a]
saplings <- read.csv("SaplingStudy.csv")

View(saplings)

#[Q.11b]
sap_clean <- na.omit(saplings)

View(sap_clean)

#[Q.12a]
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()

#[Q.12b]
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(mean_BrowsingScore))

print(avg_browse_reg)

#[Q.13a]
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_Height = mean(Height)) %>%
  print()

#[Q.13b]
sap_reg_height_low <- sap_reg_height %>%
  filter(mean_Height < 20)

print(sap_reg_height_low)


#[Q.14a]
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()

#[Q.14b]
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(mean_BrowsingScore))

print(avg_browse_spe)


#[Q.15]
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()

#[Q.16]
barplot(fir_reg_browse$mean_BrowsingScore,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Moose Browsing Score",
        main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

#[Q.17a]
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()

#[Q.17b]
barplot(spruce_reg_browse$mean_BrowsingScore,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Moose Browsing Score",
        main = "Average Black Spruce Browsing Intensity by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)

#[Q.17c]
# Comparing Black Spruce to Balsam Fir browsing across ecoregions:
# In general, Balsam Fir has higher average browsing scores than Black Spruce 
# across most ecoregions, indicating that moose prefer Balsam Fir over Black Spruce.

#[Q.18]
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

#[Q.19]
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#[Q.20a]
# The SaplingStudy dataset is not evenly distributed. 
# Some ecoregions, such as Western_Forests, have more saplings counted, 
# while others, like Avalon_Forest, have fewer, indicating over and underrepresentation. 
# Similarly, certain species like Balsam_Fir are more frequently sampled than species like White_Spruce.

#[Q.20b]
# Recognizing bias in ecological datasets is important because uneven sampling can 
# misrepresent real patterns in the ecosystem, leading to incorrect conclusions about 
# species abundance or behavior. Accounting for bias ensures more accurate and reliable interpretations.

# Part III: Creating and Joining Datasets
#[Q.21a]

moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%              
  mutate(MooseDensity = Estimated_Moose_Pop / Area) 

View(moose_2020b)

#[Q.21b]
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")

View(moose_sap)

#[Q.22]
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore),
            mean_MooseDensity = mean(MooseDensity)) %>%
  print()

#[Q.23a]
# The figure suggests that at low moose densities, browsing is more selective, with 
# certain species being preferred (higher average browsing scores). At higher moose densities, 
# browsing becomes more uniform across species, supporting the researchers’ hypothesis.

#[Q.23b]
# Moose favor Balsam Fir the most, as it has the highest average browsing scores across ecoregions. 
# Black Spruce is browsed the least, showing the lowest average browsing intensity.

#[Q.23c]
# White Spruce is not shown on the figure, likely because there were too few observations 
# in 2020 or missing data, so an average browsing score could not be calculated for this species.

#[Q.24]

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens",
                 "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                 "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

View(moose_coll)

#[Q.25a]
library(dplyr)

moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

View(moose_coll2)

#[Q.25b]
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

View(coll_merge)

#[Q.26a]
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density (per sq km)",
     ylab = "Number of Moose-Vehicle Collisions",
     main = "Relationship Between Moose Density and Vehicle Collisions",
     pch = 16,         
     col = "darkorange")

#[Q.26b]
# The scatterplot gives a positive relationship: regions with higher moose density 
# tend to have more moose vehicle collisions. Some regions, like Avalon_Forests, appear 
# as outliers with very high collision numbers relative to density.

#[Q.27]

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

View(coll_merge_per_capita)

#[Q.28]
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions per Capita",
     main = "Relationship Between Human Population and Moose Collisions per Capita",
     pch = 16,     
     col = "steelblue")

#[Q.29]
# The scatterplot suggests that smaller human populations tend to have higher moose collisions per capita, 
# while larger populations have lower collision rate per person. This makes sense because areas with few people 
# but high moose densities experience more collisions relative to the number of residents.

#ALL DONE!


