# BIOLOGY 1002 R ASSIGNMENT
# NAME: CHEYANNE COFFIN
# DATE: 01-15-2026

# load libraries needed
library(dplyr)

# set working directory
setwd("C:/Users/cheya/OneDrive/Documents/BIO 1002 R ASSIGNMENT")

# Q2
moosedata <- read.csv("MoosePopulation.csv")

# Q3
View(moosedata)

moose_clean <- na.omit(moosedata)

#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5
min(moosedata$Year)
year_min <- min(moosedata$Year)
# 1904

max(moosedata$Estimated_Moose_Pop)
moose_max <- max(moosedata$Estimated_Moose_Pop)
# 41250
# This was for the specfic ecoregion, Central Forests

#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Q8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity,
  type = "l",
  xlab = "Year",
  ylab = "Moose per sq km",
  main = "Moose density over time in Western Forests ecoregion")

#Q9
moose_2020 <- filter(moosedata2, Year == 2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
   print()

#Q11

setwd("C:/Users/cheya/OneDrive/Documents/BIO 1002 R ASSIGNMENT")

library(dplyr)

saplings <-read.csv("SaplingStudy.csv")

View(saplings)

sap_clean <- na.omit(saplings)


#Q12
sap_reg_browse <-sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
              print()

avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#The ecoregion with the most moose browsing is Northern Peninsula Forests
#The ecoregion with the lowest moose browsing is Strait Of Belle Isle Barrens

#Q13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(MeanHeight = mean(Height) ) %>%
  print()

sap_reg_height_low <- sap_reg_height %>%
  filter(MeanHeight < 20) %>%
  print()

# The ecoregions that have average heights of less than 20cm are the Northern Peninsula Forests and the Western Forests

#Q14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(MeanBrowsing)) %>%
  print()

# The species with the highest browsing score is the Black Ash
# The species with the lowest browsing score is Black Spruce

#Q15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore))
print(fir_reg_browse)

#Q16
barplot(fir_reg_browse$MeanBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Balsam Fir by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)

#Q17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore))
print(spruce_reg_browse)

barplot(spruce_reg_browse$MeanBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Moose Browsing on Black Spruce by Ecoregion",
        col = "maroon",
        cex.names = 0.6)
# Compared to Balsam Fir, Black Spruce shows a lower browsing intensity
# across most ecoregions, showing that it may be less preferred by the moose.

#Q18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
print()

#Q19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
tally() %>%
print()
# The same number of saplings sampled is not equal across ecoregions.

#Q20
#a) The sapling study dataset is not evenly distrubuted.
# Some ecoregions like and species like the Northern Peninsula Forests and Balsam Fir
# are more heavily sampled than others.

#b) Recognizing bias is important in ecological datasets because the uneven
# sampling can provide and distort wrong information and lead to incorrect
# conclusions about the browsing preferences.

#Q21
library(dplyr)

moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Q22
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
          AvgDensity = mean(MooseDensity, na.rm = TRUE))
print(sum_spe_browse)

#Q23
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

# a) Yes the figure supports the researchers hypothesis. At lower moose
# densities, the browsing intensity differs strongly among species.
# While at higher densities, browsing becomes more consistent.

# b) The moose appear to favor the species Willow and Alder, which shows
# the highest average browsing scores across moose densities. Black Spruce 
# is browsed the least, compared to the other species

# c) The black spruce species is not shown on the figure because it has
# little to no recorded browsing.

#Q24
library(dplyr)
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion", relationship = "many-to-many")

#Q26 

# a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
xlab = "Moose Density",
ylab = "Number of Collisions (2020)",
main = "Moose Density vs Moose-Vehicle Collisions")

# b) It seems as if places with higher moose density are experiencing more moose vehicle collisions.
# While areas with a lower moose density are seeing less moose vehicle collisions.

#Q27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

#Q28
plot(
  coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
  xlab = "Human Population",
  ylab = "Collisions Per Capita",
  main = "Collisions Per Capita vs Human Population")

#Q29
# Regions with a smaller human population, tend to have higher collisions per capita,
# probably because the moose density remains high and the moose have less barriers between
# the forest and roads.
