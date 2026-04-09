# PART I: Moose Populations in Newfoundland -------------------------------
# Question 1 --------------------------------------------------------------
install.packages("dplyr")
library("dplyr")

# Question 2 --------------------------------------------------------------
moosedata <- read.csv(
  "C:/Users/natha/Desktop/School/Bio1002/MoosePopulation.csv")


# Question 3 --------------------------------------------------------------
View(moosedata)
moose_clean <- na.omit(moosedata)


# Question 4 --------------------------------------------------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)


# Question 5a -------------------------------------------------------------
year_min <- min(moose_sel$Year)
print(year_min)

# b
moose_max <- max(moose_sel$Estimated_Moose_Pop)
print(moose_max)


# Question 6 --------------------------------------------------------------
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)


# Question 7 --------------------------------------------------------------

plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")


# Question 8a -------------------------------------------------------------
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# b
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland's western forests over time")


# Question 9a -------------------------------------------------------------

moose_2020 <- filter(moosedata2, Year == "2020")
# b
moose_2020_high <- filter(moose_2020, MooseDensity > 2)
# c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))


# Question 10 -------------------------------------------------------------

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()




# PART II: Tree Sapling Study ---------------------------------------------
# Question 11 -------------------------------------------------------------

saplings <- read.csv(
  "C:/Users/natha/Desktop/School/Bio1002/SaplingStudy.csv")
saplings_clean <- na.omit(saplings)


# Question 12a ------------------------------------------------------------

sap_reg_browse <- saplings_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Northern_Peninsula_Forests had the highest browsing score. 
#StraitOfBelleIsleBarrens had the lowest browsing score.


# Question 13a ------------------------------------------------------------

sap_reg_height <- saplings_clean %>%
  group_by(Ecoregion) %>%
  summarise(Height = mean(Height)) %>%
  print()

# b
sap_reg_height_low <- sap_reg_height %>%
  filter(Height < 20) %>%
  arrange(desc(Height)) %>%
  print()
#Only Northern_Peninsula_Forests and Western_Forests have tree heights lower than 20cm


# Question 14a ------------------------------------------------------------

sap_spe_browse <- saplings_clean %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# b
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Black_Ash has the highest average browsing score
#Black_Spruce has the lowest average browsing score


# Question 15 -------------------------------------------------------------

fir_reg_browse <- saplings_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()


# Question 16 -------------------------------------------------------------

bp_fir <- barplot(fir_reg_browse$AverageBrowsing,
              names.arg = FALSE,
              xlab = "Ecoregions Containing Balsam Fir",
              ylab = "Average Browsing Score",
              main = "Balsam Fir Browsing Score Across Newfoundland's Ecoregions",
              col = "lightgreen",
              ylim = c(0, 5))
text(x = bp_fir,
     y = 0.1,
     labels = fir_reg_browse$Ecoregion,
     srt = 90,
     adj = 0,
     cex = 0.7,
     col = "black")


# Question 17a ------------------------------------------------------------

spruce_reg_browse <- saplings_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# b
bp_spruce <- barplot(spruce_reg_browse$AverageBrowsing,
                  names.arg = FALSE,
                  xlab = "Ecoregions Containing Black Spruce",
                  ylab = "Average Browsing Score",
                  main = "Black Spruce Browsing Score Across Newfoundland's Ecoregions",
                  col = "grey",
                  ylim = c(0, 5))
text(x = bp_spruce,
     y = 0.1,
     labels = spruce_reg_browse$Ecoregion,
     srt = 90,
     adj = 0,
     cex = 0.7,
     col = "black")

# c
# There is more Black Spruce browsing only within the Long_Range_Barrens and Western_Forrests.
# Within every other ecoregion Balsam Firs have higher browsing scores.


# Question 18 -------------------------------------------------------------

sap_reg_tally<- saplings_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  arrange(desc(n)) %>%
  print()


# Question 19 -------------------------------------------------------------

sap_spe_tally <- saplings_clean %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print()


# Question 20a -------------------------------------------------------------

# The SaplingStudy dataset isn't evenly distributed. 
# Balsam_Fir & North_Shore_Forests have the most of samples taken, 11 & 8 respectively.
# When Compared to Black_Ash & StraitOfBelleIsland which only have 1 sample taken.

# b
# Bias with any study can skew results in any direction favouring one side over another.
# So, it is important to gather as many & equal amounts of data as possible for accurate conclusions.




# PART III: Creating and Joining Datasets ---------------------------------
# Question 21a ------------------------------------------------------------

moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()

# b
moose_sap <- left_join(moose_2020b, saplings_clean,
                       by = 'Ecoregion',
                       relationship = "many-to-many")


# Question 22 -------------------------------------------------------------

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(
    AverageBrowsing = mean(BrowsingScore),
    AverageDensity = mean(MooseDensity)
  ) %>%
  print()


# Question 23 -------------------------------------------------------------

install.packages("ggplot2")
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AverageDensity, y = AverageBrowsing, 
  color= Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

# a
# Yes, the evidence supports their hypothesis.
# Since at low moose density there is no browsing activity on certain species, but much higher scores on a few other species.

# b
# Moose clearly favour Willow showing up as their top choice in both low and higher densities.
# Alder follows close behind as their second favourite.

# c
# Black_Ash doesn't show up on the chart.
# This is because it shares the same position as one of the averages for willow.
# It is there, but the willow point overlaps it, and since it is the only Black_Ash data point it cant be seen on the rest of the chart.


# 6.0.1 Moose-vehicle collisions ------------------------------------------
# Question 24 -------------------------------------------------------------

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests",
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


# Question 25a ------------------------------------------------------------

moose_coll2 <- moose_coll %>%
  rename_with(~"Ecoregion", .cols = "study_sites")

# b
coll_merge <- left_join(moose_2020, moose_coll2)


# Question 26a ------------------------------------------------------------

plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose Collisions",
     main = "Relationship Between Moose Density and Number of Collisions with Moose in Newfoundland")

# b
# There is a subtle trend upwards towards the higher the moose density the more collisions there are.
# However this isn't taking into account the density/population of people in the given areas.
# This is why there is an outlier that isn't consistent with the rest of the graph.
# The point way at the top represents the Avalon that has a human population that is substantially higher than the other regions.


# Question 27 -------------------------------------------------------------

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop) %>%
  print()


# Question 28 -------------------------------------------------------------

plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Moose Collisions per Capita",
     main = "Relationship Between Human Population and Number of Collisions with Moose in Newfoundland"
     )


# Question 29 -------------------------------------------------------------

# Even though there may be more total moose collision in areas with a higher population of people the collisions per capita can be lower.
# Due to moose not being as common in and around urban areas with higher human populations.
# Based on this data there is a higher chance of a moose collision in less populated areas (rural).
# Since there is higher moose to human ratio, even though there are less total collisions the collision rate will still be higher.








