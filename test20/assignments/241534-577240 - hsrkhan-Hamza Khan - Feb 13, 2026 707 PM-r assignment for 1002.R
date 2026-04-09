# --- PART I: Moose Populations in Newfoundland ---

install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("C:/Users/Hamza Khan/Downloads/MoosePopulation.csv")
View(moosedata)
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "Year",
     ylab = "Moose Density",
     main = "Moose Density in Western Forests Over Time")

moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# --- PART II: Tree Sapling Study ---

saplings <- read.csv("C:/Users/Hamza Khan/Downloads/SaplingStudy.csv")

sap_clean <- na.omit(saplings)

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AvgHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_reg_height %>%
  filter(AvgHeight < 20) %>%
  print()

sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

avg_browse_spe <- arrange(sap_spe_browse, desc(MeanBrowsing))

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore))

barplot(fir_reg_browse$AvgBrowsing, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion", ylab = "Avg Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion", col = "forestgreen", cex.names = 0.5)

spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore))

barplot(spruce_reg_browse$AvgBrowsing, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion", ylab = "Avg Browsing Score",
        main = "Black Spruce Browsing by Ecoregion", col = "blue", cex.names = 0.5)

# Comparison: Balsam Fir generally shows much higher browsing scores across all regions 
# compared to Black Spruce, as moose prefer fir over the less palatable spruce.

sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()


# Question 20: Sampling Bias Evaluation
# a. The dataset is not evenly distributed; some regions like North Shore Forests have 8 samples 
# while others like the South Coast Barrens have only 1. 
# b. It is important to recognize bias because low sample sizes can lead to inaccurate averages 
# that don't represent the true ecological state of the entire ecoregion.

# --- PART III: Creating and Joining Datasets ---

moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()

# Question 23: Hypothesis Observation
# Palatable species like Willow and Balsam Fir show high browsing even at low moose densities, 
# whereas less preferred species like Black Spruce only see increased browsing at higher densities.

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", 
                 "Central_Newfoundland_Forests", "Maritime_Barrens", "Avalon_Forest", 
                 "Western_Forests", "Avalon_Peninsula", "South_Coast_Barrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", 
                 "Central_Newfoundland_Forests", "Maritime_Barrens", "Avalon_Forest", 
                 "Western_Forests", "Avalon_Peninsula", "South_Coast_Barrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")

plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density", ylab = "Collisions (2020)",
     main = "Moose Density vs Vehicle Collisions")

# Trend: There is a general positive trend where higher moose density correlates with 
# more vehicle collisions, though the Avalon Peninsula often acts as an outlier due to human density.

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population", ylab = "Collisions Per Capita",
     main = "Human Population vs Collisions Per Capita")

# Question 29: Final Trend description
# The risk per person (per capita) is often higher in regions with smaller human populations 
# but high moose densities, as a single person is statistically more likely to encounter 
# a moose on the road in those areas.