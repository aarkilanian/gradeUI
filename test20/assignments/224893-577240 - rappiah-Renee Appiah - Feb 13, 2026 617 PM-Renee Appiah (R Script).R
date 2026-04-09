# ==========================================
# BIOL 1002 Assignment: Moose Populations in Newfoundland
# Name: Renee Appiah
# Date: February 10, 2026
# ==========================================


# --- PART I: MOOSE POPULATION ANALYSIS ---

# 1. Load libraries
# Loading dplyr
library(dplyr)

# 2. Import moose data set
# Remember: Use forward slashes (/) in file paths to avoid errors in R
moosedata <- read.csv("C:/Users/adepa/OneDrive/Documents/BIOL1002_RAssignment/BIOL1002_RAssignment/MoosePopulation.csv")
head(moosedata) # Preview the first 6 rows to check formatting

# 3. Handle Missing Values
# na.omit removes any rows containing "NA" so they don't mess up our calculations
moose_clean <- na.omit(moosedata)

# 4. Select Columns of interest
# We only need Ecoregion, Year, Area, and Population for this analysis
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
head(moose_sel)

# 5a. Find the Oldest Observation (Minimum Year)
year_min <- min(moose_sel$Year)
year_min

# 5b. Highest Estimated Moose Pop recorded
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max

# 6. Standardize data (Calculate Density)
# Density = Pop / Area; this allows us to compare regions of different sizes fairly
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
head(moosedata2)

# 7. Data Visualization: Density over Time
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose Density in NL Ecoregions Over Time",
     pch = 16, col = "darkblue")

# 8a. Filter for Western Forests Ecoregion
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# 8b. Line Plot for Western Forests
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose Density: Western Forests",
     type = "l", lwd = 2, col = "darkgreen")

# 9. Filtering and Arranging 2020 Data
# a. Filter for 2020
moose_2020 <- filter(moosedata2, Year == 2020)

# b. Identify high density areas (> 2 moose/sq km)
moose_2020_high <- filter(moose_2020, MooseDensity > 2)

# c. Arrange by descending density
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
print(moose_2020_high_byD)

# 10. Using Pipes (Refactoring Step 9)
# Pipes (%>%) make code much easier to read by flowing data from one step to the next
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity))
print(moosefinal)


# --- PART II: TREE SAPLING STUDY ---

# 11. Import and Clean Sapling Data
saplings <- read.csv("C:/Users/adepa/OneDrive/Documents/BIOL1002_RAssignment/BIOL1002_RAssignment/SaplingStudy.csv")
sap_clean <- na.omit(saplings)

# 12a. Browsing Score by Ecoregion
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

# 12b. Sort by decreasing average browsing
avg_browse_reg<- sap_reg_browse%>%
  arrange(desc(AverageBrowsing))%>%
  print()
# Highest browsing score is in Northern Peninsula Forests, lowest in Strait of Belle Isle Barrens

# 13a. Mean Height per Ecoregion
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE))%>%
print()

# 13b. Identify severely browsed regions (Height < 20cm)
sap_reg_height_low <- sap_reg_height %>% 
  filter(AverageHeight < 20)%>%
  print()
# The following ecoregions have average heights less than 20 cm: Northern Peninsula Forests and Western Forests

# 14a. Browsing by Species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))%>%
  print()


# 14b. Rearrange by decreasing browsing
avg_browse_spe<- sap_spe_browse%>%
  arrange(desc(AverageBrowsing))%>%
  print()
# Species with highest browsing is Black Ash and species with lowest browsing is Black Spruce

# 15. Balsam Fir Analysis
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))%>%
  print()

# 16. Balsam Fir Visualization
barplot(fir_reg_browse$AverageBrowsing, 
        names.arg = fir_reg_browse$Ecoregion, 
        las = 2, # Rotates axis labels for readability
        ylab = "Avg Browsing Score", 
        main = "Balsam Fir Browsing by Region", 
        col = "forestgreen", cex.names = 0.6)

# 17a. Black Spruce Analysis & Comparison
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))%>%
  print()

# 17b. Black Spruce Visualization
barplot(spruce_reg_browse$AverageBrowsing, 
        names.arg = spruce_reg_browse$Ecoregion, 
        las = 2, 
        ylab = "Avg Browsing Score",srt=45,
        main = "Black Spruce Browsing by Region", 
        col = "steelblue", cex.names = 0.6)

# 17b.Comparison: Generally, Balsam Fir shows higher browsing scores across most regions compared to Black Spruce.For example, in the Avalon Forests, Balsam Fir has a score of 2.0 while Black Spruce is significantly lower at roughly 0.5. This supports the idea that Balsam Fir is a "preferred" food source, while Black Spruce is often avoided or only eaten when other options are scarce.

# 18. Count per region
# Tallying counts per region and species to check for sampling bias
sap_reg_tally <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  tally()%>%
  print()

# 19. Count per species
sap_spe_tally <- sap_clean %>% 
  group_by(Species) %>% 
  tally()%>%
  print()

# 20a. The data set is definitely not evenly distributed. Rare species like Black Ash or remote regions are underrepresented.For example, North Shore Forests has 8 trees sampled, while the Strait of Belle Isle Barrens has only 1. Species-wise, Black Ash only has 1 tree recorded, while Balsam Fir and Alder have 11 and 8 respectively.

# 20b. Recognizing this bias is important because the high browsing score of 5 for Black Ash is based on just one tree—this might not represent the whole species across the province. Over represented data from easy-to-access areas can skew averages and lead to the wrong management decisions.


# --- PART III: JOINING DATASETS ---

# 21a. Prepare 2020 Data and Join
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)%>%
  print()

# 21b. Joining by Ecoregion
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
head(moose_sap)

# 22. Averages by Species and Ecoregion
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
            AvgDensity = mean(MooseDensity, na.rm = TRUE), .groups = 'drop')%>%
  print()

# 23. Visualization 
library(ggplot2)
ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

# 23a. Yes, the figure supports the hypothesis because at low densities (0 to 1), the browsing scores are spread widely between 0 and 4.5, showing selective behavior . As moose density increases toward 2.5, the scores for all species converge and increase toward the top of the scale (3.0 to 5.0), indicating that moose shift to generalist browsing as food becomes more limited.
# 23b. Based on the plot, moose favour Willow and Alder the most, as these species consistently maintain high browsing scores across all density levels . They browse Black Spruce the least, as it has the lowest scores (including 0) at low densities and only increases when moose density is high.
# 23c. Black Ash is the species missing from most of the figure because it was only sampled in one ecoregion. Also because there is no data for Black Ash at any other density level, we cannot see how moose preference for this species changes as their population density fluctuates.


# --- PART IV: VEHICLE COLLISIONS ---

# 24. Create and Merge Collision Data
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
Ecoregion <- c("North_Shore_Forests", "Northern_Peninsula_Forests", 
               "Long_Range_Barrens", "Central_Forests", "Western_Forests", 
               "EasternHyperOceanicBarrens", "Maritime_Barrens", 
               "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, Ecoregion)

# 25a. Renaming column
moose_coll2 <- moose_coll %>% 
  rename_with(~ "study_sites", Ecoregion)

# 25b. Joining data sets
coll_merge <- left_join(moose_coll, moose_2020b, by = "Ecoregion")
View(coll_merge)

# 26a. Plot Density vs. Collisions
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density", ylab = "Number of Collisions",
     main = "Moose Density vs. Vehicle Collisions", pch = 19, col = "red")
# The scatter plot shows a general upward trend. Generally, more moose mean more collisions. 

# 26b.The Avalon Forests is a clear outlier. Even though its moose density isn't the highest (1.0), it has the highest number of collisions (110).

# 27. Per Capita Analysis
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# 28. Visualization
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population", ylab = "Collisions Per Capita",
     main = "Human Pop vs. Per Capita Collisions", pch = 19, col = "purple")

# 29. Regions with lower human populations have much higher per capita collision rates. In rural areas, you have more moose per person and fewer divided highways. This means a single driver in a rural area has a much higher statistical chance of hitting a moose than someone driving in the city, where there are hundreds of thousands of people but very few moose on the roads.

