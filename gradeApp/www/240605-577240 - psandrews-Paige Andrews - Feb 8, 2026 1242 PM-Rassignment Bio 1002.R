moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
library(dplyr)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# Oldest observation
year_min <- min(moose_sel$Year, na.rm = TRUE)

# Highest moose population
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
# a) Filter for Western Forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# b) Line plot of MooseDensity over Year
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "Year", ylab = "Moose per sq km",
     main = "Moose density in Western Forests Ecoregion over time")
# Filter for year 2020
moose_2020 <- filter(moosedata2, Year == 2020)

# Filter for MooseDensity > 2.0
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Arrange MooseDensity in descending order
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
View(moosefinal)
# Load the dataset
saplings <- read.csv("SaplingStudy.csv")

# Remove rows with missing values
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(MooseBrowsingScore, na.rm = TRUE)) %>%
  print()

avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
colnames(sap_clean)

# Group by Ecoregion and calculate mean browsing score
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

# Arrange in decreasing order of AverageBrowsing
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

#

# Group by Ecoregion and calculate mean tree height
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>%
  print()

# Filter for ecoregions with AverageHeight < 20 cm
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

# # Ecoregions with average height < 20 cm (severely browsed by moose)
# Replace with actual region names from sap_reg_height_low

# a) Group by Species and calculate mean browsing score
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

# Arrange in decreasing order of AverageBrowsing
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

# # Species with highest and lowest browsing
# Highest: [replace with top row Species from avg_browse_spe]
# Lowest: [replace with bottom row Species from avg_browse_spe]
# Highest browsing: Black_Ash (5)
# Lowest browsing: Black_Spruce (2.33)

# Filter for Balsam_Fir, group by Ecoregion, and calculate mean browsing
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing =
            
            
            fir_reg_browse <- sap_clean %>%
              filter(Species == "Balsam_Fir") %>%
              group_by(Ecoregion) %>%
              summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
              print()
            barplot(fir_reg_browse$AverageBrowsing, 
                    names.arg = fir_reg_browse$Ecoregion, 
                    xlab = "Ecoregion", 
                    ylab = "Average Balsam Fir Browsing Score", 
                    main = "Average Balsam Fir Browsing by Ecoregion", 
                    col = "forestgreen", 
                    cex.names = 0.6)
            
            # Filter for Black_Spruce, group by Ecoregion, calculate mean browsing
            spruce_reg_browse <- sap_clean %>%
              filter(Species == "Black_Spruce") %>%
              group_by(Ecoregion) %>%
              summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
              print()
            
            # Bar plot for Black Spruce browsing by ecoregion
            barplot(spruce_reg_browse$AverageBrowsing,
                    names.arg = spruce_reg_browse$Ecoregion,
                    xlab = "Ecoregion",
                    ylab = "Average Black Spruce Browsing Score",
                    main = "Average Black Spruce Browsing by Ecoregion",
                    col = "darkblue",
                    cex.names = 0.6)
            
            # # Comparison comment
            # Black Spruce generally experiences lower browsing intensity than Balsam Fir across all ecoregions, 
            # as indicated by the shorter bar heights in the bar plot.
            sap_reg_tally <- sap_clean %>%
              group_by(Ecoregion) %>%
              tally() %>%
              print()
            sap_spe_tally <- sap_clean %>%
              group_by(Species) %>%
              tally() %>%
              print()
            # a) Distribution evaluation
            # The SaplingStudy dataset is not evenly distributed. 
            # Some ecoregions (e.g., Avalon_Forests) and some species (e.g., Black_Spruce) are overrepresented, 
            # while others have fewer observations.
            
            # b) Importance of recognizing bias
            # Recognizing bias is important because uneven sampling can distort patterns of moose browsing, 
            # leading to incorrect conclusions about which ecoregions or species are most affected.
            
            
            # Merge Moose density with Sapling data by Ecoregion
            moose_saplings <- left_join(sap_clean, moosedata2, by = "Ecoregion")
            
            # Check the merged dataset
            View(moose_saplings)
            
            
            # Filter moose_clean for 2020 and calculate MooseDensity
            moose_2020b <- moose_clean %>%
              filter(Year == 2020) %>%
              mutate(MooseDensity = Estimated_Moose_Pop / Area)
            
            # Merge with sap_clean by Ecoregion
            moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
            
            # View the merged dataset
            View(moose_sap)
            
            
            # Group by Species and Ecoregion, calculate mean BrowsingScore and mean MooseDensity
            sum_spe_browse <- moose_sap %>%
              group_by(Species, Ecoregion) %>%
              summarize(
                AverageBrowsing = mean(BrowsingScore, na.rm = TRUE),
                AverageMooseDensity = mean(MooseDensity, na.rm = TRUE)
              ) %>%
              print()
          
            
            # Based on the figure:
            
            # 1) Evidence for hypothesis
            # The plot suggests that at low moose densities, browsing is more selective (some species have high scores while others are low), 
            # and at higher moose densities, browsing becomes more uniform across species, supporting the researchers' hypothesis.
            
            # 2) Most and least favoured species
            # Moose favour Black_Ash and Willow the most, as they have the highest average browsing scores. 
            # Black_Spruce is browsed the least.
            
            # 3) Species not shown
            # Some species may not appear on the figure because they had no observations in the 2020 data used to calculate average browsing, 
            # or their average browsing scores were missing/NA.
            
            # Vectors
            collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
            human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
            study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens",
                             "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                             "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
            
            # Create data frame
            moose_coll <- data.frame(collisions2020, human_pop, study_sites)
            
            # View dataset
            View(moose_coll)
            
            
            # Rename study_sites to Ecoregion so it matches moose_2020b
            moose_coll2 <- moose_coll %>%
              rename(Ecoregion = study_sites)
            
            # Join with moose_2020b (from 2020) by Ecoregion
            coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")
            
            # View the merged dataset
            
            # Scatterplot of MooseDensity vs collisions
            plot(coll_merge$MooseDensity, coll_merge$collisions2020,
                 xlab = "Moose Density (moose per km²)",
                 ylab = "Number of Moose-Vehicle Collisions",
                 main = "Moose Density vs Vehicle Collisions",
            
            # Comment on trends
            # # As moose density increases, the number of moose-vehicle collisions generally increases, 
            # # suggesting a positive relationship. Some regions (e.g., Avalon_Forests_
            
            # Calculate collisions per person
            coll_merge_per_capita <- coll_merge %>%
            
            # View the updated dataset
            coll_merge_per_capita %>%
              arrange(desc(coll_per_capita)) %>%
              print()
            # The ecoregions with the highest moose collisions per person are Avalon_Forests and Central_Forests, 
            # indicating that people in these regions are at greater risk of vehicle collisions with moose.
            # Scatterplot of collisions per person vs human population
            plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
                 xlab = "Human Population",
                 ylab = "Collisions per Person",
                 main = "Collisions per Person vs Human Population",
                 pch = 19, col = "blue")
            
            # Comment on trends
            # # Generally, regions with smaller human populations tend to have higher collisions per person, 
            # # while regions with larger populations have lower collisi
            # Regions with smaller human populations tend to have higher collisions per person, while more populated regions have lower collisions per capita. 
            # This makes sense because moose are more abundant relative to people in less populated areas, increasing the chance of vehicle collisions.
            
            