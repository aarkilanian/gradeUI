# Title: My R script for Bio 1002
# Author: Zoe Hurley
# Date: 22-02-2026

# ------------------------------------------------------------------------------
# Part I: Moose Populations in Newfoundland

# Set working directory --------------------------------------------------------
setwd("~/R stuff for school/Bio1002/Scripts")

# Question 1: Load libraries needed --------------------------------------------
library(dplyr)

# Question 2: Load data --------------------------------------------------------
moosedata <- read.csv("MoosePopulation.csv")

# Question 3: Analyze data -----------------------------------------------------
View(moosedata)
moose_clean <- na.omit(moosedata) # Removing missing data

# Question 4: Simplify data ----------------------------------------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5: Find specific values ---------------------------------------------
year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Question 6: Tidy up data -----------------------------------------------------
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7: Plot data --------------------------------------------------------
plot(moosedata2$Year, moosedata2$MooseDensity,
    xlab = "year", 
    ylab = "Moose per sq km", 
    main = "Moose density in Newfoundland ecoregions over time"
    )

# Question 8: Plot specific data -----------------------------------------------
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
    type = "l", 
    col = "blue", main = "Moose Density Over Time: Western Forests", 
    xlab = "Year", 
    ylab = "Moose per sq km"
    )

# Question 9: Filter data ------------------------------------------------------
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity)) # Sorting

# Question 10: Use a pipe instead of multiple steps ----------------------------
moosefinal <- moosedata2 %>% # Using cleaned dataset
  filter(Year == 2020) %>% # Filtering for year 2020
  filter(MooseDensity > 2.0) %>% # Filtering for Moose Density above 2.0
  arrange(desc(MooseDensity)) %>% # Arranging in descending order
  print() # Printing the output

# ------------------------------------------------------------------------------
# Part II: Tree Sapling Study

# Question 11: Import data -----------------------------------------------------
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings) # Removing missing data

# Question 12: Using pipes to create a new database ----------------------------
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>% # Grouping data by ecoregion
  summarize(AverageBrowsing = mean(BrowsingScore)) %>% # Calculating mean
  arrange(desc(AverageBrowsing)) %>% # Rearranging dataset in decreasing order
  print() # Printing the result
# The Northern Peninsula Forests had the highest average browsing scores, while 
# the Strait of Belle Isle Barrens had the lowest

# Question 13: Calculating mean height per Ecoregion ---------------------------
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>% # Grouping data by ecoregion
  summarize(AverageHeight = mean(Height)) %>% # Calculating mean tree height
  print() # Printing the result
sap_reg_height_low <- sap_reg_height %>% 
  filter(AverageHeight < 20) %>%
  print() # Printing result
# Both Northern Peninsula and Western Forests have average heights less than
# 20cm

# Question 14: Sorting browsing score by species -------------------------------
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>% # Grouping data by species
  summarize(AverageBrowsing = mean(BrowsingScore)) %>% # Calculating mean
  print() # Printing results
avg_browse_spe <- sap_spe_browse %>% 
  arrange(desc(AverageBrowsing)) # Rearrange
# Black Ash has the highest browsing score, while Black Spruce has the lowest

# Question 15: Looking specifically at Balsam Fir data -------------------------
fir_reg_browse <- filter(sap_clean, Species == "Balsam_Fir") %>% # Filtering
  group_by(Ecoregion) %>% # Sorting by ecoregion
  summarize(AverageBrowsing = mean(BrowsingScore)) # Determining mean browsing

# Question 16: Making a bar graph ----------------------------------------------
par(mar = c(12, 4.1, 4.1, 2.1)) # Adjusting margins so names fit
barplot(fir_reg_browse$AverageBrowsing, # Using dataset from previous question
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "", # Leaving blank to avoid overlapping with names
        ylab = "Average Browsing Intensity", 
        main = "Balsam Fir Browsing Intensity by Ecoregion", 
        col = "forestgreen", 
        cex.names = 0.6,
        las = 2
        )
title(xlab = "Ecoregion", line = 9) # Adding the "Ecoregion" label

# Question 17: Looking specifically at Black Spruce data -----------------------
spruce_reg_browse <- filter(sap_clean, Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>% # Sorting by ecoregion
  summarize(AverageBrowsing = mean(BrowsingScore)) # Determining mean browsing
barplot(spruce_reg_browse$AverageBrowsing, # Using the dataset I just created
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "", # Leaving blank to avoid overlapping with names
        ylab = "Average Browsing Intensity", 
        main = "Black Spruce Browsing Intensity by Ecoregion", 
        col = "forestgreen", 
        cex.names = 0.6,
        las = 2
        )
title(xlab = "Ecoregion", line = 9) # Adding the "Ecoregion" label
# Balsam Fir seems to be the favorite snack for moose, while Black Spruce seems
# to be avoided. Balsam Fir shows higher browsing intensity across most
# ecoregions compared to Black Spruce

# Question 18: Looking at number of tree saplings counted ----------------------
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% # Determining how many trees were counted in each ecoregion
  print()

# Question 19: Looking at number of tree saplings counted for each species -----
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>% # Determining how many trees were counted for each species
  print()

# Question 20: Does the dataset accurately reflect the system? -----------------

# I don't think the SaplingStudy dataset is evenly distributed. North Shore and
# Northern Peninsula Forests are overrepresented, while the Strait of Belle Isle
# and Maritime Barrens are severely underrepresented. The same can be said for
# certain tree species. Balsam Firs are overrepresented while Black Ash is
# extremely underrepresented

# It's important to recognize bias in ecological datasets because it ensures
# that our conclusions reflect the true ecological state of what we're studying 
# rather than errors in how the data was collected. Without identifying bias, we
# risk making incorrect management decisions or overestimating the impact of
# certain factors

# ------------------------------------------------------------------------------
# Part III: Creating and Joining Datasets

# Question 21: Looking at year 2020 --------------------------------------------
moose_2020b <- filter(moose_clean, Year == 2020) # Using original moose_clean
moose_2020b <- mutate(moose_2020b, MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, # Joining moose_2020b with sap_clean
                       sap_clean, 
                       by = 'Ecoregion', # Matching rows by Ecoregion column
                       relationship = "many-to-many")

# Question 22: Calculating average browsing score and average moose density ----
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>% # Grouping by species and ecoregion
  summarize(AverageBrowsing = mean(BrowsingScore), # Finding mean browsing
            AverageDensity = mean(MooseDensity)) %>% # Finding mean density
  print() # Printing the result

# Question 23: Looking at research team figure ---------------------------------

# There's evidence that supports the researchers' hypothesis. At low densities,
# the wide gap between species scores suggests selective preference, while at
# higher densities (above 2.0), the scores converge as moose shift to general
# browsing due to increased competition

# Moose favor Willow and Alder saplings the most. They browse Black Spruce the
# least

# Black Ash isn't shown on the figure because its browsing scores are likely
# zero, which is identical to Black Spruce. This would cause the points to
# overlap and be obscured by the Black Spruce data markers

# ------------------------------------------------------------------------------
# Part III (6.0.1): Moose-vehicle collisions

# Question 24: Importing and editing the vector --------------------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests",
                 "Northern_Peninsula_Forests",
                 "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens",
                 "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25: Joining dataset with moose_2020 ---------------------------------
moose_coll2 <- moose_coll %>%
  rename("Ecoregion" = "study_sites")
coll_merge <- left_join(moose_2020, # Joining the datasets into a new dataset
                       moose_coll2, 
                       by = 'Ecoregion',
                       relationship = "many-to-many")

# Question 26: Analyzing data --------------------------------------------------
par(mar = c(5.1, 4.1, 4.1, 2.1)) # Adjusting margins again
plot(coll_merge$MooseDensity, coll_merge$collisions2020, # Creating scatterplot
     xlab = "Moose per square km", 
     ylab = "Number of collisions", 
     main = "Moose Density vs. Moose-vehicle Collisions (2020)",
     pch = 19
      )
# In general, the number of moose-vehicle collisions increases as the moose
# density increases. There's one outlier: a point sitting way up at the top near
# 1.0 moose per sq km with over 100 collisions

# Question 27: Analyzing moose collisions per person ---------------------------
coll_merge_per_capita <- mutate(coll_merge, # Using mutate() function as in Q 6
                                coll_per_capita = collisions2020 / human_pop)

# Question 28: Creating a scatterplot of the above data ------------------------
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Human Population vs Collision Rate",
     pch = 19)

# Question 29: Observations ----------------------------------------------------

# In the previous graph, more moose meant more collisions (a positive trend). In
# this graph, as the human population gets larger (moving right on the x-axis),
# the collisions per capita actually decreases (a negative correlation). This
# makes sense because in high-population areas, there are fewer moose wandering
# around, and the roads are often more developed or lit