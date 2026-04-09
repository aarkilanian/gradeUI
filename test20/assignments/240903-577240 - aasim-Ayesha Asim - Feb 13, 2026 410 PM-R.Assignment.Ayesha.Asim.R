# Title: BIOL 1002 R Assignment Moose Population & Sapling Study
# Name: Ayesha Asim
# Student ID: 202511828
# Date: February 2026

# Question 1: Install and load dplyr
install.packages("dplyr")
library(dplyr)

# Question 2: Import MoosePopulation.csv dataset
moosedata<-read.csv("MoosePopulation.csv")

# Question 3: Remove missing values
View(moosedata)
moose_clean<-na.omit(moosedata)

# Question 4: Select columns of interest
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5: Oldest year and highest population
year_min<-min(moose_sel$Year)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
# a: Oldest observation year = 1904
# b: Highest Estimated_Moose_Pop = 41250

# Question 6: Calculate Moose Density
moosedata2<-mutate(moose_sel,MooseDensity=Estimated_Moose_Pop/Area)

# Question 7: Plot MooseDensity over Year
plot(moosedata2$Year, moosedata2$MooseDensity, xlab="year",
       ylab="Moose per sq km", main="Moose density in Newfoundland ecoregions over time")

# Question 8a: Filter Western_Forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8b: Line graph for Western_Forests
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose Density",
     main = "Moose Density in Western Forests Over Time")

# Question 9A, 9B, 9C: Filter for 2020 and high density 
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10: Use pipes
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# PART II: Sapling Study

# Question 11) A, B: Load SaplingStudy.csv and clean
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

# Question 12a: Mean browsing score by ecoregion
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# Moose browsing pressure varies among ecoregions because the mean BrowsingScore 
# differs between regions.

# Question 12b: Order by highest browsing
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
# Highest average browsing: Northern_Peninsula_Forests
# Lowest average browsing: StraitOfBelleIsleBarrens

# Question 13a: Mean tree height by ecoregion
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AvgHeight = mean(Height)) %>%
  print()
# Average tree height varies across ecoregions because the mean Height differs 
# between regions.

# Question 13b: Ecoregions with average height less than 20cm
sap_reg_height_low <- sap_reg_height %>%
  filter(AvgHeight < 20) %>%
  print()
# Ecoregions with average height less than 20cm: Northern_Peninsula_Forests and 
# Western_Forests

# Question 14a: Mean browsing by Species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
# The average browsing score varies among species. Black_Ash is highest, 
# Black_Spruce is lowest, and Alder and Willow are moderately high.

# Question 14b:
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
# The highest browsing species is Black_Ash (5)
# The lowest browsing species is Black_Spruce (2.33)

# Question 15: Balsam Fir browsing by Ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))

# Question 16: Barplot for Balsam Fir
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "green",
        cex.names = 0.6)

# Question 17A: Black Spruce browsing
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
# Question 17B & 17C: Average browsing score graph
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "green",
        cex.names = 0.6)
# Balsam Fir generally shows higher browsing intensity than Black Spruce across most ecoregions. 
# Especially in Avalon Forests and Northern Peninsula Forests.
# However, in the Maritime Barrens, Black Spruce appears to have slightly higher browsing.

# Question 18: Number of saplings per Ecoregion
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# No, the same number of tree saplings were not counted in each ecoregion.
# The tally shows unequal sample sizes, with some regions having more observations than others.

# Question 19: Number of saplings per Species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# No, the same number of tree saplings were not counted for each species.
# The tally shows different sample sizes, with Balsam Fir having the most and Black Ash the fewest.

# Question 20: Sampling bias comments
# a) The SaplingStudy dataset is not evenly distributed. 
# Some ecoregions such as North Shore Forests have many more samples, 
# while others like StraitOfBelleIsleBarrens and Maritime Barrens are underrepresented.
# Species counts are also uneven, with Balsam_Fir overrepresented and Black_Ash having very few observations.
# b) Recognizing bias in ecological datasets is important because uneven sampling can distort 
# interpretations of browsing patterns. Higher sample sizes may make some species or regions appear more 
# heavily browsed even if the difference is due to sampling effort rather than ecology.

# PART III: Joining Data

# Question 21A: Join datasets
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Question 21B: Join datasets
moose_sap <- left_join(moose_2020b, sap_clean,
                       by = 'Ecoregion',
                       relationship = "many-to-many")

# Question 22: Average browsing and density
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity)) %>%
  print()

# Question 23: Interpretation of ggplot figure
# a) Yes, the figure suggests that at low moose density browsing is more selective,
# while at higher densities browsing scores increase across multiple species,
# indicating a shift toward more generalist feeding.
# b) Moose appear to favour Willow and Alder the most based on consistently high
# browsing scores, while Black_Spruce and Balsam_Fir tend to have lower browsing overall.
# c) Black_Ash is not shown on the figure because there were too few observations,
# so an average browsing and density relationship could not be meaningfully displayed.

# Moose-Vehicle Collisions

# Question 24: Create collision dataset
collisions2020 <- c(56,60,14,36,48,10,40,110,6)
human_pop <- c(18000,12000,4000,75100,24000,3500,32000,270000,2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25: Rename and join datasets 
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Question 26: Scatterplot MooseDensity vs collisions
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Collisions in 2020",
     main = "Moose Density vs Vehicle Collisions")
# As moose density increases, the number of moose-vehicle collisions generally increases. 
# The relationship is not perfectly linear. There is a clear outlier at a density 
# of 1.0 that spikes with over 100 collisions, and another point at the same density 
# with around 40 collisions that slightly deviates from the trend.

# Question 27: Collisions per capita
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
# The ecoregion with the highest number of moose collisions per person is Northern_Peninsula_Forests. 
# This region has the largest value in the coll_per_capita column (0.005).
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita)) %>%
  select(Ecoregion, coll_per_capita)

# Question 28: Scatterplot per capita collisions
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs Human Population")

# Question 29: Interpretation
# Collisions per capita are highest in areas with low to moderate human populations 
# and generally decrease as the human population increases. This trend makes sense because moose are more abundant in sparsely populated regions of Newfoundland, 
# so each person is more likely to experience a collision.