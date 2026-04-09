#Title: BiologyRscript
#Author: Kayla Pelley
#Date: 12-01-2026
install.packages("vegan")
library(vegan)
getwd()
library(readr)
MoosePopulation <- read_csv("MoosePopulation.csv")
View(MoosePopulation)
library(readr)
sap_clean <- read_csv("SaplingStudy.csv")
View(sap_clean)
#Q1-Loading dplyr
install.packages("dplyr")
library(dplyr)
#Q2- Load moose dataset
moosedata <- read.csv("MoosePopulation.csv")
head(moosedata)
#Q3- Remove NAs
moose_clean <- na.omit(moosedata)
print(moose_clean)
#Q4-Select specific columns
moose_sel <- moose_clean %>%
  select(Ecoregion, Year, Area, Estimated_Moose_Pop)
print(moose_sel)
#Q5-Earliest year and max population
min_year <- min(moose_sel$Year, na.rm = TRUE)
library(dplyr)
# Find the row(s) with the oldest observation
oldest_obs <- moose_sel %>%
  filter(Year == min(Year, na.rm = TRUE))
# Print the result
print(oldest_obs)
#a oldest ovservation 1904
max_pop <- max(moose_sel$Estimated_Moose_Pop)
print(min_year)
print(max_pop)
# highest recorded 41250
#Oldest observation 1904
#Q6-Create MooseDensity
moosedata2 <- moose_sel %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
#Q7-Plot MooseDensity over time
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose Density",
     main = "Moose Density Over Time",
     pch = 19, col = "blue")
#Q8-Filter ecoregion and plot
moose_region <- moosedata2 %>%
  filter(Ecoregion == "Central_Forests") %>%
  print()
plot(moose_region$Year, moose_region$MooseDensity,
     xlab = "Year",
     ylab = "Moose Density",
     main = "Central Forests Moose Density",
     pch = 19, col = "darkgreen")
#Q9-Filter 2020, high density
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Q10
# Already done in Q9 using pipes, same output
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Q11- Load and clean saplings
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
print(sap_clean)
#Q12- Moose browsing by ecoregion
library(dplyr)
# Step 1: Group by ecoregion and calculate average browsing
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
# Step 2: Rearrange in descending order of AverageBrowsing
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Highest browsing: Northern_Peninsula_Forests, Lowest browsing: StraitOfBelleIsleBarrens
#Q13-Average tree height by ecoregion
colnames(sap_clean)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AvgHeight = mean(Height, na.rm = TRUE)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AvgHeight < 20) %>%
  print()
# The average tree height varies across ecoregions.
# Western_Forests and Northern_Peninsula_Forests have the lowest average heights (<20),
# while Avalon_Forests, North_Shore_Forests, and EasternHyperOceanicBarrens have the tallest average trees.
#Q14-Average browsing by species
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AvgBrowsing)) %>%
  print()
# a The average browsing score varies among tree sapling species. Black_Ash experiences the highest average browsing score 5.00, followed by Willow 4.31 and Alder 4.25. Black_Spruce has the lowest average browsing score 2.33. This shows that moose browsing pressure is not uniform across species, with some species being browsed more heavily than others
#b Black_Ash has the highest browsing score
#b Black_Spruce has the lowest browsing score
#Q15-Balsam fir by ecoregion
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
#Q16-Barplot balsam fir
barplot(fir_reg_browse$AvgBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing by Ecoregion",
        col = "forestgreen",
        cex.names = 0.6)
#Q17-Black spruce by ecoregion
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()
barplot(spruce_reg_browse$AvgBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing by Ecoregion",
        col = "darkred",
        cex.names = 0.6)
#a Black Spruce browsing varies across ecoregions, with Highest Black Spruce browsing: Northern_Peninsula_Forests and Lowest Black Spruce browsing: EasternHyperOceanicBarrens, Maritime_Barrens
#Q18-Sapling per ecoregion
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#a No, the number of tree saplings counted varies across ecoregions. Some regions, like North_Shore_Forests, have 8 saplings, while others, like StraitOfBelleIsleBarrens, have only 1.
#Q19-saplings per species
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#a No, the number of saplings counted differs among species. Balsam_Fir had the most saplings 11, while Black_Ash had the fewest 1
#Q20-sampling bias
#a The SaplingStudy dataset is not evenly distributed. Some ecoregions and species are overrepresented (North_Shore_Forests, Balsam_Fir) while others are underrepresented() StraitOfBelleIsleBarrens, Black_Ash).
#b Recognizing bias is important because uneven sampling can misrepresent patterns in the ecosystem. Over- or underrepresented groups may lead to incorrect conclusions about moose browsing behavior.
#Q21-Merge moose and saplings
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
moose_sap <- left_join(
  moose_2020b,
  sap_clean,
  by = "Ecoregion",
  relationship = "many-to-many"
)
head(moose_sap)
#Q22-Avg browsing and moosedensity by species/ecoregion
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    Avg_Browsing = mean(BrowsingScore, na.rm = TRUE),
    Avg_MooseDensity = mean(MooseDensity, na.rm = TRUE)
  ) %>%
  print()
#Q23-Moosedensity by species
#a # Yes, the figure suggests moose show strong preferences at low density, focusing on certain species, and broaden their browsing to more species as moose density increases, supporting the hypothesis of density-dependent browsing.
#b # Moose favour Black_Ash the most, followed by Willow and Alder, while Black_Spruce is browsed the least.
#c # The unnamed species (blank row) is not shown on the figure because no data were recorded for it or the browsing score was missing (NaN).
#Q24-Moose-vehicle collisions
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c(
  "North_Shore_Forests",
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
#Q25-Avoid
#Q26-Moosedensity vs collisions
# Step 1: Create the dataset
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens",
                 "Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
                 "Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
MooseDensity <- c(2.588, 2.667, 0.602, 2.012, 2.5, 0.5, 0.96, 0.962, 0.01)
coll_merge <- data.frame(study_sites, collisions2020, human_pop, MooseDensity)
# Step 2: Create the scatterplot
plot(
  coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of Moose-Vehicle Collisions",
  main = "Moose Density vs Moose-Vehicle Collisions",
  pch = 19,
  col = "darkblue"
)
# Moose-vehicle collisions generally increase with higher moose density.
# Avalon_Forests has a high number of collisions relative to its moose density, which could be an outlier.
#Q27=Collisions per person
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop) %>%
  print()
coll_merge_per_capita %>%
  arrange(desc(coll_per_capita)) %>%
  select(study_sites, coll_per_capita) %>%
  print()
#Q28-Collisions per capita vs human population
plot(
  coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  xlab = "Human Population",
  ylab = "Collisions per Person",
  main = "Collisions per Capita vs Human Population",
  pch = 19, col = "darkgreen"
)
#Q29-Describing trends
#Moose collisions per person tend to be higher in areas with low human populations and moderate moose densities.
# This makes sense because in sparsely populated regions, moose have more space to move freely, increasing the chance of vehicle collisions relative to the number of people.


