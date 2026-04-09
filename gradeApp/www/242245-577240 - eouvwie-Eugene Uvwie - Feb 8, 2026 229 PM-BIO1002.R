#Title:Biology 1002 R-assignment.
#Author:Eugene Uvwie.
#Date Due:13-02-2026.
#Set working directory.
setwd("/Users/Patrick/Documents/BIOL1002_RAssignment")
#PART I

#1.Load library needed.
library(dplyr)

#2.Import Moosepopulation.csv dataset.
moosedata <- read.csv("MoosePopulation.csv")

#3a.View the dataset.
View(moosedata)
#3b.Remove rows with missing lines.
moose_clean <- na.omit(moosedata)

#4.Simplify the dataset.
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#5a.The oldest observation in the dataset.
year_min <- min(moose_sel$Year)
#year_min = 1904
#5b.The highest ‘Estimated_Moose_Pop’ recorded.
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#moose_max = 41250

#6.Calculating moose density for each ecoregion.
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#7.Plot a line graph.
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#8a.Create a new dataset.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#8b.Plot a line graph showing how moose density has changed over time in Western_Forests region. 
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in westen_forests regions over time")

#9a.function to filter for the year 2020.
moose_2020 <- filter(moosedata2, Year == "2020")
#9b.show ecoregions where moose density is greater than 2.0.
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
#9c.function to sort the MooseDensity column in descending order. 
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#10.To connect one line of code to the next.
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#PART II

#11a.Load the SaplingStudy.csv data file.
saplings <- read.csv("SaplingStudy.csv")
#11b.remove the NAs from the dataset. 
sap_clean <- na.omit(saplings)

#12a.Create a new database.
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(
    mean_browsing = mean(BrowsingScore)
  )

print(sap_reg_browse)
#Moose browsing pressure varies across ecoregions, with some ecoregions showing higher mean browsing scores than others, indicating spatial differences in moose impact.
#12b.To rearrange dataset(Descending order)
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(mean_browsing))

print(avg_browse_reg)

# The first ecoregion has the highest average moose browsing
# The last ecoregion has the lowest average moose browsing

#13a.Function to group the data.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(
    mean_height = mean(Height)
  )

print(sap_reg_height)
#13b.To describe which ecoregions have average heights less than 20 cm.
sap_reg_height_low <- sap_reg_height %>%
  filter(mean_height < 20)

print(sap_reg_height_low)

# These ecoregions have average tree heights less than 20 cm and are considered severely browsed by moose

#14a.Function to group the data by Species.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(
    mean_browsing = mean(BrowsingScore)
  )

print(sap_spe_browse)
#Average moose browsing scores vary among sapling species, indicating that moose preferentially browse some species more than others.
#14b.To rearrange the data according to decreasing mean browsing score.
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(mean_browsing))

print(avg_browse_spe)

# The first species listed has the highest average browsing score
# The last species listed has the lowest average browsing score
#The species with the highest average browsing score is [Black_Ash], while the species with the lowest average browsing score is [Black_Spruce].

#15.To filter the Species column for only Balsam_Fir.
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(
    mean_browsing = mean(BrowsingScore)
  )

print(fir_reg_browse)

#16.To make a bar graph.
barplot(fir_reg_browse$mean_browsing, names.arg = fir_reg_browse$Ecoregion,las=2, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Moose Browsing Intensity on Balsam Fir by Ecoregion", col = "forestgreen")

#17a.To create the summary table for Black Spruce.
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(
    mean_browsing = mean(BrowsingScore)
  )

print(spruce_reg_browse)
#17b.To create a Bar graph of Black Spruce browsing by ecoregion.
barplot(
  spruce_reg_browse$mean_browsing,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Intensity",
  main = "Average Moose Browsing Intensity on Black Spruce by Ecoregion",
  las = 2
)
#17c. Black Spruce generally shows lower average browsing intensity than Balsam Fir across most ecoregions,
# suggesting that moose preferentially browse Balsam Fir over Black Spruce.

#18.Function to determine how many trees were counted in each Ecoregion.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# The number of tree saplings counted differs across some ecoregions, indicating unequal sampling effort.

#19.Function to determine how many individual trees were counted in for each Species.
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()

print(sap_spe_tally)
# The number of saplings counted differs among species, indicating unequal sampling effort.

#20a.The SaplingStudy dataset is not evenly distributed, as some ecoregions and tree species have more sampled saplings than others.
# This indicates that certain regions or species are overrepresented while others are underrepresented in the dataset.
#20b.Recognizing sampling bias is important because uneven representation can skew results and lead to incorrect conclusions about moose browsing patterns.
# Biased datasets may overemphasize trends from heavily sampled regions or species while underrepresenting others.

#PART III

#21a. Function to select only the rows for the year 2020 and to create a new column
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(
    MooseDensity = Estimated_Moose_Pop / Area
  )

print(moose_2020b)

#21b.To join moose_2020b with the sap_clean dataset, matching rows by the common Ecoregion column. 
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#22.To calculate the average browsing score and average moose density for each species within each ecoregion.
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    mean_browsing = mean(BrowsingScore),
    mean_moose_density = mean(MooseDensity)
  )

print(sum_spe_browse)

#23a.The figure provides evidence supporting the researchers’ hypothesis, as moose show stronger preferences for certain sapling species at lower moose densities.
# At higher moose densities, browsing scores across species become more similar, suggesting a shift toward more generalist browsing behavior.

#23b.Moose appear to favour Balsam Fir the most, as it consistently shows higher browsing scores across densities.
# Black Spruce shows the lowest browsing intensity, indicating it is least preferred by moose.

#23c.White Spruce is not shown in the figure because it had little to no browsing or insufficient data for calculating average browsing scores across density levels.
# As a result, it does not appear in the summarized dataset used for the plot.

#24.Function to create a dataset using vectors.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#25a.To correct and join datasets.
moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", study_sites)
#25b.Join the datasets.
coll_merge <- moose_2020 %>%
  left_join(moose_coll2, by = "Ecoregion")

print(coll_merge)

#26a.Moose density vs collisions (scatterplot).
plot(
  coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of Moose-Vehicle Collisions (2020)",
  main = "Moose Density vs Moose-Vehicle Collisions"
)
#26b.There is a positive relationship between moose density and the number of moose-vehicle collisions, with higher densities generally associated with more collisions.
# One or two points appear to be outliers, likely representing regions with unusually high collision rates relative to moose density.

#27.Collisions per capita.
coll_merge_per_capita <- coll_merge %>%
  mutate(
    coll_per_capita = collisions2020 / human_pop
  )

print(coll_merge_per_capita)

#28.Scatterplot of collisions per capita vs human population code.
plot(
  coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  xlab = "Human Population",
  ylab = "Moose Collisions per Capita",
  main = "Moose Collisions per Capita vs Human Population"
)

#29.Collisions per capita tend to be higher in regions with lower human populations, while more populated regions show lower per-capita collision rates.
# This makes sense in Newfoundland, where rural areas often have high moose densities but fewer people, increasing collision risk per person.






