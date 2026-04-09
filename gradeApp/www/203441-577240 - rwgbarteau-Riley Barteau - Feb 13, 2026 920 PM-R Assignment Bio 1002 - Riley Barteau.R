install.packages("dplyr")
install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(moose_sel$Year)
year_min
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
xlab = "Year",
ylab = "Moose per sq km",
main = "Moose Density in Newfoundland Ecoregions Over Time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
type = "l",
xlab = "Year",
ylab = "Moose per sq km",
main = "Moose Density in Western Forests Over Time")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()
moosefinal
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()
avg_browse_reg
avg_browse_reg
avg_browse_reg <- sap_reg_browse %>%
arrange(desc(AverageBrowsing))
# The ecoregion at the top has the highest average browsing
# The ecoregion at the bottom has the lowest average browsing
avg_browse_reg
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(AverageHeight = mean(Height)) %>%
print()
sap_reg_height_low <- sap_reg_height %>%
filter(AverageHeight < 20) %>%
print()
# These ecoregions have average heights less than 20 cm
# and may be severely browsed by moose.
sap_spe_browse <- sap_clean %>%
group_by(Species) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
print()
avg_browse_spe <- sap_spe_browse %>%
arrange(desc(AverageBrowsing))
# The species at the top has the highest browsing
# The species at the bottom has the lowest browsing
avg_browse_spe
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore))
fir_reg_browse
barplot(fir_reg_browse$AverageBrowsing,
names.arg = fir_reg_browse$Ecoregion,
xlab = "Ecoregion",
ylab = "Average Browsing Intensity",
main = "Balsam Fir Browsing by Ecoregion",
col = "forestgreen",
cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore))
spruce_reg_browse
barplot(spruce_reg_browse$AverageBrowsing,
names.arg = spruce_reg_browse$Ecoregion,
xlab = "Ecoregion",
ylab = "Average Browsing Intensity",
main = "Black Spruce Browsing by Ecoregion",
col = "darkblue",
cex.names = 0.6)
# Balsam fir is much more severely browsed in nearly all ecoregions, indicating that moose prefer balsam fir.
sap_reg_tally <- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>%
print()
sap_spe_tally <- sap_clean %>%
group_by(Species) %>%
tally() %>%
print()
#This dataset seems mostly evenly distributed with Balsam fir being slightly overrepresented and black ash being reprsented in only one sample.
#It is important to recognize bias because uneven sampling can make certain species or regions appear more heavily browsed due to higher sampling effort rather than true ecological pattern.
moose_2020b <- moose_clean %>%
filter(Year == 2020) %>%
mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b,
sap_clean,
by = "Ecoregion",
relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(
AvgBrowsing = mean(BrowsingScore),
AvgDensity = mean(MooseDensity)
) %>%
print()
library(ggplot2)
# There is partial support for the hypothesis. At lower moose densities, browsing appears more selective, while at higher densities browsing becomes more similar across species.
# Moose seem to favour willow and alder trees.
# Black ash does not appear in the figure because it had no matching 2020 moose density data after the dataset merge, so it was excluded from the summarized dataset used for plotting.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
"Long_Range_Barrens","Central_Forests","Western_Forests",
"EasternHyperOceanicBarrens","Maritime_Barrens",
"Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
plot(coll_merge$MooseDensity,
coll_merge$collisions2020,
xlab = "Moose Density",
ylab = "Number of Collisions (2020)",
main = "Moose Density vs Moose-Vehicle Collisions")
# There appears to be a positive correlation between moose density and the number of collisions, regions with higher density tend to have more collisions, though some outliers exist.
coll_merge_per_capita <- coll_merge %>%
mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$human_pop,
coll_merge_per_capita$coll_per_capita,
xlab = "Human Population",
ylab = "Collisions per Capita",
main = "Moose Collisions per Capita vs Human Population")
# Regions with smaller human populations sometimes show higher collisions per capita because even a moderate number of collisions represents a large proportion relative to population size. This makes ecological sense since rural areas often overlap more directly with moose habitat.
