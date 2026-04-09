install.packages(dplyr)
library(dplyr)
install.packages("dplyr")
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
View(moosedata)
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min(df$Year)
year_min <- min(moose_sel$Year)
moose_max <- max(Estimated_Moose_Pop, na.rm = TRUE)
moose_max <- max(Estimated_Moose_Pop, na.rm = TRUE)
moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
moose_max
plot(moosedata2$Year, moosedata2$MooseDensity,
xlab = "year",
ylab = "Moose per sq km",
main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# Subset the data for the Western_Forests region
west <- subset(moose, Region == "Western_Forests")
# Subset the data for the Western_Forests region
west <- subset(moosedata2, Region == "Western_Forests")
> plot(moosedata2$Year, moosedata2$MooseDensity,
plot(moosedata2$Year, moosedata2$MooseDensity,
+      xlab = "year",
plot(moosedata2$Year, moosedata2$MooseDensity,
xlab = "year",
ylab = "Moose per sq km",
main = "Moose density changed in western regions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moosedata2$Year, moosedata2$MooseDensity,
xlab = "year",
ylab = "Moose per sq km",
main = "Moose density changed in western regions over time)
plot(moose_west$Year, moose_west$MooseDensity,
xlab = "year",
plot(moose_west$Year, moose_west$MooseDensity,
xlab = "year",
ylab = "Moose per sq km",
main = "Moose density in western regions over time")
plot(moose_west$Year, moose_west$MooseDensity,
+      xlab = "year",
plot(moose_west$Year, moose_west$MooseDensity,
xlab = "year",
ylab = "Moose per sq km",
main = "Moose density in western regions over time")
plot(moose_west$Year, moose_west$MooseDensity,
type = 1,
xlab = "year",
ylab = "Moose per sq km",
main = "Moose density in western regions over time")
plot(moose_west$Year, moose_west$MooseDensity,)
plot(moose_west$Year, moose_west$MooseDensity, type = "1", xlab = "year", ylab = "Moose per sq km", main = "Moose density in western regions over time")
# Create the line plot
plot(western_data$Year,
western_data$Density,
type = "l",
xlab = "Year",
ylab = "Moose Density (per km^2)",
main = "Moose Density Over Time in Western Forests")
# Create the line plot
plot(moose_west$Year,
moose_west$MooseDensity,
type = "l",
xlab = "Year",
ylab = "Moose Density (per km^2)",
main = "Moose Density Over Time in Western Forests")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- moose_2020 %>%
filter(MooseDensity > 2.0)
arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(mean_browse = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
avg_browse_reg <- moosefinal %>%
group_by(Ecoregion) %>%
summarise(AverageBrowsing = mean(AverageBrowsing, na.rm = TRUE)) %>%
avg_browse_reg
avg_browse_reg <- moose %>%
group_by(Ecoregion) %>%
summarise(AverageBrowsing = mean(AverageBrowsing, na.rm = TRUE)) %>%
arrange(desc(AverageBrowsing))   # Highest at top, lowest at bottom
avg_browse_reg <- moosefinal %>%
group_by(Ecoregion) %>%
summarise(AverageBrowsing = mean(AverageBrowsing, na.rm = TRUE)) %>%
arrange(desc(AverageBrowsing))   # Highest at top, lowest at bottom
rlang::last_trace()
avg_browse_reg <- sap_reg_browse %>%
arrange(desc(AverageBrowsing))
avg_browse_reg <- sap_reg_browse %>%
arrange(desc(BrowsingScore))
avg_browse_reg <- sap_reg_browse %>%
arrange(desc(mean_browse))
print(avg_browse_reg)
View(avg_browse_reg)
# Highest browsing: <Northern_Peninsula_Forests>
# Lowest browsing: <StraitOfBelleIsleBarrens>
sap_reg_height <- saplings %>%
group_by(Ecoregion) %>%
summarize(meanheight = mean(Height, na.rm = TRUE))
print(sap_reg_height)
# Identify ecoregions with average heights < 20 cm (severely browsed by moose)
sap_reg_height_low <- sap_reg_height %>%
filter(avg_height < 20)   # ecoregions with severe browsing (<20 cm)
# Identify ecoregions with average heights < 20 cm (severely browsed by moose)
sap_reg_height_low <- sap_reg_height %>%
filter(Height < 20)   # ecoregions with severe browsing (<20 cm)
# Identify ecoregions with average heights < 20 cm (severely browsed by moose)
sap_reg_height_low <- sap_reg_height %>%
filter(meanheight < 20)   # ecoregions with severe browsing (<20 cm)
print(sap_reg_height_low)
View(sap_reg_height_low)
sap_spe_browse <- saplings %>%
group_by(Species) %>%
summarize(mean_browse = mean(BrowsingScore, na.rm = TRUE))
print(sap_spe_browse)
View(sap_spe_browse)
avg_browse_spe <- sap_spe_browse %>%
arrange(desc(mean_browse))
avg_browse_spe
View(avg_browse_spe)
fir_reg_browse <- browse_data %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(mean_browse = mean(BrowsingScore, na.rm = TRUE))
fir_reg_browse <- avg_browse_spe %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(mean_browse = mean(BrowsingScore, na.rm = TRUE))
fir_reg_browse <- avg_browse_reg %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(mean_browse = mean(BrowsingScore, na.rm = TRUE))
View(saplings)
fir_reg_browse <- saplings %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(mean_browse = mean(BrowsingScore, na.rm = TRUE))
View(fir_reg_browse)
avg_browse <- tapply(fir_reg_browse$mean_browse,
fir_reg_browse$Ecoregion,
mean)
barplot(avg_browse,
main = "Average Browsing Intensity by Ecoregion",
xlab = "Ecoregion",
ylab = "Average Browsing Intensity",
col = "forestgreen")
barplot(fir_reg_browse$mean_browse, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing intensity", main = "Average Browsing Intensity by Ecoregion", col = "forestgreen", #forestgreen cex.names = 0.6)
barplot(fir_reg_browse$mean_browse,
names.arg = fir_reg_browse$Ecoregion,
xlab = "Ecoregion",
ylab = "Average Browsing Intensity",
main = "Average Browsing Intensity by Ecoregion",
col = "forestgreen",
cex.names = 0.6)
View(avg_browse_reg)
spruce_reg_browse <- moose_data %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarise(mean_browse = mean(BrowsingScore, na.rm = TRUE))
spruce_reg_browse <- saplings %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarise(mean_browse = mean(BrowsingScore, na.rm = TRUE))
barplot(spruce_reg_browse$mean_browse,
+     names.arg = spruce_reg_browse$Ecoregion,
spruce_reg_browse <- saplings %>%
+     filter(Species == "Black_Spruce") %>%
+     group_by(Ecoregion) %>%
+     summarise(mean_browse = mean(BrowsingScore, na.rm = TRUE))
barplot(spruce_reg_browse$mean_browse,
+     names.arg = spruce_reg_browse$Ecoregion,
barplot(spruce_reg_browse$mean_browse,
+     names.arg = spruce_reg_browse$Ecoregion,
barplot(fir_reg_browse$mean_browse,
+     names.arg = fir_reg_browse$Ecoregion,
barplot(
height = spruce_reg_browse$mean_browse,
names.arg = spruce_reg_browse$Ecoregion,
xlab = "Ecoregion",
ylab = "Average Browsing Score",
main = "Average Moose Browsing on Black Spruce by Ecoregion",
col = "forestgreen",
las = 2
)
View(spruce_reg_browse)
#Black Spruce browsing tends to be lower and more uniform across ecoregions, while Balsam Fir browsing is generally higher and shows stronger regional variation due to its greater palatability.
sap_reg_tally<- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>%
print()
sap_spe_tally <- sap_clean %>%
group_by(Species) %>%
tally() %>%
print()
# The SaplingStudy dataset is not evenly distributed; some ecoregions and species appear far more often,
# while others have very few samples, making them underrepresented.
# Recognizing bias in ecological datasets matters because uneven sampling can distort patterns we think we see
# and lead to inaccurate conclusions about species, habitats, or ecological processes.
moose_2020b <- moose_clean %>%
filter(Year == 2020) %>%
mutate(MooseDensity = Moose / Area)
View(moose_clean)
moose_2020b <- moose_clean %>%
filter(Year == 2020) %>%
mutate(MooseDensity = Estimated_Moose_Pop / Area)
View(moose_2020b)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")
View(moose_sap)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(
mean_browse = mean(BrowsingScore, na.rm = TRUE),
mean_moose = mean(MooseDensity, na.rm = TRUE)
)

print(sum_spe_browse)
View(sum_spe_browse)
library(ggplot2)
install.packages(ggplot2)
library(ggplot2)
#yes, the evidence generally supports the hypothesis. Moose tend to be choosy when food is abundant and shift toward generalist browsing when population density rises.
#moose tend to favour willows and alders the most. They tend to browse black spruce the least.
#Black ash is not shown on the figure. this is because there is a small data sample for black ash consisting of 1 entry from the western forests.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
View(moose_coll)
moose_coll2 <- moose_coll %>%
rename_with(~ "Ecoregion", study_sites)
left_join(moose_coll2, moose_2020, by = "Ecoregion")
rename_with(moose_coll = moosecoll2)
rename_with(moose_coll2 = moose_coll)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
# Scatterplot of Moose Density vs. Collisions in 2020
plot(MooseDensity, collisions2020,
xlab = "Moose Density",
ylab = "Moose-Vehicle Collisions (2020)",
main = "Relationship Between Moose Density and Collisions",
pch = 19, col = "darkgreen")
plot(coll_merge$MooseDensity,
coll_merge$collisions2020,
xlab = "Moose Density",
ylab = "Collisions in 2020",
main = "Scatterplot of Moose Density vs 2020 Collisions")
# The scatterplot shows that a higher moose density typically results in more collisions. However, collisions in the avalon forests region are much higher even though its moose density is less than 1.
coll_merge_per_capita <- coll_merge %>%
mutate(coll_per_capita = collisions2020 / human_pop)
View(coll_merge_per_capita)
plot(coll_merge_per_capita$human_pop,
coll_merge_per_capita$coll_per_capita,
xlab = "Human Population",
ylab = "Collisions per Capita",
main = "Scatterplot of Collisions per Capita vs Human Population")
# The scatterplot shows that a higher human population results in fewer collisions. this is becasuse in higher populated area there is less forest for moose leasing to less moose in the area.



