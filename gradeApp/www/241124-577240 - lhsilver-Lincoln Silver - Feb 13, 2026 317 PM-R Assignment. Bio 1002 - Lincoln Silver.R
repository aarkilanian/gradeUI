install.packages("dplyr")
library("dplyr")
Moose <- read.csv("MooseData/MoosePopulation.csv")
View(Moose)
na.omit(Moose)
moose_clean <- na.omit(Moose)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
min(moose_sel$Year)
year_min <- 1904
max(moose_sel$Estimated_Moose_Pop)
moose_max <- 41250
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

saplings <- read.csv(file.choose())
## this line was causing issue saplings <- read.csv("(R part 2/SaplingStudy(1).csv")
View(saplings)
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
summarise(AverageBrowsing = mean(BrowsingScore))%>%
print()

avg_browse_reg <- sap_clean %>%
  group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
arrange(desc(AverageBrowsing)) %>%
print()
# Northern_Peninsula_Forests have the most moose browsing
# Straight_Of_Belle_Isle_Barrens have the least moose browsing

sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

sap_reg_height_low <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(mean_height = mean(Height)) %>%
filter(mean_height < 20) %>%  
print()
# Eco regions with average tree heights less than 20cm are considered severly browsed
# Northern_Peninsula_Forests and Western_Forests
 
avg_browse_reg<- sap_clean %>%
group_by(Species) %>%
summarize(AverageBrowsing = mean(BrowsingScore)) %>%
arrange(desc(AverageBrowsing)) %>%
print()  

# Black_Ash has the highest browsing score of 5
# Black_Spruce has the lowest browsing score of 2.33

fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
print()

barplot(fir_reg_browse$mean_BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Mean_Browsing", main = "Mean Browsing of Balsam Fir in Each Ecoregion", col = "red", cex.names = 0.6)
spruce_reg_browse <-sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarize(mean_browsing = mean(BrowsingScore)) %>%
print()
barplot(spruce_reg_browse$mean_browsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Mean_Browsing", main = "Mean Browsing of Black Spruce in Each Ecoregion", col = "orange", cex.names = 0.6)
sap_reg_tally <- sap_clean %>%
group_by(Species) %>%
tally() %>%
print()
sap_spe_tally <- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>%
print()
# No, the same number of tree saplings were not counted for each species
# No, I believe that the SaplingStudy dataset is not evenly distributed across all Ecoregions
# Straight_Of_Bell_Isle_Barrens is an underrepresented ecoregion as it only has 1 tree, Northern_Shore_Forests is an overrepresented ecoregion at 8 trees. 
# Recognizing bias in ecological datasets is important because bias can disturb results to show unfair and inaccurate data. This would mean the audience of the data would be led to believe something is true, when it is inaccurate.

moose_2020b <- moose_clean %>%
filter(Year == "2020") %>%
mutate(MooseDensity = Estimated_Moose_Pop/Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(AvgDensity = mean(BrowsingScore), 
          AvgBrowsing = mean(MooseDensity)) %>%
print()

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# The researchers hypothesis is not supported, as the same species had high and low browsing scores in comparison to each other.
# The sapling species that moose prefered the most, therefore getting the highest browsing score was the Willow. The lowest browsing score was for the Black_Spruce.
# Black_Ash is not shown on the figure, likely because of the fact that it had no broswing activity and might not have been found in study plots.
 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll%>%
rename(Ecoregion = study_sites) 
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")

plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Collisions 2020", main = "Moose Density vs. Collisions")
coll_per_capita <- mutate(moose_coll2, coll_per_capita = collisions2020 / human_pop)
coll_merge_per_capita <- coll_per_capita
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population", 
     ylab = "Moose Collisions per Capita", 
     main = "Moose Collisions per Capita vs Human Population")
# The trend of the graph shows that as human population increases, moose collisions decrease. This makes sense, as in a populated area such as St. Johns, you rarely hear about moose collisions.