library("dplyr")
Moosedata <- read.csv("MoosePopulation.csv")
View(Moosedata)
Moosedata <- na.omit(Moosedata)
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question 5a Ans 1904
min_year <- min(Moosedata$Year, na.rm = TRUE)
#Question 5b Ans 41250
max(Moosedata$Estimated_Moose_Pop)
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
# Question 6
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)
plot(Moosedata$Year, Moosedata$MooseDensity,
xlab = "year",
ylab = "Moose per sq km",
main = "Moose density in Newfoundland ecoregions over time")
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")
plot(MooseDataWest$Year, MooseDataWest$Estimated_Moose_Pop, 
     type = "l",
     xlab = "year", 
     ylab = "density", 
     main = "Moose density in Western_Forest Region")
Moosedata_2020 <- filter(Moosedata, Year == 2020)
Moosedata_2020_b <- filter(Moosedata_2020, MooseDensity > 2.0)
arrange(Moosedata_2020_b, desc(MooseDensity))
MooseData_final <- Moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>% 
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
  
  
SaplingStudy <- read.csv("SaplingStudy.csv")
View(SaplingStudy)
SaplingStudy <- na.omit(SaplingStudy)
BrowsingScore <- SaplingStudy %>%
group_by(Ecoregion) %>%
summarize(Mean_Browsing = mean(BrowsingScore))
print(BrowsingScore)
# 12b Northern Peninsula Forests
HeightSummary <- SaplingStudy %>%
group_by(Ecoregion) %>%
summarize(Mean_Browsing = mean(Height))
print(HeightSummary)
# 13 sap_reg_height
# 13b Northern_Peninsula_Forests,Western_Forests
SpeciesSummary <- SaplingStudy %>%
group_by(Species) %>%
summarize(Mean_Browsing = mean(BrowsingScore))
print(SpeciesSummary)

# 14, 14b Highest Score=Black_Ash Lowest Score=Black_SpruceBalsamFir<- SaplingStudy %>%
#filter(Species == "Balsam_Fir") %>%
#group_by(Ecoregion) %>%
#summarize(Mean_Browsing = mean(BrowsingScore))
#print(BalsamFir)

barplot(BalsamFir$Mean_Browsing,
names.arg = BalsamFir$Ecoregion,
xlab = "Ecoregion",
ylab = "Mean Browsing",
main = "Balsam Fir Browsing Intensity",
col = "forestgreen",
cex.names = 0.6)
Black_Spruce <- SaplingStudy %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(Mean_Browsing = mean(BrowsingScore))
print(Black_Spruce)
barplot(Black_Spruce$Mean_Browsing,
names.arg = Black_Spruce$Ecoregion,
xlab = "Ecoregion",
ylab = "Mean Browsing",
main = "Black Spruce Browsing Intensity",
col = "blue",
cex.names = 0.6)
# 17c Comparing Black Spruce to Balsam fir, Black Spruce no density in the Eastern Hyper Oceanic and Maritime Barrens.
sap_reg_tally<- SaplingStudy %>%
group_by(Ecoregion) %>%
tally() %>%
print()
sap_spe_tally<- SaplingStudy %>%
group_by(Species) %>%
tally() %>%
print()
# 20a I think that the sapling study is not evenly distributed for example in the Species tally count the Balsam_fir is 11 and Black ash is 1
# 20b Its important to recognize bias data sets because, it misrepresents the data leading to distorted results. 
Moose_2020b <- Moosedata %>% 
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(Moose_2020b, SaplingStudy, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(
AvgBrowsing = mean(BrowsingScore, na.rm = TRUE),
AvgDensity = mean(MooseDensity, na.rm = TRUE)
) %>%
print()
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# 23a Yes, this graph supports their hypothesis.It shows there is a higher browsing score of moose at a higher density of trees. Where as the lower the density there is less dense amout of moose browsing. 
# 23b Moose favor willow, and they browse the least with Black Spruce
# 23c Black ash is not shown on the figure because its hidden under another plot, beacause the values are lower.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
rename_with(~ "Ecoregion", .cols = study_sites)
coll_merge <- left_join(Moose_2020b, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions2020)
#26b The trends found on this graph there is one outlier at the top of the graph, and there is a upwards trend of density
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$coll_per_capita, coll_merge$human_pop)
#29 No, this does not make sense bases on what I know about the human and moose population in Newfoundland and Labrador. I think there is more collisions in Newfoundland and Labrador then what this graph shows.
