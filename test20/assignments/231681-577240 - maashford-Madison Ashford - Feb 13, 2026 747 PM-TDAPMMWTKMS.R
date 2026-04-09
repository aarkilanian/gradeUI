#all code that worked 
#4(PART1 MOOSE POP)

#View(MoosePopulation)
library(dplyr)
moosedata <- MoosePopulation
moose_clean <- na.omit(moosedata)
moose_sel <- dplyr::select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moosedata$Year)
year_min <- min(moosedata$Year)
max(moosedata$Year) 
moose_max <- max(moosedata$Year)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_max <- max(moosedata$Year)
moosedata2 <- mutate(moose_sel,
                     MooseDensity = Estimated_Moose_Pop / Area)
head(moosedata2)
colnames(moosedata2)    
moose_west <- filter(moosedata2, Ecoregion == "western_forests")
plot(moosedata2$Year, moosedata2$MooseDensity, type = "l", pch = 2, xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()













#5(SAMPLING)

#View(SaplingStudy)
sap_clean <-SaplingStudy
sap_clean <- na.omit(SaplingStudy)
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_BrowsingScore = mean(BrowsingScore))
avg_browse_reg <- arrange(sap_reg_browse, desc(mean_BrowsingScore))
print(avg_browse_reg)
# Lowest average browsing is the Boreal Shield
# Highest average browsing is the Western Forests
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20) %>%
  print()

sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20) %>%
  print()
# Ecoregions with severe browsing AverageHeight is less then < 20 cm 
# Northern Peninsula Forests and Western Forests have average heights
sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
print(avg_browse_spe)
# Highest browsing species is the Black Ash with 5
# Lowest browsing species is the Black Spruce with 2.33 
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore))

print(fir_reg_browse)

spruce_reg_browse <- sap_clean %>% 
  filter(Species == "Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
barplot(fir_reg_browse$AverageBrowsing,names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Balsam Fir Browsing Intensity by Ecoregion", col = "forestgreen",cex.names = 0.6)
print(fir_reg_browse)
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Black Spruce Browsing Intensity by Ecoregion", col = "darkblue", cex.names = 0.6)

# Black Spruce generally shows Higher browsing when compared to the Balsam Fir.
# The difference is most noticeable in the Maritime Barrens BS in MB = 4.5, BF in MB = 2.5
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() 
print(sap_reg_tally)

# a) dataset does not look perfectly distributed and there are some ecoregions have noticeably more saplings counted than others, so basically certain areas may influence the averages more strongly.

# b) Recognizing bias is important because uneven sampling can affect results and make browsing patterns seem stronger or weaker than they actually are.also it helps ensure conclusions about moose feeding behaviour are more accurate.


moose_2020b <- moose_clean %>%  filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many") 
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(AvgBrowsing = mean(BrowsingScore), AvgDensity = mean(MooseDensity) )
print(sum_spe_browse)
# The pattern somewhat supports the hypothesis since browsing looks less selective
# when moose density is higher.
# Black Ash has the highest browsing, and Black Spruce has the lowest.
# The missing species probably had very few observations or missing values so it didn’t show up clearly in the figure.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,  xlab = "Moose Density", ylab = "Collisions 2020",main = "Moose Density vs Collisions")




# Regions with smaller populations tend to have higher collisions per person, because even with fewer people collision per person is increased due to each driver being more likely to experience a collision. Also just a side not while thinking about this but rural areas tend to have roads that cut through the forest whereas more populated areas tend to be more spaced out so more time to react when seeing one, and less likely for them to want to wander in those populated areas.. Unless there's salt on the roads 



