
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <-min(moose_sel$Year)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year", 
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
xlab = "Year", 
ylab = "Moose per sq km",
main = "Moose Density in Western Forest Regions over time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_highbyD <-arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020)%>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
  ### R Assignment Part 2###

saplings <- read.csv2("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE) %>%
 arrange(desc(AverageBrowsing)
 print(avg_browse_reg)

 #highest BrowsingScore: 5.0 (North_Shore_Forests and Northern_Peninsula_Forests) \
 #Lowest BrowsingScore: 0.0 (EasternHyperOceanicBarrens and Maritime_Barrens)\
 sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
summarize(mean_height = mean(Height,na.rm = TRUE) %>%
 print()           
# question 13
#Northern_Peninsula_Forests and Wedtern_Forests\
sap_reg_height_low <- sap_reg_height %>%
filter(mean(Height) < 20) %>%
print(sap_reg_height_low)

 # question 14a 
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print()
#question 14b
avg_browse_spe <- sap_clean %>%
group_by(Species) %>%
summarize (AverageBrowsing = mean(BrowsingScore, na.rm = TRUE))
arrange(desc(AverageBrowsing)) %>%
  print()


#question 15
library(dplyr)
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%
summarize(MeanBrowsing = mean(BrowsingScore, na.rm =TRUE))
print(fir_reg_browse)

#question 16
barplot(fir_reg_browse$MeanBrowsing, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregions",
        ylab = "Average Browsing Intensity", main = "Average Moose Browsing Intensity on Balsam Fir by Ecoregion",
        col = "forestgreen", cex.names = 0.5)

#question 17 a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowsing = mean(BrowsingScore)) %>%
  print()

#question 17b bar graph
barplot(spruce_reg_browse$MeanBrowsing, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregions", 
        ylab = "Average Browsing Intensity", main = "Average Moose Browsing Intensity on Black Spruce by Ecoregion",
        col = "forestgreen", cex.names = 0.5)
# question 17c black spruce vs balsam fir\
#Balsam fir is overall browsed more than black soruce.\
#this indicates that balsam fir is mored preferred by moose than black spruce in most Ecoregions.

# question 18, counted tree saplings\
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion)
tally() %>%
  print()
# question 19, tree saplings within species\
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#question 20 a
#the saplingstudy is not evenly distributed.
#North_Shore_Forests are well represented for ecoregions while balsam fir and black spruce are well represented by species.\
# straight of Belle Isle Barrens is under represented by ecoregions while black ash is under represented for species.\

#question 20b
# it is important to recognize bias in ecological datasets because uneven sampling can skew results.\
#heavily sampled regions will dominate trends while rarely sampled regions will give unreliable answers.\


#R assignment part 3
#question 21 a
moose_2020b <- moosedata2 %>%
  filter(Year == 2020)
#question 21 b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion',) relationship = "many-to-many")

#question 22, average browsing score & density for each ecoregion\
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion) %>%
summarize(mean(BrowsingScore), mean(MooseDensity)) %>%
  print()

#question 23 a
#yes, low moose density leads to high browsing on a few preferred species and higher moose density means more species show moderate to high browsing.\

#question 23 b
# most favoured would be willow ss it has highest browsing scores across densities, the least favoured would be black spruce because of its very low browsing at low density.

#question 23 c
# black ash is the unshown species.\
#likely a low amount of browsing or insufficient data for averages.\

library(dplyr)
#question 24\
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 3200, 270000,   2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#question 25 a, joining datasets
moose_coll2 <- moose_coll %>%
mutate(Ecoregion = study_sites)

#question 25 b, joining datasets
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
#question 26 a, moose density vs. moose-vehicle collisions
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density", 
     ylab = "Number of moose-vehicle Collisions (2020)",
     main = "Moose Density vs Moose-Vehicle Collisions",
     pch = 19)
#question 26b
# Higher moose density leads to more moose-vehicle collisions,1.0 has very high collisions compared to other sites.\
# Low densities have fewer collisions.

#question 27 ecoregions with the highest number of moose collisions per person\
coll_merge_per_capita <- mutate(coll_merge , coll_per_capita = collisions2020 / human_pop)

#question 28 scatterplot of coll_per_capita vs human_pop\
library(dplyr)
plot(coll_merge_per_capita$coll_per_capita,
coll_merge_per_capita$human_pop,
xlab = "Human Population",
ylab = "Moose Collisions per Capita", 
main = "Moose Collisions per Capita vs Human Population" ,
pch = 19)

#question 29\
#as human population increases, moose collisions per capita decrease, very small populations have higher per-capita collision rates.\
#this ties into Newfoundland, since rural areas have a lot of moose and long driving distances on hughways, whereas urban areas have less moose encounters and slower traffic.\