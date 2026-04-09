install.packages("dplyr")
#PART I
#Question 1
library(dplyr)

#Question 2
moosedata <- read.csv ("MoosePopulation.csv")

#Question 3
View(moosedata)
moose_clean <- na.omit(moosedata)

#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5a
year_min <- min(moosedata$Year)

#Question 5b
moose_max <- max(moose_clean$Estimated_Moose_Pop)

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#Question 8b ***
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests region over Time")

#Question 9a
moose_2020 <- filter(moosedata2, Year == "2020")

#Question 9b
moose_2020_high <- filter (moose_2020, MooseDensity > "2.0")

#Question 9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter (MooseDensity > "2.0") %>%
  arrange(desc(MooseDensity)) %>%
  print()


#PART II
#Question 11a
saplings <- read.csv("SaplingStudy.csv")
#Question 11b
sapclean <- na.omit(saplings)

#Question 12a
sap_reg_browse <- sapclean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing=mean(BrowsingScore)) %>%
  print()
#Question 12b
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
#Northern Peninsula Forests had the highest average browsing score
#Strait of Belle Isle Barrens had the lowest average browsing score

#Question 13a
sap_reg_height <- sapclean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight=mean(Height)) %>%
  print()

#Question 13b
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < "20") %>%
  print ()

#Question 14a
sap_spe_browse <- sapclean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing=mean(BrowsingScore)) %>%
  print()
#Question 14b
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
#Black Ash has the highest browsing score
#Black Spruce has the lowest browsing score

#Question 15
fir_reg_browse <- sapclean %>%
  filter (Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing=mean(BrowsingScore)) %>%
  print()

#Question 16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Intensity", main = "Average Browsing Intensity in Different Ecoregions Containing Balsam Fir", col = "forestgreen", cex.names = 0.6) #Reduces x-axis label size for readability

#Question 17a
spruce_reg_browse <- sapclean %>%
  filter (Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing=mean(BrowsingScore)) %>%
  print()

#Question 17b
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Intensity", main = "Average Browsing Intensity in Different Ecoregions Containing Balsam Fir", col = "forestgreen", cex.names = 0.6) #Reduces x-axis label size for readability

#Question 17c
#The average browsing scores for Balsam Fir are generally higher than the average browsing scores of Black Spruce.

#Question 18
sap_reg_tally <- sapclean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

#Question 19
sap_spe_tally <- sapclean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#Question 20a***
#The SaplingStudy dataset is not evenly distributed. The North Shore Forests are over-represented compared to the Maritime Forests and Strait of Belle Isle Barrens regions that are very much underrepresented.

#Question 20b***
#It is important to recognize bias in ecological datasets to ensure a proper estimation to implement effective environmental controls and policies.

#Question 21a
moose_2020b <- filter(moose_clean, Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()

#Question 21b
moose_sap <- left_join(moose_2020b, sapclean, by = 'Ecoregion', relationship = "many-to-many")

#Question 22
sum_spe_browse <- moose_sap %>%
  group_by(Species) %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing=mean(BrowsingScore), AverageMooseDensity=mean(MooseDensity)) %>%
  print()

#Question 23a
#The evidence supports the researchers' hypothesis.
#Moose show strong preferences at low density and shift to more generalist browsing at higher density.

#Question 23b
#Moose favour the Willow species the most.
#Moose browse the Black Spruce species the least.

#Question 23c
#Black Ash is not shown on the figure. It was only spotted in one Ecoregion, and so cannot be representative of the entire species in Newfoundland.

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25a
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

#Question 25b
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")

#Question 26a
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose per sq km",
     ylab = "Number of moose-vehicle Collisions",
     main = "A Graph showing the relationship between Moose Density and the Number of Moose-Vehicle Collisions")

#Question 26b
#The greater the moose density, the greater the number of moose-vehicle collisions, and vice-versa. 
#Avalon Forest is an outlier to this relationship. Its moose density is approximately 1.0 moose per sq km and it has over 100 moose-vehicle collisons!

#Question 27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "No. of People",
     ylab = "No. of Collisions per Person",
     main = "Moose Collisions per Capita vs. Human Population across Different Ecoregions")

#Question 29
#As the human population increases, the number of collisions per person decreases.
#The trend makes sense. As human population increases, there are fewer moose per person, and so fewer moose collisions per person.

        