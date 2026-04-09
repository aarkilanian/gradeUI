install.packages("dplyr")  
library(dplyr)

#2
moosedata <- read.csv("C:/Users/jayde/Downloads/MoosePopulation.csv")
#3
moose_clean <- na.omit(moosedata)
#4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
max_year <-max(moose_sel$Year)
#6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
xlab= "year", 
ylab= "Moose per sq km", 
main= "Moose density in Newfoundland ecoregions over time")

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")


#8
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density Over Time in Western Forests")

#9
moose_2020<- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_by<- arrange(moose_2020_high, desc(MooseDensity))

#10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


#11
saplings <- read.csv("C:/Users/jayde/Downloads/SaplingStudy.csv")
sap_clean <- na.omit(saplings)


#12
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

#13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>%
  print()
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

#14
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

#15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore)) %>%
  print()

#16
barplot(
  fir_reg_browse$AvgBrowsing,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Balsam Fir Browsing Intensity by Ecoregion",
  col = "forestgreen",
  cex.names = 0.6
)
#17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AvgBrowsing = mean(BrowsingScore)) %>%
  print()
barplot(
  spruce_reg_browse$AvgBrowsing,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Score",
  main = "Black Spruce Browsing Intensity by Ecoregion",
  col = "darkblue",
  cex.names = 0.6
)
#17 c)  The black spruce is browsed less than the Balsam Fir, especially 
#due to the Long_Range_Barrens

#18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

#19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#20
#20a 
# Some tree species and some ecoregions are more prominant, which would mean 
# uneven sampling

#20b
#Recoginizing bias is important because ignoring bias can end up leading to
# incorrect conclusion and how moose are different in species.



#21 
moose_2020b <- moose_clean %>%
filter(Year == 2020) %>%
mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion",
relationship = "many-to-many")

#22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()

#23
install.packages("ggplot2")
library(ggplot2)
ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score"
  )

#23a
#At low moose densities, the browsing is much more selective. 
#at higher densities, it then becomes more uniform

#23b
#Moose prefer balsam fir, and browse the black spruce not as often

#23c
#One species not shown in the figure is Choke Cherry. It is now shown
#because moose often avoid due to it being toxic and fatal. 

#24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c(
  "North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens",
  "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens",
  "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens"
)
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#25
moose_coll2 <- moose_coll %>%
rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020b, moose_coll2, by = "Ecoregion")

#26
plot(
  coll_merge$MooseDensity, coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Moose-Vehicle Collisions (2020)",
  main = "Moose Density vs Vehicle Collisions"
)
#26B
#Higher moose density is generally linked with more collisions (except for 1.)

#27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

#28
plot(
  coll_merge_per_capita$human_pop,
  coll_merge_per_capita$coll_per_capita,
  log = "x",  # Use log scale for x-axis
  xlab = "Human Population (log scale)",
  ylab = "Collisions Per Capita",
  main = "Moose Collisions Per Capita vs Human Population",
  pch = 16,
  col = "darkred",
  cex = 1.3
)

#29
#Collisions per capita are higher in areas with a  smaller human population\
#Moose are usually found in forested areas, and moose would not be walking 
#through a city.

