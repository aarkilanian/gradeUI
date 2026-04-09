#question 1
install.packages("dplyr")
library(dplyr)

# question 2

moosedata <- read.csv("MoosePopulation.csv")

# question 3

moose_clean <- na.omit(moosedata)

# 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#5 a
year_min <- min(moose_sel$Year)
year_min
# the oldest year is 1904.

# 5 b

moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
# 41250

# 6

moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# 7

plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time", type = "l")

# 8 a

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# 8 b
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forest Region over time", , type = "l")
# 9 a

moose_2020 <- filter(moosedata2, Year == "2020")

# 9b
moose_2020_high <- filter(moose_2020, MooseDensity > 2)

# 9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


#11 a
saplings <- read.csv("SaplingStudy.csv")

# 11 b

saplings_clean <- na.omit(saplings)

# 12 a
sap_reg_browse <- saplings_clean %>%
  group_by(Ecoregion) %>%
  summarize(
    AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)
  )

print(sap_reg_browse)

#1 Avalon_Forests                           2   
#2 Central_Forests                          4   
#3 EasternHyperOceanicBarrens               2.4 
#4 Long_Range_Barrens                       2.6 
#5 Maritime_Barrens                         1.83
#6 North_Shore_Forests                      4.38
#7 Northern_Peninsula_Forests               4.57
#8 StraitOfBelleIsleBarrens                 1   
#9 Western_Forests                          4.5 

# 12 b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

print(avg_browse_reg)
# northern peninsula forest has the highest and
#strait of belle isle barrens has the lowest.

#13 a

sap_reg_height <- saplings_clean %>%
  group_by(Ecoregion) %>%
  summarize(
    AverageHeight = mean(Height, na.rm = TRUE)
  )

print(sap_reg_height)

# 13 b
sap_reg_height_low <- filter(sap_reg_height, AverageHeight<20 )
print(sap_reg_height_low)
# northern peninsula forests and western forests had an average height less than 20

#14 a
sap_spe_browse <- saplings_clean %>%
  group_by(Species) %>%
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE)
  )

print(sap_spe_browse)

# 14 b
avg_browse_spe <- arrange(
  sap_spe_browse,
  desc(mean_BrowsingScore)
)

print(avg_browse_spe)
#Black ash has the highest mean 
#Black spruce has the lowest mean 

# 15
fir_reg_browse <- saplings_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE)
  )

print(fir_reg_browse)
# highest mean was North_Shore_Forests
#lowest mean was Long_Range_Barrens


#16
barplot(
  fir_reg_browse$mean_BrowsingScore,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Mean Browsing Score",
  main = "Average Moose Browsing on Balsam Fir by Ecoregion",
  col = "red",
  cex.names = 0.7
)

#17 a
spruce_reg_browse <- saplings_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE)
  )

print(spruce_reg_browse)
# 17 b
barplot(
  spruce_reg_browse$mean_BrowsingScore,
  names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Mean Browsing Score",
  main = "Average Moose Browsing on Spruce Region by Ecoregion",
  col = "red",
  cex.names = 0.7
)

#17 c
# for the balsam fir for the third and fifth bars there is a number of moose browsing 
#but for the balasam spruce the mean is 0 for these collums. 

# 18
sap_reg_tally<- saplings_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#there was not the same number in each ecoregion.
#most of them were five but it ranged from 1 to 8

# 19
sap_spe_tally<- saplings_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
# No there was not the same number of tree saplings for each species
#The numbers went from 8-11

#20a
# The SaplingStudy dataset does not appear to be evenly distributed.
# Some ecoregions have many more sampled sites than others, and certain tree species
# (e.g., common conifers such as Balsam Fir or Black Spruce) appear overrepresented,
# while less common species and some ecoregions are underrepresented in the dataset.


#20b
# It is important to recognize bias in ecological datasets because some species or
# regions may be sampled more than others, which can influence the results.
# Being aware of this helps avoid over or under estimating ecological patterns
# and leads to more realistic conclusions.



#21
moose_2020b <- moose_2020 
moose_sap <- left_join(moose_2020b, saplings_clean, by = 'Ecoregion', 
                       relationship = "many-to-many")
# 22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE),
    mean_MooseDensity  = mean(MooseDensity, na.rm = TRUE)
  )

print(sum_spe_browse, n=500)

#23
library(ggplot2)

ggplot(sum_spe_browse, aes(x = mean_MooseDensity, y = mean_BrowsingScore, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#23a 
# yes, there is evidence that supports this hypothesis, the figure shows at low moose density
#browsing intensity is high on a few specices, while at higher densities browing increases
#across multiple species, indicating more generalist feedings. 


#23b
#moose appear to favour willow along with alder which show the highest browing scores consistentaly 
#black spruce is shiwn to be browsed the least across moose densities 

#23c 
#one sapling species is not shown, which is black ash, because it shows little to no browsing pressure
#this means there was insufficent variation or data to include it in the figure 



# 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# 25a

moose_coll2 <- moose_coll %>%
  rename_with(~ "Ecoregion", study_sites)

print(moose_coll2)

# 25b

coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion", relationship = "many-to-many")

print(coll_merge)

#26 a
plot(
  coll_merge$MooseDensity,
  coll_merge$collisions2020,
  xlab = "Moose Density",
  ylab = "Number of Moose-Vehicle Collisions (2020)",
  main = "Relationship Between Moose Density and Vehicle Collisions"
)

# 26b
# Overall, moose-vehicle collisions tend to increase as moose density increases.
# There are a few ecoregions that show unusually high or low collision numbers compared
# to their moose density, which suggests other factors may also be influencing collisions.


# 27
coll_merge_per_capita <- coll_merge %>%
  mutate(
    coll_per_capita = collisions2020 / human_pop
  )

print(coll_merge_per_capita)

# ordered to find the highest
coll_merge_per_capita <- coll_merge %>%
  mutate(
    coll_per_capita = collisions2020 / human_pop
  ) %>%
  arrange(desc(coll_per_capita))

print(coll_merge_per_capita)

# 28
plot(
  coll_merge_per_capita$coll_per_capita,
  coll_merge_per_capita$human_pop,
  xlab = "Moose Collisions per Person",
  ylab = "Human Population",
  main = "Human Population vs Moose Collisions per Capita"
)

#29
# Regions with higher moose densities generally have more moose-vehicle collisions,
# especially where moose habitat overlaps with roads and human activity.
# This trend makes sense in Newfoundland, where moose are abundant and many roads pass
# through forested areas that moose frequently use.