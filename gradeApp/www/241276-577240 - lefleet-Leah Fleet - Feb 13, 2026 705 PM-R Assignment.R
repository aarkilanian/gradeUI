# Title: Bio 1002 R assignment
# Name: Leah Fleet

# Question 1: ------------------------------------------------------------------
library(dplyr)

# Question 2: ------------------------------------------------------------------
moosedata <- read.csv("MoosePopulation.csv")

# Question 3: ------------------------------------------------------------------
View(moosedata)
moose_clean <- na.omit(moosedata)

# Question 4: ------------------------------------------------------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5: ------------------------------------------------------------------
# a)
year_min <- min(moose_sel$Year)
# Answer: 1904
# b)
moose_max <- max(moose_sel$Estimated_Moose_Pop)
# Answer: 41250

# Question 6: ------------------------------------------------------------------
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7: ------------------------------------------------------------------
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8: ------------------------------------------------------------------
# a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# b)
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# Question 9: ------------------------------------------------------------------
# a)
moose_2020 <- filter(moosedata2, Year == "2020" )
# b)
moose_2020_high <- filter(moose_2020, MooseDensity > "2.0")
# c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10: -----------------------------------------------------------------
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Question 11: -----------------------------------------------------------------
# a)
saplings <- read.csv("SaplingStudy.csv")
# b)
sap_clean <- na.omit(saplings)
View(sap_clean)

# Question 12: -----------------------------------------------------------------
# a) 
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(avg_BrowsingScore = mean(BrowsingScore)) %>%
  print()
# b)
avg_browse_reg <- arrange(sap_reg_browse, desc(avg_BrowsingScore))
# Highest is Northern_Peninsula_Forests  
# The lowest is StraitOfBelleIsleBarrens	

# Question 13: -----------------------------------------------------------------
# a)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(avg_TreeHeight = mean(Height)) %>%
  print()
# b)
sap_reg_height_low <- filter(sap_reg_height, avg_TreeHeight < "20") %>%
  print()
# Northern_Peninsula_Forests and Western_Forests avg height of less than 20cm

# Question 14: -----------------------------------------------------------------
# a)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(avg_BrowsingScore = mean(BrowsingScore)) %>%
  print()
# b)
avg_browse_spe <- arrange(sap_spe_browse, desc(avg_BrowsingScore))
# Highest browsing score is Black_Ash
# The lowest browsing score is Black_Spruce

# Question 15: -----------------------------------------------------------------
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(avg_BrowsingScore = mean(BrowsingScore)) %>%
  print()

# Question 16: -----------------------------------------------------------------
barplot(fir_reg_browse$avg_BrowsingScore, names.arg = 
          fir_reg_browse$Ecoregion, xlab = "Ecoregion", 
        ylab = "Average Browsing Intesity", 
        main = "Average Browsing Intensity of Balsam Fir by Ecoregion", 
        col = "forestgreen", cex.names = 0.4, las=2)

# Question 17: -----------------------------------------------------------------
# a)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(avg_BrowsingScore = mean(BrowsingScore)) %>%
  print()
# b)
barplot(spruce_reg_browse$avg_BrowsingScore, names.arg = 
          spruce_reg_browse$Ecoregion, xlab = "Ecoregion", 
        ylab = "Average Browsing Intesity", 
        main = "Average Browsing Intensity of Black Spruce by Ecoregion", 
        col = "forestgreen", cex.names = 0.3, las = 2)
# c) The Black spruce browsing is generally lower than the Balsam Fir browsing 
# in barrens ecoregions, but their comparable or higher in Forested ecoregions.

# Question 18: -----------------------------------------------------------------
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# The same number was not counted. In five of the regions there were 5 saplings 
# counted, but the others had 3,8,7, and 1 counted

# Question 19: -----------------------------------------------------------------
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
# No the same number was not counted for each, they were all different

# Question 20: -----------------------------------------------------------------
# a)
# It was not evenly distributed. In terms of ecoregions StraitOfBelleIsleBarrens 
# was underrepresented and in terms of species the Black_Ash was 
# underrepresented, while the Balsam_Fir was overrepresented
# b)
# It is important because the data is biased you can not accurately compare the 
# data to each other, as using more data allows for a higher level of accuracy.
# If you don't use enough in comparison your data may not be as accurate.

# Question 21: -----------------------------------------------------------------
# a)
moose_2020b <- moose_clean %>%
  filter(Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
# b)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', 
                       relationship = "many-to-many")

# Question 22: -----------------------------------------------------------------
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(avg_BrowsingScore = mean(BrowsingScore),
            avg_MooseDensity = mean(MooseDensity)) %>%
   print()

# Question 23: -----------------------------------------------------------------
library(ggplot2)

ggplot(sum_spe_browse, aes(x = avg_MooseDensity, y = avg_BrowsingScore, 
                           color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# a) Yes. The moose show a strong preference at low density areas and become 
# more general at higher density
# b) The moose favor Willow the most and browse Black Spruce the least.
# c) Black Ash sampling is the only not shown. This is because there is more 
# than one sapling with the same moose density and the same browsing score.
# The Black ash is being hidden by one of the Willow dots as they have the same 
# data. 

# Question 24: -----------------------------------------------------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", 
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25: -----------------------------------------------------------------
# a) 
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
# b)
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', 
                        relationship = "many-to-many")

# Question 26: -----------------------------------------------------------------
# a) 
plot(coll_merge$MooseDensity, coll_merge$collisions2020, pch = 17, 
     col = "forestgreen", xlab = "Moose Density", ylab = "collisions 2020")
# b) In general the trend is that as moose density increases so does the number 
# of collisions. There is an outlier as there is a point around 1.0 for density
# where the number of collisions skyrocket, and then it goes back to the trend.

# Question 27: -----------------------------------------------------------------
coll_merge_per_capita <- mutate(coll_merge, 
                                coll_per_capita = collisions2020/human_pop)

# Question 28: -----------------------------------------------------------------
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop, 
     pch = 17, col = "blue", xlab = "Collisions per capita", 
     ylab = "Human population")

# Question 29: -----------------------------------------------------------------
# The trend here is that when there is more people there is also more
# collisions. In the more populated areas there is more collisions because there
# is more cars on the road, so yes this trend does make sense.

