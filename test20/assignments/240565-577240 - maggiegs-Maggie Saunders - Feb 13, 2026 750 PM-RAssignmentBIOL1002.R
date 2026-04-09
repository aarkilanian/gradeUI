# Title: My R Assignment BIOL 1002
# Author: Maggie Saunders
# Date: 02-12-2026
# Set working directory ----------------
setwd("~/Downloads")

# Q1
install.packages("dplyr")
library("dplyr")

# Q2
moosedata <- read.csv("MoosePopulation.csv")

# Q3
moose_clean <- na.omit(moosedata)

# Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Q5
min(moose_clean$Year)
year_min <- 1904
max(moose_clean$Estimated_Moose_Pop)
moose_max <- 41250

# Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Q7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab ="year", 
     ylab = "Moose per sq km", main = "Moose Density in Newfoundland ecoregions
     over time")

# Q8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "year", 
     ylab = "Moose per sq km", main = "Moose density in Western Forests 
     Ecoregion over time")

# Q9
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Q10
moosefinal <- moosedata2 %>% filter(Year == "2020") %>% 
  filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()

# Q11
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)

# Q12
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()

avg_browse_reg <- sap_reg_browse %>% arrange(desc(AverageBrowsing))
# Northern Peninsula Forests has the highest average browsing score and Strait 
# Of Belle Isle Barrens has the lowest.

# Q13
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height)) %>% print()
# Northern Peninsula Forests and Western Forests have average heights less than 
# 20cm
sap_reg_height_low <- sap_reg_height %>% filter(AverageHeight < 20) %>% print()

# Q14
sap_spe_browse <- sap_clean %>% group_by(Species) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore)) %>% print()
avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing))
# Black Ash has the highest browsing score, Black Spruce has the lowest 
# browsing score.

# Q15
fir_reg_browse <- sap_clean %>% filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% 
  print()

# Q16
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", ylab = "Average Balsam Fir Browsing Score", 
        main = "Average Balsam Fir Browsing Intensity per Ecoregion", 
        col = "pink", cex.names = 0.6)

# Q17
spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% 
  group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore)) %>% 
  print()
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", ylab = "Average Black Spruce Browsing Score",
        main = "Average Black Spruce Browsing Intensity per Ecoregion",
        col = "pink", cex.names = 0.6)
# Black Spruce has lower browsing intensity than Balsam Fir across most ecoregions.
# Balsam Fir seems to have higher browsing scores on average, showing that moose 
# prefer them over Black Spruce.

# Q18
sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print()

# Q19
sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print()

# Q20
# a) I think the SaplingStudy dataset is not evenly distributed. Some of the 
# Ecoregions (North Shore Forests) have more samples than others and some 
# species (Balsam Fir) appear more frequently.
# b) It is important to recognize bias in ecological datasets because sampling
# can be uneven and change species preference and browsing patterns. If some 
# species or regions are overrepresented, conclusions may not be accurate.

# Q21
moose_2020b <- moose_clean %>% filter(Year == 2020) %>% 
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion")

# Q22
sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% 
  summarize(AverageBrowsing = mean(BrowsingScore), 
            AverageMooseDensity = mean(MooseDensity)) %>% print()

# Q23
install.packages("ggplot2")
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AverageMooseDensity, y = AverageBrowsing, 
                           color = Species)) + geom_point(size = 3) + 
  theme_minimal() + labs(title = "Browsing Intensity Across Moose Density by
                         Species", x = "Average Moose Density", y = "Average 
                         Browsing Score")
# a) There is some support for the hypothesis. At low moose densities, scores
# vary a lot among species, demonstrating selective feeding. At higher densities
# most species show higher browsing scores, showing that there is more 
# generalized browsing for more competition.
# b) Moose seem to like Alder and Willow the most, they show consistent high 
# browsing scores. Black Spruce is browsed the least, frequently showing low
# scores.
# c) Black Ash is not represented because it does not have enough information 
# in the dataset, which makes it less visible. 

# Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests",
                 "Long_Range_Barrens", "Central_Forests", "Western_Forests",
                 "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
print(moose_coll)

# Q25
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
print(moose_coll2)
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")
print(coll_merge)

# Q26
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density",
     ylab = "Number of Collisions (2020)", main = "Moose Density vs. 
     Moose-Vehicle Collisions")
# I see a positive trend between moose density and collisions. There is one high 
# collision outlier. 

# Q27
coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)

# Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population", ylab = "Collisions Per Capita", main = 
       "Collisions Per Capita vs. Human Population")

# Q29
# There is a negative relationship between human population and collisions per
# capita. This makes sense because rural, low population areas overlap more 
# with moose habitats, increasing the risk of collision in relation to number
# of people.
