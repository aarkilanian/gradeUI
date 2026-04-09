dev.off()

# Q1
library(dplyr)

# Q2
moosedata <- read.csv("MoosePopulation.csv")

# Q3
moose_clean <- na.omit(moosedata)

# Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Q5-A
year_min <- min(moose_sel$Year)

# Q5-B
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# Q8-A
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Q8-B
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density over time in Western Forests")

# Q9-A
moose_2020 <- filter(moosedata2, Year == 2020)

# Q9-B
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Q9-C
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Q11-A
saplings <- read.csv("SaplingStudy.csv")

# Q11-B
sap_clean <- na.omit(saplings)

# Q12-A
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Q12-B
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_reg
# Highest average browsing: Northern Peninsula Forests
# Lowest average browsing: Strait of Belle Isle Barrens


# Q13-A
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageHeight = mean(Height)) %>%
  print()

# Q13-B
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
# Ecoregions w/avg. height < 20cm: Northern Peninsula Forests, Western Forests

# Q14-A
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Q14-B
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
avg_browse_spe
# Highest browsing species: Black Ash
# Lowest browsing species: Black Spruce

# Q15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Q16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing Intensity by Ecoregion",
        col = "darkgreen",
        cex.names = 0.375)

# Q17-A
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

# Q17-B
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black Spruce Browsing Intensity by Ecoregion",
        col = "darkgreen",
        cex.names = 0.3)

# Q17-C
# Compared to Balsam Fir, Black Spruce shows lower average browsing across most ecoregions.

# Q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# No, there were varying numbers of saplings counted across different regions.
# For example, North Shore Forests had 8 counted, while the Straight of Belle Isle Barrens only had 1.

# Q19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# Again, no. As example, 11 Balsam Fir saplings were counted, with only 1 Black Ash.

# Q20-A
# I do not think the dataset was remotely close to being evenly distributed.
# As mentioned in the previous answers, the data for ecoregion samples varied by 8 times,
# with species counts varying in higher amounts.
# Most notably, saplings counted in the Straight of Belle Isle Barrens are most underrepresented,
# with Black Ash being in the same regard.

# Q20-B
# Recognizing sampling bias matters,
# as overrepresented groups can throw averages and make patterns look greater/lesser than they truly are.
# If sampling isn’t balanced, we might draw the wrong conclusions about browsing intensity across ecoregions or species.

# Q21
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

# Q22
sum_spe_browse <- moose_sap %>%
  group_by(Ecoregion, Species) %>%
  summarise(
    AvgBrowsing = mean(BrowsingScore),
    AvgMooseDensity = mean(MooseDensity)
  ) %>%
  print()

# Q23
library(ggplot2)
ggplot(sum_spe_browse, aes(x = AvgMooseDensity, y = AvgBrowsing, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

# Q23-A
# Yes, browsing varies more by species at low moose density, but at higher density most species have high browsing, suggesting more general feeding.

# Q23-B
# Moose appear to favour Willow the most (highest browsing scores) and browse Black Spruce the least (lowest scores).

# Q23-C
# Black Ash is missing because it didn’t have usable data in the joined 2020 dataset, so no average point could be plotted.

# Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens",
                 "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens",
                 "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Q25-A
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

# Q25-B
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

# Q26-A
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose density (moose per sq km)",
     ylab = "Moose-vehicle collisions (2020)",
     main = "Moose density vs. collisions (2020)")

# Q26-B
# There’s a general positive relationship, with higher moose density tending to be associated with more collisions.
# One clear outlier is around Moose Density = 1.0 with ~110 collisions, being much higher than the rest.

# Q27
coll_merge_per_capita <- arrange(coll_merge_per_capita, desc(coll_per_capita))
coll_merge_per_capita
# Long Range Barrens - 0.0035 Collisions/Capita

# Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human population",
     ylab = "Collisions per person (2020)",
     main = "Collisions per capita vs. human population")

# Q29
# Yes, this data alligns with my general knowledge of moose collisions on the island.
# From my knowledge, most accidents happen along rural back roads, or sections of the Trans Canada Highway which are largely inhabited (Terra Nova Park).
# With St. John's housing most of the island's residents and falling far on the right end of the graph (250000+)
# it's not surprising to see collision rates so low in this area, as rarely moose traverse so close to the city.
