############################################################
# BIOL 1002: Quantitative Methods R Assignment
# Name: Grace White
# Student #: 202311319
# Date:13/02/26
############################################################

# Question 1
# (Install ONLY once; after it is installed you can keep it commented out)
# install.packages("dplyr")
library(dplyr)

# Set where your CSV files are located (change if needed)
data_dir <- "~/Downloads"

# =========================
# Part I: Moose Populations
# =========================

# Question 2
moosedata <- read.csv(file.path(data_dir, "MoosePopulation.csv")) %>%
  mutate(across(where(is.character), trimws))

# Question 3
moose_clean <- na.omit(moosedata)

# Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5a
year_min <- min(moose_sel$Year)

# Question 5b
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7a
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8b
plot(moose_west$Year,
     moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density over time in Western_Forests")

# Question 9a
moose_2020 <- filter(moosedata2, Year == 2020)

# Question 9b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

# Question 9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# =========================
# Part II: Tree Sapling Study
# =========================

# Question 11a
saplings <- read.csv(file.path(data_dir, "SaplingStudy.csv")) %>%
  mutate(across(where(is.character), trimws))

# Question 11b
sap_clean <- na.omit(saplings)

# Question 12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore), .groups = "drop") %>%
  print()

# Question 12b
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
# Answer: The ecoregion with the average highest score is Northern_Peninsula_Forests

# Question 13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(Height), .groups = "drop") %>%
  print()

# Question 13b
short_ecoregions <- filter(sap_reg_height, AverageHeight < 20)
# Answer: The ones under 20 are the Northern_Peninsula_Forests and the Western_Forests

# Question 14a
sap_spp_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore), .groups = "drop") %>%
  print()

# Question 14b
sap_spp_browse_sorted <- arrange(sap_spp_browse, desc(AverageBrowsing))
# Answer: Highest avg browsing species: Black_Ash (n = 1, but highest mean)
# Answer: Lowest avg browsing species: Black_Spruce

# Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowseScore = mean(BrowsingScore), .groups = "drop")

# Question 16
barplot(fir_reg_browse$MeanBrowseScore,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        cex.names = 0.6)

# Question 17a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(MeanBrowseScore = mean(BrowsingScore), .groups = "drop") %>%
  print()

# Question 17b
barplot(spruce_reg_browse$MeanBrowseScore,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Average Black Spruce Browsing Intensity by Ecoregion",
        cex.names = 0.6)

# Question 17c
# Answer: Black Spruce tends to have lower browsing intensity than Balsam Fir in most ecoregions, which means moose generally prefer Balsam Fir.
# Both species still show similar trends across ecoregions, with some areas having higher browsing than others.

# Question 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

# Question 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

# Question 20a
# Answer: The SaplingStudy dataset is not evenly distributed, as some ecoregions and tree species have many more observations than others.

# Question 20b
# Answer: Recognizing bias matters because uneven sampling can lead to inaccurate conclusions about patterns in nature. If some areas/species are sampled more than others,
# results may not represent the whole system.

# =======================================
# Part III: Creating and Joining Datasets
# =======================================

# Question 21a
moose_2020b <- moosedata2 %>%
  filter(Year == 2020)

# Question 21b
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")

# Question 22
sum_reg_tally <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AverageBrowsingScore = mean(BrowsingScore),
    AverageMooseDensity = mean(MooseDensity),
    .groups = "drop"
  ) %>%
  print()

# Question 23a
# Answer: At low moose densities (0–1), browsing scores are widely spread, with moose targeting species like Willow and Alder while ignoring others like Black Spruce.
# As density increases toward ~2.5, most species converge toward high browsing scores (4–5), indicating moose become less selective and browse what is available.

# Question 23b
# Answer: Moose favor Willow and Alder the most, while they browse Black Spruce the least. Willow and Alder stay highly browsed even at lower densities, while Black Spruce remains low.

# Question 23c
# Answer: Black Ash is not shown because there are no plotted data points for it (likely very few/no observations in the plotted subset), even though it appears in the legend.

# Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests",
                 "Northern_Peninsula_Forests",
                 "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens",
                 "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")

moose_coll <- data.frame(Collisions_2020 = collisions2020,
                         HumanPopulation = human_pop,
                         StudySites = study_sites)

# Question 25a
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = StudySites)

# Question 25b
coll_merge <- left_join(moose_coll2, moose_2020b, by = "Ecoregion")

# Question 26a
plot(coll_merge$MooseDensity, coll_merge$Collisions_2020,
     xlab = "Moose density (moose / km^2)",
     ylab = "Collisions in 2020",
     main = "Moose density vs collisions (2020)")

# Question 26b
# Answer: There is a clear outlier with very high collisions (likely Avalon_Forests). Collisions are influenced not only by moose density but also by human population/traffic.

# Question 27
coll_merge_per_capita <- coll_merge %>%
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)

# Question 28
plot(coll_merge_per_capita$HumanPopulation, coll_merge_per_capita$CollisionsPerCapita,
     xlab = "Human population",
     ylab = "Collisions per capita (2020)",
     main = "Collisions per capita vs human population")

# Question 29
# Answer: There is a negative trend: higher-population regions tend to have fewer collisions per person, while smaller/rural regions can have higher per-capita collisions due to more moose exposure.
