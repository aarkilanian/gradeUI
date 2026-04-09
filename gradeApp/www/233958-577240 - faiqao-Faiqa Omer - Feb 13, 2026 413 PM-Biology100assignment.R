###########################
#PART 1: MOOSE POPULATION
###########################

#Q1
library(dplyr)

#Q2
moosedata <- read.csv("~/Downloads/BIO R ASSIGNMENT/MoosePopulation.csv")

#Q3 
View(moosedata)
na.omit(moosedata)
moose_clean <- na.omit(moosedata)

#Q4
select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5a 
year_min <- min(moose_sel$Year)
#Ans: 1904

#Q5b
moose_max <- max(moose_sel$Estimated_Moose_Pop)
#Ans: 41250

#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Q7
plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in NL ecoregions over time")

#Q8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#Q8b
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forests over time")

#Q9a
moose_2020 <- filter(moosedata2, Year == 2020)

#Q9b
moose_2020_high <- filter (moose_2020, MooseDensity > 2.0)

#Q9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Q10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#########################
#PART 2: SAPLING STUDY
#########################

#Q11a
saplings <- read.csv("~/Downloads/BIO R ASSIGNMENT/SaplingStudy.csv")

#Q11b
sap_clean <- na.omit(saplings)

#Q12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Averagebrowsing = mean(BrowsingScore)) %>%
  print()

#Q12b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(Averagebrowsing)) %>%
  print()
#Highest browsing: Nothern Peninsula Forests
#Lowest browsing: Straight Of Belle Isle Barrens

#Q13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageHeight = mean(Height)) %>%
  print()

#Q13b
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()
#Ecoregions with avg height < 20cm: 
#Northern Peninsula Forests(19.9) and Western Forests(18.9)

#Q14a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#Q14B
avg_browse_spec <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Highest browsing species: Black Ash 
#Lowest browsing species: Black Spruce 

#Q15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#Q16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Balsam Fir browsing intensity by Ecoregion",
        col = "lightblue",
        cex.names = 0.6)

#Q17a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#Q17b
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score",
        main = "Black Spruce browsing intensity by Ecoregion",
        col = "lavender",
        cex.names = 0.6)
#Q17c
#Balsam Fir generally shows higher browsing intensity than Black Spruce across
#most ecoregions. Black Spruce browsing is more moderate and less variable.

#Q18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()
#The no. of saplings counted was not the same across ecoregions.

#Q19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#Q20a
#The SaplingStudy dataset isn't evenly distributed across ecoregions or species.
#(Ex. Northern_Peninsula_Forests has more samples than StraitofBelleIsleBarrens,
#which is underrepresented).

#Q20b
#Recognizing sampling bias is important because uneven sampling can lead to
#misleading conclusions about browsing patterns. If some regions or species
#are underrepresented, the results may not reflect the true system.

######################################
#PART 3: CREATING & JOINING DATASETS
######################################

#Q21a
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

#Q21b
moose_sap <- left_join(moose_2020b, sap_clean,
                       by = "Ecoregion",
                       relationship = "many-to-many")

#Q22
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  ) %>%
  print()
  
#Q23a 
#There is some evidence supporting the hypothesis. At higher moose density,
#browsing scores across species appear more similiar, suggesting less selective
#browsing.

#Q23b
#Moose appear to favour species like Black Ash and Willow, which show higher
#browsing scores. Black spruce tends to have the lowest browsing intensity.

#Q23c
#No sapling species are missing from the figure. All species in the dataset 
#are represented on the plot.

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", 
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25a
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)

#Q25b
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")

#Q26a
plot(coll_merge$MooseDensity,
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Moose Density vs. Vehicle Collisions")

#Q26b
#There appears to be a positive relationship between moose density & collisions
#, with collisions increasing as density increases. One region appears to have 
#unusually high no. of collisions compared to other regions. (~110 collisions)

#Q27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)
#Nothern_Peninsula_Forests has the highest no. of collisions per person.

#Q28
plot(coll_merge_per_capita$human_pop,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Collisions per Capita vs. Human Population")

#Q29
#Collisions per capita tend to decrease as human population increases.
#This makes senese because highly populated areas usually have fewer moose
#habitats, while rural regions have more (therefore higher moose population).


