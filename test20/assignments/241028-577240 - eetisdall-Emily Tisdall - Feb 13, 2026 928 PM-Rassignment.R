# R assignment 3
# BIOL 1002
# Emily Tisdall

##
### PART ONE: Moose Population
##

## ## Q1 :
library(dplyr)

## ## Q2 :
moosedata <- read.csv("MoosePopulation.csv")

## ## Q3  :
moose_clean <- na.omit(moosedata)

## ## Q4 :
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

## ## Q5 :

  # a)
year_min <- min(moose_sel$Year)

  # b)
moose_max <- max(moose_sel$Estimated_Moose_Pop)

## ## Q6 :
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

## ## Q7 :
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose/km^2", 
     main = "Newfoundland ecoregion moose density, 1900-2020")

## ## Q8 :

  # a)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

  # b)
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "Year", 
     ylab = "Moose/km^2", 
     main = "Newfoundland moose density in western forests, 1900-2020")

## ## Q9 :

  # a)
moose_2020 <- filter(moosedata2, Year == "2020")

  # b)
moose_2020_high <- filter(moose_2020, MooseDensity >= 2.0)

  # c)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

## ## Q10 :

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

##
### PART TWO: Sapling Study
##

## ## Q11 :

  # a)
saplings <- read.csv("SaplingStudy.csv")

  # b)
sap_clean <- na.omit(saplings)

## ## Q12 :

  # a)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

  # b)
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(sap_reg_browse$'mean(BrowsingScore)')) %>%
  print()
### Highest browsing region: Northern_Peninsula_Forests
## Lowest browsing region: StraitOfBelleIsleBarrens

## ## Q13 :

  # a)
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(Height)) %>%
  print()

  # b)
sap_reg_height_low <- sap_reg_height %>%
  filter(sap_reg_height$'mean(Height)' <= 20) %>%
  print()
## Northern_Peninsula_Forests, Western_Forests

## ## Q14 : 

  # a)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

  # b)
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(sap_spe_browse$'mean(BrowsingScore)')) %>%
  print()
## Highest score: Black Ash
## Lowest score: Black Spruce

## ## Q15 :

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

## ## Q16 :

barplot(fir_reg_browse$'mean(BrowsingScore)', names.arg = fir_reg_browse$Ecoregion,
        xlab = "Average moose browsing score",
        ylab = "Region",
        main = "Moose average browsing of Balsam fir",
        col = "forestgreen",
        cex.names = 0.6)

## ## Q17 :

  # a)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

  # b)
barplot(spruce_reg_browse$'mean(BrowsingScore)', 
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Average moose browsing score",
        ylab = "Region",
        main = "Moose average browsing of black spruce saplings",
        col = "purple",
        cex.names = 0.6)

  # c)
## In balsam fir saplings, the average time that moose would browse stayed 
## relatively around 2 in each region, with some slight variation. 
## In black spruce, the average time spent browsing was much more 
## varied depending on region.

## ## Q18 :

sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

## ## Q19 :

sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

## ## Q20 :

# a)

## In the dataset, some species are over-represented and
## under-represented. For example, the Balsam fir has
## 11 species counted, while the black ash has only
## one species represented in the data.

# b)

## Acknowledging bias in ecological datasets is incredibly important to 
## determine the validity of the data given. Biased datasets may not
## be considered credible, due to swayed data.

##
### PART THREE: Joining databases
##

## ## Q21 :


  # a)
moose_2020b <- moose_clean %>%
  filter(Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()

  # b)
moose_sap <- left_join(moose_2020b, sap_clean,
                       by = 'Ecoregion', 
                       relationship = "many-to-many")

## ## Q22 :

sum_spe_browse <- moose_sap %>%
  group_by(Species) %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  summarize(mean(moose_sap$MooseDensity)) %>%
  print()

## ## Q23 :

  # a)
## Yes, the data shown supports their hypothesis. 

  # b)
## At lower moose densities, the moose tended to gravitate towards
## Alder and willow saplings. They tended to have an aversion to
## black spruce.

  # c)
## Black ash is not shown on the figure, because only one member of
## The species was recorded in the data. Thus, the species was
## Removed to avoid major biases.

## ## Q24 :

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

## ## Q25 :

  # a)
moose_coll2 <- rename(moose_coll, "Ecoregion" = study_sites)

  # b)
coll_merge <- left_join(moose_coll2, moose_2020,
                       by = 'Ecoregion', 
                       relationship = "many-to-many")

## Q26 :

  # a)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose density (per km/^2)", 
     ylab = "Collisions", 
     main = "Collisions in relation to moose population density")

  # b)
## Generally, as the moose density increases, so too does the number
## of collisions. However, one outlier exists, where there exist 100
## collisions at a moose density of 1.0.

## ## Q27 :
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

## ## Q28 :
plot(coll_merge_per_capita$coll_per_capita, coll_merge$human_pop,
     xlab = "Collisions per capita", 
     ylab = "Population", 
     main = "Collisions per capita in relation to population")

## ## Q9 :
## In the data shown, more collisions are found near lower levels of human
## population. This makes sense, considering that the highest areas of moose density
## Are in places that have a relatively low human population