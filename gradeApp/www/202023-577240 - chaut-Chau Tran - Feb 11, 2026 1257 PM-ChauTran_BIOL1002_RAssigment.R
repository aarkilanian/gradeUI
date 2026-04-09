# Title: My R script for R assignment, BIOL 1002
# Author: Chau Tran
# Date: 02-02-2026

#PART I: Moose Populations in Newfoundland

#Question 1
# Alternatively, install just dplyr:
install.packages("dplyr") 

# Use the library()function to load dplyr:
library("dplyr")

#Question 2
moosedata <- read.csv("~/Documents/BIOL1002_Rassignment/MoosePopulation.csv")
View(moosedata)

#Question 3

moose_clean <- na.omit(moosedata)

#Question 4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5 (#not sure)
#a. 
Year <- select(moose_sel, Year)
View(Year)
year_min <- min(Year, na.rm = FALSE)

#b. 
EstimatedMoosePop <- select(moose_sel, Estimated_Moose_Pop)
View(EstimatedMoosePop)
moose_max <- max(EstimatedMoosePop, na.rm = FALSE)

#Question 6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8
#a.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#b.
plot(type = "l", moose_west$Year, moose_west$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Western Forest region over time")

#Question 9
#a.
library(dplyr)
moose_2020 <- filter(moosedata2,Year == "2020")

#b.
library(dplyr)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

#c.
moose_2020_high_byD <-arrange(moose_2020_high, desc(MooseDensity))

#Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#PART II:Tree Sapling Study

#Question 11
#a.
saplings <- read.csv("~/Documents/BIOL1002_Rassignment/SaplingStudy.csv")
View(saplings)

#b.
sap_clean <- na.omit(saplings)
View(sap_clean)

#Question 12
#a.
library(dplyr)
sap_reg_browse <- group_by(sap_clean, Ecoregion)%>%
  summarize(AverageBrowsing = mean(BrowsingScore))%>%
  print()

#b. 
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing)) %>%
  print()

#Region had highest average browsing scores is Northern Peninsula Forest
#Region had lowest average browsing scores is Strait Of Belle Isle Barrens

#Question 13 
#a.
sap_reg_height <- group_by (sap_clean, Ecoregion)%>%
  summarize(Height=mean(Height))%>%
  print()

#b.
sap_reg_height_low <- sap_reg_height %>%
  filter(Height < 20) %>%
  arrange(desc(Height)) %>%
  print()

# Ecoregion have average heights less than 20 cm are North Penisula Forests 
#and Western_Forests

#Question 14
#a.
sap_spe_browse <- group_by(sap_clean, Species) %>%
  summarize(BrowsingScore = mean(BrowsingScore)) %>%
  print()
#b.
avg_brow_spe <- arrange(sap_spe_browse, desc(BrowsingScore)) %>%
  print()

#Species has the highest browsing score is Black_Ash (BrowsingScore = 5)
#Species has the lowest browsing score is Black_Spruce (BrowsingScore = 2.33)

#Question 15
fir_reg_browse <- sap_clean %>%
  filter(Species =="Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

#Question 16
#using data from fir_reg_browse; pick any color, reduce x-axis label size for readability

barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", 
        ylab = "Browsing Score", main = "Average Balsam Fir Browsing by Ecoregion", 
        col = "lavender", cex.names = 0.6)

#Question 17
#a.
spruce_reg_browse <- sap_clean %>%
  filter(Species =="Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore = mean(BrowsingScore, na.rm = TRUE)) %>%
  print()

#b.
barplot(spruce_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",ylab = "Browsing Score",
        main = "Average Black Spruce Browsing by Ecoregion", col = "turquoise",
        cex.names = 0.6)

#c.
#Black Spure browing is smaller compare to Balsam Fir browsing across ecoregions. 

#Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Question 19
sap_sep_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Question 20
#a.
#The Sapling Study dataset is not evenly distributed.
#Overrepresneted ecoregion such as North_Shore_Forests,Northern_Peninsula_Forests.
#Overrepresneted tree species such as Balsam_Fir,Black_Spruce.
#StraitOfBelleIsleBarrens, Maritime_Barrens are underrepresented ecoregion.
#Black_Ash are underepresented tree species.

#b.
# It is important to recognize bias in ecological datasets because it ensure the
#validity anf accurary of the data.

#PART III: Creating and Joining Datasets.

#Question 21
#a.
library(dplyr)
moose_2020b <- moose_clean %>%
  filter(Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

#b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', 
                       relationship = "many-to-many")
#Question 22

sum_spe_browse <- group_by(moose_sap, Ecoregion, Species) %>%
  summarize(AverageBrowsingScore = mean(BrowsingScore), 
            AverageMooseDensity = mean(MooseDensity)) %>%
  print()

#Question 23

#a.
# Yes, the evidence does supports the researchers's hypothesis. Based on the figure,
#moose do show strong preferences at low desity 
#and shift to more generalist browsing at high density

#b. 
#Sapling species that moose favour most are Willow, Alder
#Moose browse the least are White_Birch, Balsam_Fir, since moose perfer Willow, 
#Alder over White Birch and Balsam Fir.

#c. 
#The sapling species is not on the figure is BLack_Ash, 
#Because on the figure, there is no point that show this species,
#and maybe moose do not perfer this species.

#Question 24

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", 
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25
#a.
library(dplyr)
moose_coll2 <- moose_coll%>%
  rename_with(~"Ecoregion", study_sites)
View(moose_coll2)

#b. 
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion',
                        relationship = "many-to-many")

#Question 26
#a.
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     main = "Moose-vehicle collsions in 2020", xlab = "Moose Density",
     ylab = "Number of Collisions in 2020", pch = 19, col = "navy")

#The rate of collision increases as increased moose density

#b.
# The trend that I see is positive linear trend; increased in moose density ->
#increase collisions2020. There is outlier, the highest and lowest points 
#represnet regions where collisions happened unexpectedly high or low.

#Question 27

coll_merge_per_capita <- mutate(coll_merge,coll_per_capita = collisions2020/human_pop)


#Question 28

plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop,
     main = "Moose Collision per Capita vs. Human Population", 
     xlab = "Collisions per Capita", ylab = "Human Populations",pch = 19,
     col = "forestgreen")

#Question 29

#The trend that I see is negative linear trend. Inversed relatiionship between
#collision per capita and human popultions, increased human_pop associated with
#decreased collision. This trend does make sense in Newfoundland, since 
#areas like Corner Brook, Grand Falls has low human polulation, so that region 
#will have high coll_per_capital, vice versa to St. John's, Mount Pearl.
