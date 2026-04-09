# Title: Biol-1002 R Assignment
# Author: Jirah Hiscock
# Date: 07-01-2026

# Set Working Directory -------------------------------------------------------

setwd("/Users/jirah/OneDrive/Documents/Biol-1002-R")



#1 Install Packages ------------------------------------------------------------

install.packages("dplyr")

library("dplyr")



#2 Load Data -------------------------------------------------------------------

moosedata <- read.csv("MoosePopulation.csv")



#3 View Data ------------------------------------------------------------------

View(moosedata)

moose_clean <- na.omit(moosedata)



#4 Columns of Interest ---------------------------------------------------------

moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)



#5 Data Observations

a.
min(moose_clean$Year)
year_min <- min(moose_clean$Year)
# 1904

b.
max(moose_clean$Estimated_Moose_Pop)
moose_max <- max(moose_clean$Estimated_Moose_Pop)
# 41250



#6 New Coloumn

moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)



#7 Making a Plot

plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time
     Jirah Hiscock")



#8 Western Forests Dataset 

a.
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

b.
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Western Forests regions in Newfoundland over time
     Jirah Hiscock")


#9 Trends in Datasets

a.
moose_2020 <- filter(moosedata2, Year == "2020")

b.
moose_2020_high <- filter(moosedata2, MooseDensity>2.0)

c.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))



#10 Connecting lines of code

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


#Part 2: Tree Sapling Study
  
#11 Load Data

a.
saplings <- read.csv("SaplingStudy.csv")

View(saplings)

b. 
sap_clean <- na.omit(saplings)



#12 Moose Browsing pressure in different ecoregions

a.
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingAverage=mean(BrowsingScore)) %>%
  print()

sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  reframe(BrowsingScore=mean(BrowsingScore)) %>%
  print()

b.
avg_browse_reg <- arrange(sap_reg_browse, desc(BrowsingAverage))

#Highest = Northern_Peninsula_Forests
#Lowest = StraitOfBelleIsleBarrens


#13 Tree height across ecoregions

a.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(TreeHeightAverage=mean(Height)) %>%
  print()

b.
sap_reg_height_low <- filter(sap_reg_height, TreeHeightAverage < 20)

#Western_Forests and Northern_Peninsula_Forests have average heights less than 20 cm



#14 Browsing Score and Tree sapling species

a.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(BrowsingAverage=mean(BrowsingScore)) %>%
  print()

b.
avg_browse_spe <- arrange(sap_spe_browse, desc(BrowsingAverage))

#Highest = Black_Ash
#Lowest = Black_Spruce



#15 Balsam Fir Browsing

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion) %>%
  summarize(BrowsingAverage=mean(BrowsingScore)) %>%
  print()



#16 Balsam Fir Bar Graph

barplot(fir_reg_browse$BrowsingAverage, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Browsing Average", main = "Average Browsing Intensity for Balsam Fir in different Ecoregions", col = "forestgreen", cex.names = 0.6)



#17 Black Spruce Browsing Intensity


a.
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion) %>%
  summarize(BrowsingAverage=mean(BrowsingScore)) %>%
  print()


b.
barplot(spruce_reg_browse$BrowsingAverage, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Browsing Average", main = "Average Browsing Intensity for Black Spruce in different Ecoregions", col = "forestgreen", cex.names = 0.6)


c.
#The Balsam Fir browsing is much more consistent between ecoregions, and it is also consistently higher than the black spruce. The two highest average browsing scores are from the same two ecoregions in both species.



#18 Ecoregion Counted Saplings

sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()



#19 Species Counted Saplings

sap_reg_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()


#20 Dataset reflection

a.
#When looking at the different ecoregions, StraitOfBelleIsleBarrens seems to be the most underepresented, and the black ash tree species is the most underepresented tree species. 

b.
#It is important to recognize bias as it can have a major impact on the resulting data. If one particular aspect of the data is overepresented, it could shift the entire results or information pulled from the data. Having a bias in ecological datasets means that the findings may not be generalizable to other ecological regions.  


#Part 3 Creating and Joining Datasets


#21 Moose Clean select


a.
moose_2020b <- moose_clean%>%
  filter(Year == "2020")%>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)%>%
  print()


b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")



#22 Average Browsing Score and Moose Density

sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(BrowsingAverage=mean(BrowsingScore), MooseDensityAverage = mean(MooseDensity)) %>%
  print()



#23 Data analysis 

a.
#They show relatively the same browsing sapling species preference at the low and high average moose density. This does not support the researchers' hypothesis as they still favour the same sapling species at low and high moose density. However, with a greater moose density, there is a higher browsing average, and the least preferred species are browsed more in the high density areas. 

b.
#Moose favour the willow saplings the most, and they browse the black ash (not on graph) and black spruce the least.

c.
#The black ash sapling is not shown on the graph as there is only one recorded data point of that species. This makes it highly underepresented, and placing it on the graph may show an incorrect average as there was only one sapling measured.

#24 Saving Vectors

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)



#25 Join dataset

a.
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)

b.
coll_merge <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")



#26 Moose Density and Moose-vehicle collisions

a.
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     xlab = "Moose Density", 
     ylab = "Collisions 2020", 
     main = "Moose density and moose-vehicle collisions in Newfoundland")

b.
#There is a gradual upwards trend depicted in the graph. As moose density increases, the number of collisions also increases. However, there is one major high outlier shown when moose density is 1.0.



#27 Ecoregions with highest moose collisions per person

coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020/human_pop)



#28 Scatter plot of collisions per capita

plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population", 
     ylab = "Collisions per Capita", 
     main = "Moose Collisions Per Person in Newfoundland")

#29 Explanation

#The trend displays that moose collisions per capita decrease as the human population increases. This makes sense as we are looking at just the increase in human population compared to the same population of moose. As the human population increases and the moose population remains the same, it decreases the collisions per capita as there will likely be close to the same number of collisions with a greater population.