#loading dplyr 
library(dplyr)
#load data
moosedata <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv")
#getting rid of NA's 
moose_clean <- na.omit(moosedata)
#simplify data set 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#oldest year
min(moose_sel$Year)
#highest moose pop
max(moose_sel$Estimated_Moose_Pop)
#moose density 
moosedata2 <- mutate(moose_sel, MooseDensity= Estimated_Moose_Pop / Area)
#line graph of data 
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "moose per square km", main = "moose density in newfoundland ecoregions over time", type = "l")
#observations from western forest 
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#how moose density has changed in western forests 
plot(moose_west$Year, moose_west$MooseDensity, xlab = "year", ylab = "moose density", main = "moose density over time in western forests", type = "l")
moose_2020 <- filter(moosedata2, Year== "2020")
#high moose density in 2020
moose_2020_high <- filter(moose_2020, MooseDensity>2.0)
#moose density in descending order 
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#pipes for moose final 
moosefinal <- moosedata2%>%
  filter(Year==2020)%>%
  filter(MooseDensity>2.0)%>%
  arrange(desc(MooseDensity))%>%
  print()
#part 2 
samplings <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
#clean data set 
sap_clean <- na.omit(samplings)
#vary of moose browsing pressure 
sap_reg_browse <- sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
#highest and lowest browsing 
avg_browse_reg <- arrange(sap_reg_browse, desc(BrowsingScore))
#lowest was StraightOfBelleIsleBarrens
#highest was Northern_Peninsula_Forests
#tree height vary across ecoregions 
sap_reg_height <- sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean(Height))%>%
  print()
# avg height less than 20, Western_Forests, Northern_Peninsula_Forests
# browsing score across different tree species 
sap_spe_browse <- sap_clean%>% 
  group_by(Species)%>%
  summarise(mean(BrowsingScore))%>%
  print()
avg_browse_spe <- arrange(sap_spe_browse, desc("BrowsingScore"))
#highest score Black_Ash, Lowest score Black_Spruce
fir_reg_browse <- sap_clean%>%
  filter(Species== "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
#bar plot 
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Browsing Intensity", main = "Browsing Intensity in Ecoregions", col = "green", cex.names = 0.6)
#Black Spruce Intensity 
spruce_reg_browse <- sap_clean%>%
  filter(Species== "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
#bar plot 
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Score", main = "Browsing of Black Spruce", col = "blue", cex.names = 0.4)
# the similarities between the two are that they both relate two each other with their higher and lower browsing scores being in the same Eco regions, they main difference being that the Black Spruce in found in westerns forests 
#how many trees were counted 
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()
#counted species 
sap_spe_tally <- sap_clean%>%
  group_by(Species)%>%
  tally()%>%
  print()
# I do not think that the sapling study was evenly distributed, Balsam_Fir was very over represented in comparison to Black_Ash, and the Eco region North Shore Forests is over represented compared to Strait of Belle Isle Barrens 
# It is important to recognize bias because you want the information that you are working with to be reliable, and with this kind of study it might not give true representation of the relationship between the average Browsing score in a Eco Region
# part 3 
filter(moose_clean, Year=="2020")
moose_2020b <- mutate(moose_clean, MooseDensity= Estimated_Moose_Pop/Area)
moose_sap <- left_join(moose_2020b, sap_clean, by="Ecoregion", relationship = "many-to-many")
sum_spe_browse <- moose_sap%>%
  group_by(Species,Ecoregion)%>%
  summarise(mean(MooseDensity),mean(BrowsingScore))%>%
  print()
# question 23
# a) yes i believe there is evidence that supports the hypothesis, we are able to see in the areas with higher density the the average browsing is higher for all of the species
# b) moose favor willow the most we can see this because it has the highest browsing score no matter what the density is, black spruce is the least liked as it is consistently the lowest.
# c) the sapling species not shown on the figure is Black ash because in the studies that were done they only looked at black ash once 
# moose collisions
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# renaming 
moose_coll2 <- moose_coll%>%
  rename_with(~"Ecoregion", study_sites)%>%
  print()
#merging the 2
coll_merge <- left_join(moose_coll2, moose_2020, by="Ecoregion", relationship="many-to-many")
#scatter plot
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Collisions", main = "Vehicle Collisions Related to Moose Density")
# for the most part as the moose density increases the number of collisions also increases, the only thing out of place is there is a big jump when the density is around 1 in collisions being 110.
coll_per_capita <- (collisions2020/human_pop)
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita= collisions2020/human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions", main = "Collisions Compared to the Population")
# the pattern i see is that there are more collisions where there is a lower population, this would make sense because the moose are more likely to live where there are less people, making the higher populated areas less likely to get into a collision.