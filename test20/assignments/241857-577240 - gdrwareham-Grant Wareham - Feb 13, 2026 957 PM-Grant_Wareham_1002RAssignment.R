install.packages("dplyr")
#loading dplyr 
library(dplyr)
#import the data set
moosedata <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv")
#remove rows with missing values 
moose_clean <- na.omit(moosedata)
#simplify data set 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#find the earliest year in the data
min(moose_sel$Year)
#find the maximum moose population recorded
max(moose_sel$Estimated_Moose_Pop)
#calculate moose density 
moosedata2 <- mutate(moose_sel, MooseDensity= Estimated_Moose_Pop / Area)
#create a line graph of moose density over time 
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "Year", ylab = "Moose per Square km", main = "Moose Density in Newfoundland Ecoregions Over time", type = "l")
#filter data for the Western Forests ecoregion
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#plot how moose density has changed over time in western forests 
plot(moose_west$Year, moose_west$MooseDensity, xlab = "Year", ylab = "Moose density", main = "Moose Density over Time in Western Forests", type = "l")
moose_2020 <- filter(moosedata2, Year== "2020")
#filter areas with high moose density in 2020
moose_2020_high <- filter(moose_2020, MooseDensity>2.0)
#moose density in descending order 
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#use pipes to produce the final filtered dataset 
moosefinal <- moosedata2%>%
  filter(Year==2020)%>%
  filter(MooseDensity>2.0)%>%
  arrange(desc(MooseDensity))%>%
  print()
#part 2: Sapling study analysis
samplings <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
#remove missing values from sapling dataset
sap_clean <- na.omit(samplings)
#vary of moose browsing pressure 
sap_reg_browse <- sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
#find the regions with highest and lowest browsing scores 
avg_browse_reg <- arrange(sap_reg_browse, desc(BrowsingScore))
#lowest browsing occurred in Straight Of Belle Isle Barrens
#highest browsing occurred in Northern Peninsula Forests
#tree height varies across different ecoregions 
sap_reg_height <- sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean(Height))%>%
  print()
# avg height is less than 20, Western_Forests, Northern_Peninsula_Forests
# browsing score across different tree species 
sap_spe_browse <- sap_clean%>% 
  group_by(Species)%>%
  summarise(mean(BrowsingScore))%>%
  print()
avg_browse_spe <- arrange(sap_spe_browse, desc("BrowsingScore"))
#highest score Black_Ash, Black_Spruce the lowest
fir_reg_browse <- sap_clean%>%
  filter(Species== "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
#create bar plot for Balsam Fir browsing intensity 
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Browsing Intensity", main = "Browsing Intensity in Ecoregions", col = "green", cex.names = 0.6)
#analyze browsing intesity for Black Spruce 
spruce_reg_browse <- sap_clean%>%
  filter(Species== "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
#create bar plot for Black spruce browsing
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Score", main = "Browsing of Black Spruce", col = "blue", cex.names = 0.4)
#Both species show similar patterns across ecoregions, with higher and lower browsing in the same regions. The seperator is that Black spruce is primarily found the Western Forests 
# count the number of trees sampled in each ecoregion
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()
#count how many 
sap_spe_tally <- sap_clean%>%
  group_by(Species)%>%
  tally()%>%
  print()
# Balsam Fir is over-represented compared to Black Ash, and North Shore Forests have more samples than Strait of Belle Isle Barrens
# Recognizing bias is important because uneven sampling can affect the conclusions about browsing scores across ecoregions.Thererfore, the sapling study is not evenly distributed.
# part 3 
filter(moose_clean, Year=="2020")
moose_2020b <- mutate(moose_clean, MooseDensity= Estimated_Moose_Pop/Area)
moose_sap <- left_join(moose_2020b, sap_clean, by="Ecoregion", relationship = "many-to-many")
sum_spe_browse <- moose_sap%>%
  group_by(Species,Ecoregion)%>%
  summarise(mean(MooseDensity),mean(BrowsingScore))%>%
  print()
# Question 23
# a) There is much evidence supporting this hypothesis, because areas with higher moose density have higher average browsing across species.
# b) Moose prefer Willow the most due to its high browsing score, while Black Spruce is the least.
# c) Black Ash is not shown because it was only sampled one time in the study.
# analyze moose vehicle collisions
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# rename column to match the Ecoregion
moose_coll2 <- moose_coll%>%
  rename_with(~"Ecoregion", study_sites)%>%
  print()
#merge collision data with moose density data 
coll_merge <- left_join(moose_coll2, moose_2020, by="Ecoregion", relationship="many-to-many")
#create scatter plot of moose density vs collisions
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Collisions", main = "Vehicle Collisions Related to Moose Density")
# As moose density increases, collisions also rise largely. One noticeable difference occurs around density 1 with 110 collisions.
coll_per_capita <- (collisions2020/human_pop)
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita= collisions2020/human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions", main = "Collisions Compared to Human Population")
# The pattern I've seen is that collisions are higher in areas with a smaller human population, likely because moose are more common around less populated areas.

