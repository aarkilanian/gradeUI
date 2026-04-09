install.packages("dplyr")
#installing dplyr
library(dplyr)
#importing data
moosedata <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv")
#removings NAs
moose_clean <- na.omit(moosedata)   
#simplifying
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#using min to find oldest year
min(moose_sel$Year)
#biggest moose pop
max(moose_sel$Estimated_Moose_Pop)
#moose density for each eco region
moosedata2 <- mutate(moose_sel, MooseDensity=Estimated_Moose_Pop/Area)
#plotting line graph
plot(moosedata2$Year, moosedata2$MooseDensity, ylab = "moose per square kilometre",xlab = "year",main ="moose density in newfoundland eco regions overtime",type = "l")
#moose populations changed in western forests 
moose_west <- filter(moosedata2, Ecoregion=="Western_Forests")
#moose density change in western forest
plot(moose_west$Year,moose_west$MooseDensity, xlab = "year", ylab = "moose density", main = "moose density change in western forests", type = "l")
#filter for 2020
moosedata_2020 <- filter(moosedata2, Year=="2020")
#high moose density in 2020
moosedata_2020_high <- filter(moosedata_2020, MooseDensity>2.0)
#decending 
moosedata_2020_high_byD <- arrange(moosedata_2020_high, desc(MooseDensity))
#using pipes
moosefinal <- moosedata2%>%
  filter(Year==2020)%>%
  filter(MooseDensity>2.0)%>%
  arrange(desc(MooseDensity))%>%
  print()
#Tree Sampling Study
samplings <- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
#remove NAS
sap_clean <- na.omit(samplings)
sap_reg_browse <- sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
avg_browse_reg <- arrange(sap_reg_browse,desc(BrowsingScore))
#lowest is straight of belle is lebarrens
#highest is northern peninsula forests
#tree height
sap_reg_height <- sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean(Height))%>%
  print()
# Average height less than 20: norhtern peninsula forests, western forests
sap_spe_browse <- sap_clean%>%
  group_by(Species)%>%
  summarise(mean(BrowsingScore))%>%
  print()
avg_browse_spe <- arrange(sap_spe_browse,desc("BrowsingScore"))
# Highest black ash and lowest black spruce
fir_reg_browse <- sap_clean%>%
  filter(Species=="Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
barplot(fir_reg_browse$`mean(BrowsingScore)`,names.arg = fir_reg_browse$Ecoregion,xlab = "eco regions", ylab = "browsing", col = "blue", cex.names = 0.6)
#intensity
spruce_reg_browse <- sap_clean%>%
  filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
barplot(spruce_reg_browse$`mean(BrowsingScore)`,names.arg = spruce_reg_browse$Ecoregion,xlab= "eco regions", ylab = "average browsing",main = "browsing of black spruce",col = "pink",cex.names = 0.4)
# Black spruce is found in western forests but not balsam fir. North shore forests are similar averages in both tree species.
sap_reg_tally <- sap_clean%>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()
sap_spe_tally <- sap_clean%>%
  group_by(Species)%>%
  tally()%>%
  print()
# I believe this wasn't distributed fairly as the balsam fir was looked at 11 times and the black ash was only looked at 1 time.
#It is important to recognize if the browsing score of the moose was really that high or low in an eco region.
#part 3
filter(moose_clean,Year=="2020")
moose_2020b <- mutate(moose_clean, MooseDensity=Estimated_Moose_Pop/Area)
#Join
moose_sap <-left_join(moose_2020b, sap_clean, by="Ecoregion",relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(mean(MooseDensity),mean(BrowsingScore)) %>%
  print()
#question 23A: Yes I agree that thee hypothesis is right because at low moose density the moose are favoring the willow and alder trees. At high moose density the moose are all of the tree species.
#question 23B: The moose favor willow the most and black spruce the least.
#question 23C: Black ash is not shown on the graph because when were were discussing if moose favoured certain species, the species that only had been looked at once was black ash.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename_with(~"Ecoregion", study_sites) %>%
  print()
#Merging
coll_merge <- left_join(moose_coll2, moosedata_2020, by= "Ecoregion", relationship="many-to-many")
#plot
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab="MooseDensity", ylab= "collisions", main = "Vehicle collisions compared to moose density")
#Question 26B: I noticed that the moose density increased in all of the collisions except for when it was at 1.0, it had a big gap in collisions.
coll_per_capita <- (collisions2020/human_pop)
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita= collisions2020/human_pop)
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "human population", ylab = "collisions", main = "collision compared to populations") 
#Question 29: The trend I see is that where there is less people (more moose) they have less collisions.