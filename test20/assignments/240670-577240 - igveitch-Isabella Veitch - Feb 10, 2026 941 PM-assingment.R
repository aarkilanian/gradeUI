#Title: My R script for lab 1
#Author: Isabella Veitch
#Date: 08/02/2026

# Load libraries needed
library(dplyr)

#Set Working directory
getwd()
#working directory was set originally

# Load Data
moosedata <- read.csv("MoosePopulation.csv")
View(moosedata)
saplingstudy <- read.csv("SaplingStudy.csv")
View(saplingstudy)

#Analyze Data
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_sel$Year)
year_min<-min(moose_sel$Year)
max(moose_sel$Estimated_Moose_Pop)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_2020 <- moosedata2 %>% filter(Year==2020)
moose_2020_high <- moosedata2 %>% filter(MooseDensity>=2.0)
moose_2020_high_byD<-arrange(moosedata2, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
sap_clean <- na.omit(saplingstudy)
View(saplingstudy)
sap_reg_browse <-sap_clean %>% 
  group_by(Ecoregion)%>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_reg_browse)
avg_browse_reg<- sap_reg_browse %>% 
  arrange(desc(AverageBrowsing))
# Highest Average Browsing = Northern_Peninsula_Forests
# Lowest Average Browsing = StraitOfBelleIsleBarrens
sap_reg_height<-sap_clean %>% 
  group_by(Ecoregion)%>% 
  summarize(mean = mean(Height))
print(sap_reg_height)
sap_reg_height_low <- filter(sap_reg_height, mean <20)
print(sap_reg_height_low)
#The following ecoregions have average heights less than 20cm:Northern_Peninsula_Forests, Western_Forests
sap_spe_browse <-sap_clean %>% 
  group_by(Species)%>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
print(sap_spe_browse)
avg_browse_spe<- sap_spe_browse %>% 
  arrange(desc(AverageBrowsing))
#Highest Browsing Score =Black_Ash
#Lowest Browsing Score =Black_Spruce
fir_reg_browse <-sap_clean %>% 
  filter(Species=="Balsam_Fir")%>%
  group_by(Ecoregion)%>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
View(fir_reg_browse)
spruce_reg_browse <-sap_clean %>% 
  filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>% 
  summarize(AverageBrowsing = mean(BrowsingScore))
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#I do not think The SaplingStudy dataset is evenly distributed. Most ecoregions have similar sample sizes of around 5, but Maritime_Barrens(n = 3) and especially StraitOfBelleIsleBarrens(n = 1) are underrepresented. Balsam_Fir is overrepresented among species(n = 11), while Black_Ash is strongly underrepresented(n = 1).
#Recognizing sampling bias is important because uneven representation can skew estimates of moose browsing intensity and lead to incorrect conclusions about patterns across ecoregions or species.
moose_2020b <- moose_clean %>% filter(Year==2020)%>% mutate(MooseDensity = Estimated_Moose_Pop / Area)%>%print()
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>% 
  group_by(Species, Ecoregion) %>%
  summarize( mean_BrowsingScore = mean(BrowsingScore), mean_MooseDensity = mean(MooseDensity))
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll%>% 
  rename("Ecoregion"= "study_sites")
#rename_with didn't work^
coll_merge<-moose_coll2%>% 
left_join(moose_coll2, moose_2020b, by = 'Ecoregion', relationship= "many-to-many")
coll_merge_per_capita <-mutate(coll_merge, coll_per_capita = collisions2020 /human_pop)

#Plot Data
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time", 
     type="l")

plot(moosedata2$Year, moosedata2$MooseDensity,(type = "l"),
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland Western_Forest ecoregions over time")
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Browsing Intensity of Balsam Fir in different Ecoregions", col = "pink",cex.names=0.6)
#pick any colour you want cex.names = 0.6) # Reduces x-axis label size for readability
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Intensity", main = "Average Browsing Intensity of Black Spruce in different Ecoregions", col = "purple",cex.names=0.6)
# When comparing the two we can see that Balsam Fir has a greater browsing intensity in the ecoregions "Avalon_Forests","Central_Forests", "EasternHyperOceanicBarens", "Maritime_Barrens", "North_Shore_Forests". Black Spruce has a greater browsing intensity in "Long_Range_Barrens" and they have an equal intensity in "Northern_Peninsula_Forests". Black Spruce also has one more sapling "Western_Forests" which Balsam Fir does not have.
#The following refer to the figure in question 23:
#a)Yes, the figure shows evidence supporting the hypothesis. At lower moose densities, browsing intensity is higher on preferred species, while at higher moose densities, browsing scores increase across multiple species, suggesting more generalized browsing.
#b)Moose appear to favour Willow and Alder, which consistently show higher average browsing scores across densities. Black Spruce and Black Ash are browsed the least, especially at lower moose densities.
#c)Black Ash is missing most likely because it is strongly underrepresented in the dataset, with very few observations, making it unsuitable for meaningful comparison in the figure.
plot(moose_2020b$MooseDensity, coll_merge$collisions2020.y,
     xlab = "Moose Density",
     ylab= "# of collisions")
#As the # of collisions increases, the Moose Density increases aswell
#There is an outlier at a density of 1.0, as it does not follow the trend that the rest of the data supports
plot(coll_merge_per_capita$human_pop.x, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "# of collisions per Capita",
     main = "Moose Collisions per Capita vs Human Population", 
     xlim = c(0,80000))
#boundary added for better view of data
#This graph shows higher collisions per capita for less populated areas, and lower collisions per capita for greater populated areas
# This trend makes sense considering that these less populated areas (rural) have high moose densities and less people, increasing the likelihood for collisions to occur
