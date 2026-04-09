# Title: BIOL1002 R Assignment 1
# Author: Grace Abbott
# Date: 12-02-2026
install.packages("dplyr")
# Question 1 Load dplyr
library(dplyr)
# Question 2 Download dataset moosedata
library(readr)
moosedata <- read_csv("BIOL1002_Coding/MoosePopulation.csv")
# Question 3 View moosedata and remove missing data
View(moosedata)
moose_clean<-na.omit(moosedata)
# Question 4 Simplify dataset
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
# Question 5 a. Oldest observation year 
year_min<-min(moose_sel$Year)
# Question 5 b. Highest Estimated moose pop (max)
moose_max<-max(moose_sel$Estimated_Moose_Pop)
# Question 6 Standardize data (calc moose density for each region ,density=pop/area)
moosedata2<-mutate(moose_sel, MooseDensity= Estimated_Moose_Pop / Area)
# Question 7 plot line graph of data
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
# Question 8 a. western forest
moose_west<- filter(moosedata2, Ecoregion == "Western_Forests")
# Question 8 b. plot line graph
plot(moose_west$Year, moose_west$MooseDensity,
     type= "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland Western Forest regions over time")
# Question 9 a. filter year 2020
moose_2020<- filter(moosedata2, Year == 2020)
# Question 9 b.
moose_2020_high<- filter(moose_2020, MooseDensity > "2.0")
# Question 9 c.
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
# Question 10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
# Question 11 a.
library(readr)
saplings<- read_csv("BIOL1002_Coding/SaplingStudy.csv")
# Question 11 b.
sap_clean<-na.omit(saplings)
# Question 12 a.
sap_reg_browse<-sap_clean %>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
# Question 12 b.
average_browse_reg<-sap_reg_browse%>%
  arrange(desc(`mean(BrowsingScore)`))
  #highest: Northern_Peninsula_Forests, lowest: StraitOfBelleIsleBarrens
# Question 13 a.
sap_reg_height<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(mean(Height))%>%
  print()
# Question 14 b.
sapling_reg_height_low<-sap_reg_height%>%
  filter(`mean(Height)` < 20)%>%
  print()
 #Ecoregions Height < 20 : Northern_Peninsula_Forests (19.9) and Western_Forests (18.9)
# Question 14 a.
sap_spe_browse<-sap_clean%>%
  group_by(Species)%>%
  summarise(mean(BrowsingScore))%>%
  print()
# Question 14 b.
avg_browse_spe<-sap_spe_browse%>%
  arrange(desc(`mean(BrowsingScore)`))%>%
  print()
 #Species Highest browsing: Black_Ash, Lowest: Black_Spruce
# Question 15 
fir_reg_browse<-sap_clean%>%
  filter(Species =='Balsam_Fir')%>%
  group_by(Species =='Balsam_Fir')%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
# Question 16
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$`Ecoregion`,
        xlab = "Ecoregions", ylab = "Average Browsing Intesity)",
        main = "The Average Browsing Intensity for Different Ecoregions in Balsam Fir Trees",
        col = "forestgreen",
        cex.names = 0.6)
# Question 17 a.
spruce_reg_browse<-sap_clean%>%
  filter(Species=='Black_Spruce')%>%
  group_by(Species=='Black_Spruce')%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()
# Question 17 b.
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$`Ecoregion`,
        xlab = "Ecoregions", ylab = "Average Browsing Intesity)",
        main = "The Average Browsing Intensity for Different Ecoregions in Black Spruce Trees",
        col = "forestgreen",
        cex.names = 0.6)
# Question 17 c. The average browsing intensity is more similar and close to each other in
 #Balsam Fir trees in different Ecoregions, while there is a wider range of average browsing
#intesities for Black Spruce trees in different Ecoregions. For example, all of the Balsam Fir 
#trees, except from one region, have a average browsing intesity of 2 or above, while Black Spruce 
#trees average browsing intesities range from 0 to 4.
# Question 18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# Question 19
sap_spe_tally<-sap_clean%>%
  group_by(Species)%>%
  tally()%>%
  print()
# Question 20 a.
#The SaplingStudy is not evenly distributed.
#The ecoregions StraitOfBelleIsleBarrens is underrepresented, with one sapling sample, 
#while the North_Shore_Forests is overrepresented, with eight sampling samples.
# Question 20 b.
#It is important to recognize bias in ecoloigical datasets because it affects the accuracy of the data and averages.
#For example, the browsing average of an underepresented region may be much higheer than a region that is over represented, 
#but one average may be based on a single sample while the other is based on many different samples which would effect the accuracy of this average.
# Question 21 a.
moose_2020b<-moose_clean%>%
  filter(Year == 2020)%>%
  mutate(MooseDensity=Estimated_Moose_Pop/Area)%>%
  print()
# Question 21 b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
# Question 22
sum_spe_browse<-moose_sap%>%
  group_by(Species, Ecoregion)%>%
  summarize(mean(BrowsingScore), mean(MooseDensity))%>%
  print()
# Question 23
install.packages("ggplot2")
library(ggplot2)

ggplot(sum_spe_browse, aes(x = `mean(MooseDensity)`, y = `mean(BrowsingScore)`, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
# Question 23 a.
#The hypothesis is proven as most of the species browsing score is below 3 at low densities, and only few species have a higher browsing score,
#while at high density all browsing scores are above three, meaning the moose are less selective at high desity.
# Question 23 b.
#Moose favour Willow the most, and favour Black_Spruce the least.
# Question 23 c.
#Black_Ash is not shown on the figure because it is only in the trial once,
#and it has exact same average browsing score and average moose density as a willow sample, 
#so the Black_Ash point is being covered.
# Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
# Question 25
moose_coll2<- rename(moose_coll, Ecoregion = study_sites)
# Question 25 b.
coll_merge<-moose_coll2 %>% 
  left_join(moose_2020, moose_coll, by = 'Ecoregion', relationship = "many-to-many")
# Question 26 a.
plot(coll_merge$collisions2020, coll_merge$MooseDensity, 
     xlab = "Collisions in 2020", 
     ylab = "Moose Density", 
     main = "Moose Density related to Number of Moose Vehicle Collisions")
# Question 26 b.
# Generally, there were a higher amount of collisons in 2020 when there was a higher moose density.
#There were 2 outliers to this trend, one more extreme and the other only slightly. The extreme outlier was there being over 100 collisions when the moose density was around 1, which is a much higher amount of collisions than any others.
#The slight outlier was a bit over 40 collisions when the moose density was around 1, which is slightly higher than the amount of collsions when the moose density was around 2.
# Question 27
coll_merge_per_capita<-coll_merge %>% 
  mutate(coll_per_capita = collisions2020 / human_pop) %>% 
  print()
# Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, 
     xlab = "Human Population", 
     ylab = "Moose Collisions per Capita", 
     main = "Moose Collisions per Capita in relation to Human Population")
# Question 29
#There are more collisions per capita when the human population is lower.
#Based on what I know about human populations in Newfoundland this makes sense,
#as regions with lower popuplations tend to be in more rural areas where more moose would be present,
#while regions with a higher population tend to be in more urban areas where there are less moose present.