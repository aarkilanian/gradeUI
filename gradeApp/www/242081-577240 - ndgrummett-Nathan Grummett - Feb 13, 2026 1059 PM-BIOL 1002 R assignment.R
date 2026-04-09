#Title: My R file for Biol 1002 R assignment
#Author: Nathan Grummett
#Date February 13th 2026



#Question 1
library(dplyr)

#Question 2
moosedata = read.csv("MoosePopulation.csv") 

#Question 3
View(moosedata)
moose_clean = na.omit(moosedata)

#Question 4
moose_sel = select(moose_clean, Ecoregion, Year, Area,Estimated_Moose_Pop)

#Question 5
year_min = min(moose_sel$Year)
moose_max = max(moose_sel$Estimated_Moose_Pop)

#Question 6
moosedata2 = mutate(moose_sel, MooseDensity = Estimated_Moose_Pop/Area)

#Question 7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab= "Year", ylab = "Moose Density", main = "Density in ecoregions of newfoundland over time")

#Question 8
moose_west = filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, xlab = "Year", ylab = "Moose density", main = "Moose Density in western forests of newfoundland over time")

#Question 9
moose_2020 = filter(moosedata2, Year == "2020")
moose_2020_high = filter(moose_2020, moose_2020$MooseDensity > 2)
moose_2020_high_byD = arrange(moose_2020_high, desc(MooseDensity))

#Question 10
moosefinal = moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Question 11
saplings = read.csv("SaplingStudy.csv")
sap_clean = na.omit(saplings)

#Question 12
sap_reg_browse = sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()
avg_browse_reg = sap_reg_browse %>%
  arrange(desc(mean_BrowsingScore)) %>%
  print()
#Northern peninsula forests had the highest average and Strait of belle isle barrens had the lowest average


#Question 13
sap_reg_height = sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_Height = mean(Height))%>%
  print()
sap_reg_height_low = filter(sap_reg_height, mean_Height < 20)
print()
#Northern peninsula forests and western forests have mean heights lower than 20

#Question 14
sap_spe_browse = sap_clean %>%
  group_by(Species) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()
ave_browse_spe = sap_spe_browse %>%
  arrange(desc(mean_BrowsingScore)) %>%
  print()
#Black ash has the highest mean browsing score and Black spruce has the lowest


#Question 15
fir_reg_browse = sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()

#Question 16
barplot(fir_reg_browse$mean_BrowsingScore, names.arg = fir_reg_browse$Ecoregion,
        cex.names = 0.6, xlab = "Ecoregion", ylab = "Average Browsing score",
        main = "Average browsing score of Balsim Fir by ecoregions", col="green")

#Question 17
spruce_reg_browse = sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()
barplot(spruce_reg_browse$mean_BrowsingScore, names.arg = spruce_reg_browse$Ecoregion
        ,xlab = "Ecoregion", ylab = "Average Browsing Score", cex.names=0.6, main= "Average browsing score of Black Spruce by ecoregions")
#Average browsing score of balsim fir trees is higher than black spruce trees in every region except long range barrens

#Question 18
sap_reg_tally = sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print() 

#Question 19
sap_spe_tally = sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print() 

#Question 20
#This dataset is not evenly distributed. The Balsim Fir species was over represented while the Black Ash species was underrepresented. The Northen Shore Forest and Northern Penninsula Forest regions were overrepresented while the straits of belle island ecoregions were underrepresented.
#Outliers are possible, if you underrepresent certain species/regions or overrepresent others your sample will not reflect the population and your data will be void.

#Question 21
moose_2020b = moose_clean %>%
  filter(Year == '2020') %>%
  mutate(MooseDensity = Estimated_Moose_Pop/Area) %>%
  print()
moose_sap = left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = 'many-to-many')

#Question 22
sum_spe_browse = moose_sap %>%
  group_by(Species,Ecoregion) %>%
  summarize(meanBrowsingScore = mean(BrowsingScore), meanMooseDensity = mean(MooseDensity)) %>%
  print()

#Question 23
#Yes, at low densities the browsing scores are much more spread out with only a few species at the top while at high densities, the browsing scores are concentrated.
#Moose browse Willow and Alder the most. Moose browse Black Spruce the least
#Black ash because it overlaps with one of the willow points

#Question 24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll = data.frame(collisions2020, human_pop, study_sites)

#Question 25
moose_coll2 = moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge = left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = 'many-to-many')

#Question 26
plot(moose_2020$MooseDensity, moose_coll2$collisions2020, ylab= "Collisions", xlab = "Moose Density", main ="Collisions vs Moose Density")
#There is an upward trend wherein collisions increase with moose density. There is one outlier exhibiting over 100 collision at a density of 1.

#Question 27
coll_merge_per_capita = coll_merge %>%
  mutate (coll_per_capita = collisions2020/human_pop) %>%
  print()

#Question 28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human population", ylab = "Collsions per capita", main = "Collisions Per Capita Vs Human Pop")

#Question 29
#Collisions per capita appear to decrease with human population. I know very little about moose and human interactions.
