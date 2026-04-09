library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)

#Question 21
#a
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area) %>%
  print()
#b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Question 22
sum_spe_browse <- moose_2020b %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean_browse = mean(BrowsingScore), mean_density = mean(MooseDensity)) %>%
  print()

#Question 23
#a 
#No, the moose's preferences are more scattered out in the species and lower density
#As for the moose's higher density, the moose's preferences are more specific in higher densities. 

#b
#The Moose's browse willows the most and browse Alder the least.

#c
#Black Ash is the only species not displayed on the figure. This is because
#The preference for Black Ash from Moose is so low in all densities that it was not included.

