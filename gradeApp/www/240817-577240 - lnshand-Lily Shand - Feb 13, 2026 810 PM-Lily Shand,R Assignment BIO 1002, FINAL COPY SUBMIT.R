#Q1 moose
library(dplyr)

#Q2
moosedata <- read.csv("MoosePopulation.csv")

library(readxl)
MoosePopulation <- read.csv("MoosePopulation.csv")
View(MoosePopulation)

#Q3
View(MoosePopulation)
moose_clean <- na.omit(MoosePopulation)

#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5
min(moose_sel$Year)
max(moose_sel$Estimated_Moose_Pop)

#Q6
str(moose_sel)
moose_sel <- mutate(moose_sel, Estimated_Moose_Pop = as.numeric(Estimated_Moose_Pop))
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
str(moosedata2)

#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "Year", 
     ylab = "Moose per sq km", 
     main = "Moose Density in Newfoundland Ecoregions Over Time")

#Q8
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moosedata2$Year, moosedata2$MooseDensity,
     type = "l",
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")

#Q9
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moosedata2, MooseDensity >2.0)
moose_2020_high_byD <- arrange(moosedata2, desc(MooseDensity))

#Q10
moose_2020 <- moosedata2 %>%  filter(Year == "2020")
moose_2020_high <- moosedata2 %>% filter(MooseDensity > 2.0)
moose_2020_high_byD <- moosedata2 %>%  arrange(desc(MooseDensity))

#Q11 sapling 
saplings <- read.csv("SaplingStudy.csv")
View(saplings)
sap_clean <- na.omit(saplings)

#Q12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing=mean(BrowsingScore)) %>% 
print()

#Q12b
avg_browse_reg <- sap_reg_browse %>% 
  arrange(desc(AverageBrowsing))

#Q13
sap_reg_height <- saplings %>% 
  group_by(Ecoregion) %>% 
summarize(mean(BrowsingScore))

#Q13b
sap_reg_height_low <- saplings %>% 
  filter(Height < 20)
#The Ecoregions with heights of saplings less than 20cm: North Shore Forests,Northern Peninsula Forests, Central Forests, and Western Forests

#Q14
sap_spe_browse <- group_by(sap_clean, Species) %>% 
summarize(mean(BrowsingScore))

#Q14b
avg_browse_spe<- sap_spe_browse %>% arrange(desc("mean(BrowsingScore)"))
#Black Ash has the highest mean browsing score and Black spruce has the lowest browsing score

#Q15
fir_reg_browse <- sap_clean %>%  
  filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean(BrowsingScore)) %>% 
print()

#Q16
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Mean Browsing Score", main = "Average Browsing Intensity in Different Ecoregions For Balsam Fir", col = "forestgreen") 

#Q17a
spruce_reg_browse <- sap_clean %>%  
  filter(Species == "Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean(BrowsingScore)) %>% 
print()

#Q17b
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Mean Browsing Score", main = "Average Browsing Intensity in Different Ecoregions For Black Spruce", col = "forestgreen") 

#Q17c
#The Black Spruce browsing intensity is more uniformly distributed across different ecoregions. The Balsam Fir browsing intensity has more extreme differences in distribution across different ecoregions.

#Q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Q19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Q20a
#Accoring to the saplingdata, the sapliongs are unevenly distributed. North Shore Forests and Northen peninsula forests are overrepresented and Strait Of Belle Isle Barrens is underrepresented. 

#Q20b.It is important to recognize bias in ecological datasets because misleading conclusions can be made. Recognizing biases like biased sampling, errors in measurement and issues with collecting the data can help avoid inaccurate results.  

#Q21a
moose_2020b <- filter(moose_clean, Year == "2020") %>% 
mutate(MooseDensity = Estimated_Moose_Pop/Area)

#Q21b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#Q22
sum_spe_browse <- moose_sap %>% group_by(Species) %>% 
  summarize(mean(BrowsingScore), mean(MooseDensity))

#Q23a
install.packages("ggplot2")
library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
  
#Yes, the plot is evidence that supports the hypothesis because it shows at low moose density browsing intensity varies more, and at high density browsing score increases among the species. There is a shift toward more generalist feeding by moose.

#Q23b
#The moose favour the Willow species the most, it has the highest browsing score based on the densities. The moose least favour the Black Spruce species, it is least browsed particularly at low densities.

#Q23c
#The species not shown is Black Ash, because there is no data to plot. There's no data likely because of insufficient observations or data missing.

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25a
moose_coll2 <- moose_coll %>% 
  rename(Ecoregion = study_sites)

#Q25b
coll_merge <- moose_2020 %>%
  left_join(moose_coll2, by = "Ecoregion")

#Q26a
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
xlab= "MooseDensity per sq km",
ylab="Moose-Vehicle Collisions 2020",
main="Moose Density vs Collisions in 2020")

#Q26b 
#As moose density increases, the number of moose vehicle collisions in 2020 also increases. There is one outlier around 1.0 moose/km^2 with high collisions compared to similar densities.

#Q27
coll_merge_per_capita <- coll_merge %>% 
  mutate(coll_per_capita = collisions2020/human_pop)


#Q28
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab= "Collisions per Capita",
     ylab= "Human Population",
     main= "Collisions per Capita vs Human Population")

#Q29
#In general, areas with higher moose density have more collisions and in areas with small human population have more collisions per capita. This makes sense becuase in Newfoundland's rural areas there's a higher moose to human ratio and less city increasing the possibilty of collision.

