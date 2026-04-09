install.packages(c('dplyr', 'readr', 'tidyr'))
library(dplyr)
MoosePopulation <- read.csv("MoosePopulation.csv")
date1 <- read.csv(file = "MoosePopulation.csv")
moose_clean <- na.omit(MoosePopulation)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_clean$Year)
year_min<-min(moose_clean$Year)
max(moose_clean$Estimated_Moose_Pop)
moose_max<-max(moose_clean$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year" ,
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Ecoregions Over Time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland's Western Forest Ecoregions Over Time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity == "<2.0")
arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc (MooseDensity)) %>%
  print()

saplings <- SaplingStudy
sap_clean<-na.omit (saplings)
sap_reg_browse<-sap_clean%>%
  group_by (Ecoregion)%>%
  summarise (AverageBrowsing=mean(BrowsingScore))%>%
  print()
avg_browse_reg<-arrange(sap_reg_browse, AverageBrowsing)
view(avg_browse_reg)
       # The highest average browsing score was Northern_Peninsula_Forests while the lowest was StraitOfBelleIsleBarrens

sap_reg_height<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarize(AverageHeight=mean(Height))%>%
  print()
sap_reg_height_low<-sap_reg_height%>%
  filter(AverageHeight<20)%>%
  print()
     # The ones that have heights less than 20cm are Northern_Penisula_Forests and Western_Forests    

sap_spe_browse<-sap_clean%>%
  group_by (Species) %>%
  summarise(AverageBrowsingSpe=mean (BrowsingScore))%>%
  print()
avg_browse_reg<-arrange(sap_spe_browse, AverageBrowsingSpe)
view(avg_browse_spe)
     # The highest browsing score is Black_Ash while the lowest is Black_Spruce 

fir_reg_browse<-sap_clean%>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(AverageBalsmBrowsing=mean(BrowsingScore))%>%
  print()
barplot(fir_reg_browse$AverageBalsmBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "ecoregions",
        ylab = "browsing score")

spruce_reg_browse<-sap_clean%>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(Averagesprusebrowsing=mean(BrowsingScore))%>%
  print()
barplot(spruce_reg_browse$Averagesprusebrowsing, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "ecoregions",
        ylab = "browsing score")
# There are a lot less Black Spruce Browsing occuring in the Maritime Barrens and Eastern Hyper Oceanic Barrens compared to Balsam Browsing 

sap_reg_tally<- sap_clean%>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()

sap_spe_tally<- sap_clean%>%
  group_by(Species)%>%
  tally()%>%
  print()

# No, the Sapling Study is not evenly distributed. One example of the reasoning behind this is because there are 5 data samples.
# It is very important to recognize bias when analyzing data due to if you recognize them and continue research, it would change the results. It would also change the research methods. 

moose_2020<-moose_clean%>%
  filter(Year == 2020)
moose_2020b<-moose_2020%>%
  mutate(MooseDensity = Estimated_Moose_Pop/Area)

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

sum_spe_browse<-moose_sap%>%
  group_by(Species)%>%
  group_by(Ecoregion)%>%
  summarise(averagemoosebrowsing=mean(MooseDensity))%>%
  print()

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#A: Yes, the data supports the hypothesis due to the fact that moose browsing changes while density increases. This supports the idea that moose become much more general at higher densities 
#B: Moose favour the sapling species with the highest average browsing and also browse the species with the lowest average the least. This supports the idea that they have clear feeding preferences. 
#C: The missing sapling species was most likely excluded because there was a little bit or no browsing data for it at all. There's a chance that it didn't show up enough in the sample to include. 

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

moose_coll2<-moose_coll%>%
  rename_with(~"Ecoregion", .cols = study_sites)
coll_merge<-left_join(moose_coll2, moose_2020, by ="Ecoregion")  
length(coll_merge$collisions2020)
# Answer : 9 

plot(coll_merge$collisions2020, coll_merge$Estimated_Moose_Pop,
     xlab = "Number of Moose-Vehicle Collisions (2020)", 
     ylab = "Moose Density",
     main = "Moose Density vs. Moose-Vehicle Collisions")
# The trend that I noticed was that the higher the population, the more collision that happened. There are also two outliers display spots where moose density and collision numbers do not match the overall trend. This supports the idea that there are other factors affecting collisions. 

coll_merge_per_captia<-coll_merge%>%
  mutate(coll_merge_per_captia=collisions2020/human_pop)%>%
  print()

plot(coll_merge_per_captia$coll_merge_per_captia, coll_merge_per_captia$human_pop,
     xlab = "# Of Collisions Per Captia",
     ylab = "Human Population",
     main = "Number of Collisions Per Captia vs. Human Population")

# The trends that I see is a small negative trend where areas with larger human population have less moose collisions per capita. This makes sense based on what I know about moose and human populations in Newfoundland because moose are much more common in less populated and rural areas. 