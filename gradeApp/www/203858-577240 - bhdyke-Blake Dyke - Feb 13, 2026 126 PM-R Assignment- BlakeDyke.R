#BlakeDyke
#q1
library(dplyr)
library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
Year_min <- min(moose_sel$Year)
moose_max <- max(moose_sel$Year)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, xlab = "Year", ylab= "Height", main = "Moose Density In Newdoundland")
moose_2020 <- filter(moosedata2,Year==2020)
moose_2020_high <- filter(moose_2020,MooseDensity>2)
moose_2020_high_byD <-arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingStudy.csv")
saplings_clean <- na.omit(saplings)
sap_reg_browse <- saplings_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(averagebrowsing=mean(BrowsingScore)) %>% 
  print()
sap_reg_height <- saplings_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(averageheight=mean(Height)) %>% 
  print()
#The western and Northern Peninsula forests have lower height then 20.
fir_reg_browse <- sap_reg_browse <- saplings_clean %>% 
  filter(Species=="Balsam_Fir")%>% 
  group_by(Ecoregion)%>%
  summarise(averagebrowsing=mean(BrowsingScore))%>%
  print()
barplot(fir_reg_browse$averagebrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing", main = "Fir_Reg_browse", col = "forestgreen",cex.names = 0.6)
spruce_reg_browse <- sap_reg_browse <- saplings_clean %>% 
  filter(Species=="Black_Spruce")%>% 
  group_by(Ecoregion)%>%
  summarise(averagebrowsing=mean(BrowsingScore))%>%
  print()        
barplot(spruce_reg_browse$averagebrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing", main = "Spruce_Reg_Browse", col = "forestgreen",cex.names = 0.6)        
#Black spruce has a higher average browsing score in certain Ecoregion. The Balsam fir has a lower average browsing but doesn't have 0 in any Ecoregion.
sap_reg_tally<- saplings_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally <- saplings_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
# The data set is not evenly distributed. The north shore forest is over presented and the straitofbelleislebarrens and underrepresented.
# It is important to recognize bias in this data set is to realize that the data is objective, but in-reality is observed by humans. Balsam fir has 11 when the black as has 1.
moose_2020b <- moose_clean %>% filter(Year=="2020")%>% mutate(MooseDensity = Estimated_Moose_Pop/Area)
moose_sap <- left_join(moose_2020b, saplings_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse<- moose_sap %>%
  group_by(Ecoregion, Species) %>%
  summarise(mean_density = mean(MooseDensity), mean_browse = mean(BrowsingScore)) %>% 
  print()
library(ggplot2)

ggplot(sum_spe_browse, aes(x = mean_density, y = mean_browse, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#the hypothesis is not supported, with a lower moose population the moose still favored the willow tree the most.
#The willow tree is the most consumed while the white birch is favored least. Even at a low density of moose the willow was still the most eaten.
#The sapling species not shown is Black ash. This is due to the same mean density in other species.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites) %>% 
  rename(Ecoregion = study_sites)
moose_col <- moose_2020 %>% 
  left_join(moose_coll, by = "Ecoregion")
plot(
moose_col$MooseDensity, collisions2020,
xlab = "MooseDensity", 
ylab = "Collisions", 
main = "Moose Density and Collisions 2020")
#With a higher moose density, the number or accidents increase. One outlier would be at a density of 1.0 where moose crashes have increased to 100.
#The Avalon forest has the highest collisions rate.
coll_merge_per_capita <- moose_col %>% 
  mutate(coll_per_capita=collisions2020/human_pop)
plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collosion Per Capita",
     main = "Number Of People and Collisions Per Capita") 
     #With a lower population, there are more collisions per capita. This makes sense to what we know about moose in Newfoundland because more accidents happen in rural areas.       
    


