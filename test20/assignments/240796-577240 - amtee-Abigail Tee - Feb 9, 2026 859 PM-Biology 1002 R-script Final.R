1.
library(dplyr)


#this is a comment
setwd("C:/Users/abiga/Downloads/Biology_R")
2.
Moosedata <- read.csv("MoosePopulation.csv")


View(Moosedata)
3.
moose_clean <- na.omit(Moosedata)
4.
moose_sel <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)
5.
year_min <- min(Moosedata$Year)


#1904

5.
moose_max <- max(Moosedata$Estimated_Moose_Pop)

#41250, the largest number for a particular ecoregion is Central_Forests.

6. 
Moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

7. 
plot(Moosedata2$Year, Moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")


8. 
moose_west <- filter(Moosedata2, Ecoregion == "Western_Forests")     

8. 
plot(moose_west$Year, moose_west$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time",
     type ="l"
     )

9.
Moose_2020 <- filter(Moosedata2, Year ==2020) 

9.
moose_2020_high <- filter(Moose_2020, MooseDensity >2.0)

9.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

10.
moosefinal <- Moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


#Assignment 2

11. 
SaplingStudy <- read.csv("SaplingStudy.csv")
11. 
sap_clean <- na.omit(SaplingStudy)

12.
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>% 
  summarize(AverageBrowsing= mean(BrowsingScore)) %>% 
  print()

12. 
avg_browse_reg <- sap_clean %>%
    group_by(Ecoregion) %>% 
    summarize(AverageBrowsing= mean(BrowsingScore)) %>% 
    arrange(desc(AverageBrowsing)) %>%
  print()
  


12.
# Nothern_Peninsula_Forests have the most moose browsing
# StraitOfBelleIsleBarrens have the least moose browsing



13.
sap_reg_height <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_height= mean(Height)) %>% 
  print()

13.
sap_reg_height_low <- sap_clean %>%
  summarize(mean_height= mean(Height)) %>% 
  filter(mean_height< 20)%>%
  print()

13.
# The ecoregions Nothern_Peninsula_Forests and Westerm_Forests have average heights of less than 20cm.


14.
sap_spe_browse <- sap_clean %>% 
  group_by(Species) %>% 
  summarize(mean_browsing= mean(BrowsingScore)) %>% 
  print()

14. 
avg_browse_spe <- sap_clean%>%
  group_by(Species)%>%
  summarize(mean_browsing =mean(BrowsingScore))%>%
  arrange(desc(mean_browsing))%>%
  print()


14.
# Black_ASh has the highest browsing score
# Black-Spruce has the lowest browsing score


15. 
fir_reg_browse <- sap_clean %>% 
  filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_browsing= mean(BrowsingScore)) %>% 
  print()


16.
barplot(fir_reg_browse$mean_browsing, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "mean browsing intensity", 
        main = "Sapling browsing intensity by Ecoregion", 
        col = "forestgreen",
        cex.names = 0.6) # Reduces x-axis label size for readability


17.
spruce_reg_browse <- sap_clean %>% 
  filter(Species == "Black_Spruce") %>% 
  group_by(Ecoregion) %>% 
  summarize(mean_browsing= mean(BrowsingScore)) %>% 
  print()


17.
barplot(spruce_reg_browse$mean_browsing, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "mean browsing intensity", 
        main = "Sapling browsing intensity by Ecoregion", 
        col = "forestgreen",
        cex.names = 0.6) # Reduces x-axis label size for readability



17.
# To compare Black Spruce browsing to Balsam Fir browsing, these graphs help to show the difference in which ecoregion each tree is more browsed upon. For example, the Balsam Fir is browsed on in Maritime_Barrens whereas the Black Spruce is not.


18.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()


19.
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()


20.
# Yes, I think the the SaplingStudy dataset is evenly distrubted. I feel like most ecoregions have a proper amount of species and regions but I feel as though a few of them such as StraightOffBelleIsleBarrens and MaritimeBarrens are not presented enough. I feel like including more species in these ecoregions would help to broaden the study. 
# It is important to recognize bio in ecological datasets so that the dataset can be fixed and made fairly for each ecoregion or species. As well bias can change the results leading to an inaccurate dataset


21.
moose_2020b <- moose_clean%>%
  filter(Year==2020)%>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
  
21.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

22.
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean_browsingscore = mean(BrowsingScore),
          MooseMean_Density = mean(MooseDensity)) %>%
  print()
  
  
23.
# Yes, at lower average moose density we can see the average browsing score correlates more towards the moose preferences. Wheres as when they are in a higher density location, they appear to browse on whatever they can get and are less preference based. 
# It appears that Alder and Willow are the favorite species due to the fact they are browsed upon in most conditions and have a high browsing score even in low density. I think that Black_Spruce is browsed upon the least because even in high density areas they have the lowest average browsing score. 
# Black_Ash is browsed on the least and doesn't even appear on the figure presented. To me this means even in desperate times the moose would not browse upon this species. This could mean they are potentially poisonous to moose or they simply don't grow enough. 
  
  
24.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

24.
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
  

25.
moose_coll2 <-moose_coll%>%
  rename(Ecoregion = study_sites)


25.
coll_merge <- Moose_2020%>%
  left_join(moose_coll2, by= "Ecoregion")


26.
plot(coll_merge$MooseDensity, coll_merge$collisions2020,  
     xlab = "Moose Density", 
     ylab = "Collisions", 
     main = "Relation of Moose Density to number of vehicle collisions")

26.
#A trend I see is the higher the Moose Density the higher the Vehicle collisions. An outlier in this plot is a 1.0 at Moose density. This has a lot of collisions even when the Moose Density isn't very high.

27.
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)


28.
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop,  
     xlab = "Collisions", 
     ylab = "Human population", 
     main = "Collisions per capita vs. Human population")

29.
#A trend I see in the graph is the lower the population, the higher the collision rate. Although you would think that the higher the population would result in a higher number of collisions, it is not shown that way. Meaning I think this would opposite to my general understanding in comparison to my moose density plot where the higher the density the more collisions present. 


