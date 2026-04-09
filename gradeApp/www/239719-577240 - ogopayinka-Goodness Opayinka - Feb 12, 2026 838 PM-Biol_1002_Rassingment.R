#title: Biology 1002 R assignment
#name: Goodness Opayinka
#student id: 202414479
#Date: 14 January 2026

#QUESTION 1---------------------------
install.packages("dplyr")
library(dplyr)


#QUESTION 2------------------------
moosedata <- read.csv("MoosePopulation 2.csv")


#QUESTION 3----------------------
moosedata <- na.omit(moosedata)


#QUESTION 4 -----------------------
moosedata <- select(moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)


#QUESTION 5 ---------------

#a---------
min(Moosedata$Year)
#1904

#b--------
max(Moosedata$Estimated_Moose_Pop)
#41250


#QUESTION 6-----------------
Moosedata <- mutate(moosedata, MooseDensity = Estimated_Moose_Pop / Area)


#QUESTION 7 ------------------
plot(Moosedata$Year, Moosedata$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")


#QUESTION 8 -----------------

#a--------
MooseDataWest <- filter(Moosedata, Ecoregion == "Western_Forests")

#b--------
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", main= "Moose density in western forests over time")
      

#QUESTION 9 ------------------

#a------------
Moosedata_2020 <- filter(Moosedata, Year == "2020")

#b------------
Moosedata_2020b <- filter(Moosedata_2020, (MooseDensity > 2))

#c----------
moose_2020_high_byD <- arrange(Moosedata_2020b, desc(MooseDensity))


#QUESTION 10---------------
moosefinal <- Moosedata %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


#QUESTION 11---------------

#a----------
saplings<- read.csv("SaplingStudy 2.csv")

#b----------
sap_clean <- na.omit(saplings)


#QUESTION 12---------------

#a-----------
sap_reg_browse <- sap_clean  %>%
  group_by(Ecoregion) %>%
summarise(AverageBrowsingScore=mean(BrowsingScore)) %>%
print()

#b------------
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsingScore))
#highest browsing- "Northern_Peninsula_Forests"
#lowest browsing - "Strait Of Belle Isle Barrens"


# QUESTION 13--------------

#a-----------
sap_reg_height<-sap_clean  %>%
  group_by(Ecoregion) %>%
  summarise(AverageHeight=mean(Height)) %>%
  print()

#b----------
sap_reg_height_low <- sap_reg_height %>%
  filter( AverageHeight < 20) %>%
  print()
# Northern_Peninsula_Forests and Western_Forests have average heights less than 20cm


#QUESTION 14-----------

#a--------
sap_spe_browse <- sap_clean  %>%
  group_by(Species) %>%
  summarise(AverageBrowsingScore=mean(BrowsingScore)) %>%
  print()

#b---------
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsingScore))
#Black_Ash has the highest browsing score and Black_Spruce has the lowest browsing score


#QUESTION 15---------
fir_reg_browse <- sap_clean %>%
  filter(Species== "Balsam_Fir") %>%
  group_by(Ecoregion)%>%
  summarise(AverageBrowsingScore=mean(BrowsingScore)) %>%
  print()


#QUESTION 16 ---------------
barplot(fir_reg_browse$AverageBrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average browsing score", main = " Average browsing intensity for Balsam_fir at different ecoregions ", col = "Blue", cex.names = 0.6) 


#QUESTION 17--------------

#a-------
spruce_reg_browse <- sap_clean %>%
  filter(Species== "Black_Spruce") %>%
  group_by(Ecoregion)%>%
  summarise(AverageBrowsingScore=mean(BrowsingScore)) %>%
  print()

#b-------
barplot(spruce_reg_browse$AverageBrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average browsing score", main = " Average browsing intensity for Black_Spruce at different ecoregions ", col = "Red", cex.names = 0.6) 

#c--------
#The average browsing at different Eco regions is higher for Balsam Fir than for Black Spruce.


#QUESTION 18-----------
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()


#QUESTION 19-----------
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()


#QUESTION 20 -----------

#a ------ 
#The data is not evenly distributed. Black_ash is underrepresented while Balsam_fir is over represented for species. Strait Of Belle Isle Barrens is underrepresented while North_Shore_Forests is over represented for Eco regions. 

#b --------
#It is important to recognize bias in data sets to ensure validity and reliability.


#QUESTION 21-------------

#a-------
moose_2020b <- moosedata %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)%>%
  print()

#b--------
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")


#QUESTION 22----------
sum_spe_browse <- moose_sap %>%
  group_by(Ecoregion,Species) %>%
  summarize(AverageBrowsingScore = mean(BrowsingScore), AverageMooseDensity = mean(MooseDensity)) %>%
  print()


#QUESTION 23-----------

#a-------
#Yes, the evidence supports the hypothesis. Moose show strong preferences at low density and shift to more generalist browsing at higher density

#b-------
#The moose favor the willow species the most. Black Spruce is the least favored species.

#c-------
#Black Ash is not shown on the figure because it was only examined in one Eco region. It is not representative of the species.


# QUESTION 24----------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)


#QUESTION 25------------

#a--------
moose_coll2 <- moose_coll%>%
rename (Ecoregion = study_sites)

#b--------
coll_merge <- left_join(Moosedata_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")


#QUESTION 26----------

#a---------
plot(x = coll_merge$MooseDensity, y = coll_merge$collisions2020, 
     main = "Relationship between moose density and collisions in 2020", 
     xlab = "Moose Density",              
     ylab = "Collisions",               
     pch = 19,                                
     col = "blue")    

#b---------
#As the moose density increases the amount of collisions also increases. There is an outlier when the moose density is 1.


#QUESTION 27-------------
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020/human_pop)


#QUESTION 28-------------
plot(x = coll_merge_per_capita$coll_per_capita, y = coll_merge_per_capita$human_pop, 
     main ="Relationship between collisions per capita and human population", 
     xlab = "Collisions per capita",              
     ylab = "Human population",               
     pch = 19,                                
     col = "pink") 


#QUESTION 29--------------
 #As human population increases, collisions per capita decreases. The trend makes sense because as moose and human populations increase, collisions are more frequent.    





