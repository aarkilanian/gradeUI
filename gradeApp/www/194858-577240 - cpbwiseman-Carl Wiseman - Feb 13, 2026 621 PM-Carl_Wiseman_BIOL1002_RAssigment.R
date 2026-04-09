# Title: BIOL 1002: Quantitative Methods Assignment

# Author: Carl Wiseman

# Date: 13-09-2026

# Set working directory ---------------------------

setwd("C:/Bio R")

# Load data ---------------------------

dataset <- read.csv("C:/Bio R/MoosePopulation.csv")
dataset <- read.csv("C:/Bio R/SaplingStudy.csv")
install.packages("dplyr")

# Part 1

# Question 1 --------------------------------------

library(dplyr)

# Question 2 --------------------------------------

moosedata <- read.csv("MoosePopulation.csv")

# Question 3 -------------------------------------

Moose_clean<- na.omit(moosedata)

# Question 4 ------------------------------------

moose_sel <- select(Moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5 ------------------------------------
#a)

year_min <- min(moose_sel$Year,na.rm = FALSE)

#b)

moose_max<-max(moose_sel$Estimated_Moose_Pop,na.rm = FALSE)

# Question 6 -----------------------------------

moosedata2<-mutate(moose_sel, MooseDensity= Estimated_Moose_Pop/Area)


# Question 7 -----------------------------------

plot(moosedata2$Year,moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main="Moose density in Newfoundland ecoregions over time")

# Question 8 ----------------------------------- 
# a)

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# b)

plot(moose_west$Year,moose_west$MooseDensity,
     type="l",
    xlab="Year",
    ylab="Moose Density",
    main="Moose density in Western Forest over time")

# Question 9 -----------------------------------
# a)

Moose_2020<-filter(moosedata2,Year=="2020")

# b)

Moose_2020 %>%
  filter(MooseDensity > 2.0)

# c)

moose_2020_high_byD<-arrange(Moose_2020, desc(MooseDensity))

# Question 10

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


# Part 2
# Question 11
# a)

saplings<-read.csv(file = "SaplingStudy.csv")

# b)

saplings_clean<-na.omit(saplings)

# Question 12
# a)

sap_reg_browes <- saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm =FALSE))%>%
  print()

# b)

avg_browse_reg<-arrange(sap_reg_browes,desc(mean_browsing))

#Northern_Peninsula_Forests is the higest.
#StraitOfBelleIsleBarrens is the lowest

# Question 13

# a)

sap_reg_height<-saplings%>%
  group_by(Height)%>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm =FALSE))%>%
  print()

# b)

sap_reg_height_low <- saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height, na.rm = FALSE))%>%
  filter(mean_height < 20)%>%
  print()

# Northern_Peninsula_Forests and Western_Forests are less then 20cm

# Question 14
# a)

sap_spe_browse<-saplings%>%
  group_by(Species)%>%
  summarize(mean_BrowsingScore=mean(BrowsingScore,na.rm=FALSE))%>%
  print()

# b)

avg_browse_spe<-saplings%>%
  group_by(Species)%>%
  summarize(mean_browsing=mean(BrowsingScore,na.rm=FALSE))%>%
  arrange(desc(mean_browsing))%>%
  print()

# Question 15

fir_reg_browse<-saplings%>%
  filter(Species=="Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))

# Question 16

barplot(fir_reg_browse$mean_BrowsingScore, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "mean Brrowsing Score", 
        main = "mean Browsing Score by Ecoregion of Balsam fir", 
        col = "purple",  cex.names = 0.6) 

# Question 17

spruce_reg_browse<-saplings%>%
  filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE))

barplot(spruce_reg_browse$mean_BrowsingScore, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab = "mean Browsing Score", 
        ylab = "Ecoregion", 
        main = "mean Browsing Score by Ecoregion of black spruce", 
        col = "orange",  cex.names = 0.6)
#there is browsing across all ecoregions of balasm fir and there is not for black spurce.

# Question 18

sap_reg_tally<- saplings_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
# no not all regiongs had the same number counted

# Question 19

sap_spe_tally<- saplings_clean %>%
  group_by(Species) %>%
  tally() 
 
# Question 20

# a) yes some areas and species are over represented.
# some the lagest area has 8 samples while the smallest has 1 sample.
# and the number of sampels taken from each speises is not consitant.
#b) It is important because the data can become missleading,
# for example the some regions have no sample for a spices which in these  
# graphs that would so up as no browsing.

# Part 3
# Question 21
#a)

moose_2020b<-Moose_clean%>%
  filter(Year=="2020")%>%
  mutate(MooseDensity=Estimated_Moose_Pop/Area)

#b)

moose_sap <- left_join(moose_2020b, saplings_clean, 
                       by = 'Ecoregion', 
                       relationship = "many-to-many")

# Question 22

sum_spe_browse<-moose_sap%>%
  group_by(Species,Ecoregion)%>%
  summarize(mean_BrowsingScore=mean(BrowsingScore,na.rm=TRUE),
            mean_Moosedensity=mean(MooseDensity,na.rm=TRUE))%>%
  print()

# Question 23

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#a) yes, they there is evidence to support this based on the chart as at higher 
# densities the browsing is higher for all species.

#b) willow is most favored by moose as the browsing rate is higher then all the rest
# and is shown that at low density they are choosing  it.

#c) Black ash is not shown, as it only appears once in the data and 
# it is not browsed 

# Question 24

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)

study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens",
                 "Avalon_Forests",
                 "StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

# Question 25
#a)

moose_coll2<- moose_coll%>%
  rename(Ecoregion=study_sites)

#b)

coll_merge<-left_join(Moose_2020,moose_coll2,by="Ecoregion")

# Question 26
#a)

plot(coll_merge$MooseDensity, 
     coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-Vehicle Collisions (2020)",
     main = "Relationship Between Moose Density and Moose-Vehicle Collisions",
     pch = 19,        
     col = "darkgreen")

#b)
# there is a outlier at the 1.0 density mark, but other then that as the 
# density of moose increases the collisions slowly and stedily increase.

# Queston 27

coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)

# Question 28

plot(coll_merge_per_capita$human_pop, 
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Capita",
     main = "Moose Collisions per Person vs Human Population",
     pch = 19,         
     col = "darkblue")

# Question 29
#the points show that areas of lower human population have a higher number of
# moose collisions. this data makes sense as most moose are in areas with less
#people as is the case with newfoundland more rural areas have higher rates 
#of moose collisions.