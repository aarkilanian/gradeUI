setwd("R-assignment-work")
#1
library(dplyr)
#2
Moosedata <- read.csv("~/R-assignment-work/MoosePopulation.csv", stringsAsFactors=TRUE)
#3
View("Moosedata")
moose_better <- na.omit(Moosedata)
View(moose_better)
#4
mooselect <- select(moose_better, Ecoregion, Year, Area, Estimated_Moose_Pop)
#5
year_min <- min(mooselect$Year)
max_moose <- max(mooselect$Estimated_Moose_Pop)
print(max_moose)
print(year_min)
#6
moose2 <- mutate(mooselect, MooseDensity = Estimated_Moose_Pop / Area)
#7
plot(moose2$Year, moose2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose/km^2", 
     main = "Moose density in Newfoundland ecoregions over time")
#8
moose_west <- filter(moose2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "year", 
     ylab = "Moose/km^2", 
     main = "Moose density in Western Forests over time")
#9
moose_2020 <- filter(moose2, Year == "2020")
print(moose_2020)

moose_2020_high <- filter(moose_2020, MooseDensity>2.0)
print(moose_2020_high)

arrange(moose_2020_high, desc(MooseDensity))
#10
moosefinal <- moose2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

setwd("R-assignment-work")
library(dplyr)
#11
saplings <- read.csv("~/R-assignment-work/SaplingStudy.csv", stringsAsFactors=TRUE)
sap_better <- na.omit(saplings)
#12
sap_reg_browse<-group_by(sap_better,Ecoregion)%>%
  summarize(BrowsingScore=mean(BrowsingScore))%>%
  print()

avg_browse_reg <- arrange(sap_reg_browse, desc(BrowsingScore))
#highest = Northern Peninsula Forests, lowest = Straight of Belle Isle Barrens
#13
sap_reg_height<-group_by(sap_better,Ecoregion)%>%
  summarize(Height=mean(Height))%>%
  print()

sap_reg_height_low <- filter(sap_reg_height, Height<20.0)
print(sap_reg_height_low)
#Northern Peninsula Forests and Western Forests
#14
sap_specbro <-group_by(sap_better,Species)%>%
  summarize(BrowsingScore=mean(BrowsingScore))%>%
  print()

avg_browse_spec <- arrange(sap_specbro, desc(BrowsingScore))
print(avg_browse_spec)
#highest = black ash, lowest = black spruce
#15
frb <- filter(sap_better, Species == "Balsam_Fir") %>%
  group_by(Ecoregion)%>%
  summarize(BrowsingScore = mean(BrowsingScore))%>%
  print()
#16
barplot(frb$BrowsingScore, 
        names.arg = frb$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score", 
        main = "Average Browsing Score of Balsam Fir in Various Ecoregions", 
        col = "skyblue", cex.names = 0.6) 
#17
frbi <- filter(sap_better, Species == "Black_Spruce") %>%
  group_by(Ecoregion)%>%
  summarize(BrowsingScore = mean(BrowsingScore))%>%
  print()

barplot(frbi$BrowsingScore, 
        names.arg = frbi$Ecoregion, 
        xlab = "Ecoregion", 
        ylab = "Average Browsing Score", 
        main = "Average Browsing Score of Black Spruce in Various Ecoregions", 
        col = "purple", cex.names = 0.6) 
#on average, the balsam fir has a higher browsing score than the black spruce. 
#18 
sap_reg_tally<- sap_better %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
#19
sap_spec_tally<- sap_better %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#no there were not
#20
#I do not thing the sampling study is well distributed. 
#There are many more balsam fir tress than others, and only one black ash tree. 
#This is similar for the ecoregions, where several are over- or underrepresented. 

#It is important to recognize this bias because misrepresentation of data can 
#lead to false theories and incorrect findings in research

#21
moo <- filter(moose_better, Year == "2020")
moose_2020b <- mutate(moo, MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_better, 
            by = 'Ecoregion', 
            relationship = "many-to-many")
#22
sum_spe_browse <- group_by(moose_sap, Species)%>%
  group_by(Ecoregion)%>%
  summarize(BrowsingScore = mean(BrowsingScore), MooseDensity = mean(MooseDensity))%>%
  print()
#23
#the evidence does support the hypothesis; 
#the generalist behaviour was at high density and vice versa, 
#and the preferred trees were alder and willow in low densities
#the least liked was the black ash, as none were eaten, 
#followed by the black spruce
#the black ash likely wasn't shown since only one was in the sample

#24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests",
"Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens",
"Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#25
moose_coll2 <- moose_coll%>%rename(Ecoregion = study_sites)

coll_merge <- left_join(moose_coll2, moose_2020, 
                       by = 'Ecoregion', 
                       relationship = "many-to-many")
#26
plot(coll_merge$MooseDensity, 
    coll_merge$collisions2020, 
     xlab = "Moose Density", 
     ylab = "Collisions", 
     main = "Moose Density vs Collisions in 2020")

#as density increases, number of collisions increases, with one outlier 

#27
coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)
#28
plot(coll_merge_per_capita$human_pop, 
     coll_merge_per_capita$coll_per_capita, 
     xlab = "Population of People", 
     ylab = "Collisions per Capita", 
     main = "Human Population vs Collisions per Capita in 2020")
#29
#as the population increases, the number of collisions decreases. 
#this makes sense, since moose are more likely to be in areas further 
#away from dense towns and cities. 
