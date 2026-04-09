#Part 1 - Moose Population in Newfoundland and Labrador
install.packages("dplyr")
moosedata<-read.csv("MoosePopulation.csv")
View(moosedata)
moose_clean<-na.omit(moosedata)
moose_sel<- select.list(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min<-min(moose_clean$Year)
#According to the above code, the oldest observation is from 1904
moose_max<-max(moose_clean$Estimated_Moose_Pop)
#According to the above code, the maximun moose population was 41250
moosedata2<- mutate(moose_clean, MooseDensity = Estimated_Moose_Pop/Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "Year",
     ylab = "Moose per square km",
     main = "Moose density in Newfoundland Ecoregions Over Time")
moose_west<- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab = "Year",
     ylab = "Moose per square km",
     main = "Moose Density in Newfoundland Western Forests Over Time")
moose_2020<- filter(moosedata2, Year== "2020")
moose_2020_high<- filter(moosedata2, Year == "2020", MooseDensity > 2)
moose_2020_high_byD<- arrange(moose_2020_high, desc(MooseDensity))
moosefinal<- moosedata2 %<%
  filter(Year == 2020)%>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity))%>%
  print()
#         Ecoregion Year Area Estimated_Moose_Pop MooseDensity
#Northern_Peninsula_Forests 2020 12000               32000     2.666667
#       North_Shore_Forests 2020  8400               21740     2.588095
#           Western_Forests 2020  6800               17000     2.500000
#           Central_Forests 2020 20500               41250     2.012195
#Part 2 - Tree Sapling Study
saplings<- read.csv("SaplingStudy.csv")
View(saplings)
sap_clean<- na.omit(saplings)
sap_reg_browse<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarise(BrowsingScore = mean(BrowsingScore))%>%
  print()
#Ecoregion                 BrowsingScore
#<chr>                             <dbl>
#1 Avalon_Forests                     2
#2 Central_Forests                    4
#3 EasternHyperOceanicBarrens         2.4
#4 Long_Range_Barrens                 2.6
#5 Maritime_Barrens                   1.83
#6 North_Shore_Forests                4.38
#7 Northern_Peninsula_Forests         4.57
#8 StraitOfBelleIsleBarrens           1
#9 Western_Forests                    4.5
avg_browse_reg<- arrange(sap_reg_browse, desc(BrowsingScore))
print(avg_browse_reg)
#The Northern Peninsula Forests had the highest average browsing score.
#The Strait of Belle Isle Barrens had the lowest average browsing score.
sap_reg_height<- sap_clean %>% 
  group_by(Ecoregion) %>%
  summarise (Height = mean(Height)) %>%
  print()
#Ecoregion                  Height
#<chr>                       <dbl>
#1 Avalon_Forests               32.4
#2 Central_Forests              23.8
#3 EasternHyperOceanicBarrens   31.6
#4 Long_Range_Barrens           29.9
#5 Maritime_Barrens             26.7
#6 North_Shore_Forests          22.3
#7 Northern_Peninsula_Forests   19.9
#8 StraitOfBelleIsleBarrens     25.4
#9 Western_Forests              18.9
sap_reg_height_low<- filter(sap_reg_height, Height < 20)
print(sap_reg_height_low)
#Ecoregion                  Height
#<chr>                        <dbl>
#1 Northern_Peninsula_Forests    19.9
#2 Western_Forests               18.9
#The Northern Peninsula and Western forests have average heights lower than 20
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise (BrowsingScore = mean(BrowsingScore)) %>%
  print()
#Species        BrowsingScore
#<chr>                  <dbl>
#1 "Alder"                 4.25
#2 "Balsam_Fir"            3.14
#3 "Black_Ash"             5
#4 "Black_Spruce"          2.33
#5 "White_Birch"           3.14
#6 "Willow"                4.31
avg_browse_spe <- arrange(sap_spe_browse,desc(BrowsingScore))
print(avg_browse_spe)
#Black Ash has the highest browsing score, and Black Spruce has the lowest.
fir_reg_browse <- sap_clean %>%
  filter(Species =="Balsam_fir") %>%
  group_by(Ecoregion) %>%
  summarise(BrowsingScore = mean(BrowsingScore))
barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Browsing Score of the Balsam Fir by Ecoregion",
        col = "blue",
        cex.names = 0.3)
#I had to make cex.names smaller than 0.6 recommended to fit the labels.
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(BrowsingScore = mean (BrowsingScore))
barplot(spruce_reg_browse$BrowsingScore,names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Browsing Score of the Black Spruce by Ecoregion",
        col = "pink",
        cex.names = 0.2)
#The Black Spruce has a generally lower Browsing Score in all Ecoregions when
#compared to the Balsam Fir, but has a higher score in the Western Forests.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()
#"n" in this table shows that a different amount of trees were counted in each 
#Ecoregion.
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally()%>%
  print()
#"n" in this table shows that a different amount of saplings were collected for
#each species.
#This sapling study is not evenly distributed because of sampling biass. There 
#are variations in the amount of trees counted in each Ecoregion and how many
#species were sampled. The North Shore forests were over represented with 8 
#trees counted, and the Straight of Belle Isle Barrens were underrepresented
#with 1. The Balsam Fir was over represented with 11 and the Black Ash was 
#underrepresented with 1.
#Recognizing sampling bias in data sets is important to ensure the data is 
#accurate in its portrayal and samples are taken fairly.
#Part 3 - Creating and Joining Datasets
moose_202b<- moose_clean %>%
  filter(Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop/Area)
moose_sap <- left_join(moose_202b, sap_clean, by = 'Ecoregion',
                       relatioship = "many-to-many")
sum_spe_browse<- moose_sap %>% 
  group_by(Species, Ecoregion)%>%
  summarise(BrowsingScore = mean(BrowsingScore),
            MoosseDensity = mean(MooseDensity))%>%
  print()
#According to the figure plot in Quesion 23:
#a) When the moose density increases, the browsing score of every species
#increases. This supports the researcher's hypothesis that when there is more
#competition, browsing is less selective.
#b) The most-browsed and therfore preferred species is the Willow, and the
#least-browsed and least favored is the Black Spruce.
#c) The Black Ash species is not present in the figure because it was only
#measured in the Western Forests and no other Ecoregion, so it would be unable
#to be measured in each Ecoregion and subject to sampling bias.
collisions2020<- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites<- c("North_Shore_Forests",
                "Northern_Peninsula_Forests",
                "Long_Range_Barrens",
                "Central_Forests",
                "Western_Forests",
                "EasternHyperOceanicBarrens",
                "Maritime_Barrens",
                "Avalon_Forests",
                "StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)
#*The rename_with function suggested was unusable; no .fn provided
coll_merge<- left_join(moose_2020, moose_coll2, by = 'Ecoregion',
                       relationship = "many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions2020, type = "p",
     xlab = "Moose Density",
     ylab = "Collisions in 2020",
     main = "Collisions in 2020 vs. Moose Density")
#According to the scatterplot, areas with higher moose densities saw an increase
#in collisions in 2020. There is one outlier of 100 collisions in an area of 
#1.0 density.
coll_merge_per_capita <- mutate(coll_merge,
                                coll_per_capita = collisions2020/human_pop)
plot(coll_merge_per_cpita$coll_per_capita, coll_merge_per_capita$human_pop,
     type = "p",
     xlab = "Collisions Per Capita",
     ylab = "Human Population",
     main = "Collisions Per Capita vs. Population")
#According to the scatterplot, there are more collisions per capita in areas
#with lower populations that are rural. Rural communities have higher moose
#populations and therefore have more collisions, consistent with the data.