#Emma Neville Biology 1002 

#1----------------
library(dplyr)
#2----------------
moosedata <- read.csv("~/Downloads/MoosePopulation.csv")
#3----------------
View(moosedata)
moose_clean <- na.omit(moosedata)
#4----------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#5----------------
year_min <- min(moose_sel$Year)
# a) 1904

moose_max <- max(moose_sel$Estimated_Moose_Pop)
# b) 41250
#6---------------
moosedata2 <- mutate(moose_sel, MooseDensity = 
                       Estimated_Moose_Pop  / Area)

#7--------------
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
#8--------------
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity, 
        type = "l",
        xlab = "year", 
        ylab = "Moose per sq km", 
        main = "Moose density in Western Forests region over time")
#9--------------
moose_2020 <- filter(moosedata2, Year == 2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#10------------
#using pipes.

moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


#Part 2

#Part 2
#Part II
#11------------
saplings <- read.csv("~/Downloads/SaplingStudy.csv")

sap_clean <- na.omit (saplings)

#12--------------
sap_reg_browse <- sap_clean%>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore = mean(BrowsingScore))%>%
  print()
  
avg_browse_reg <- sap_reg_browse%>%
  arrange(desc(BrowsingScore))%>%
  print()

#highest: 
  #Northern_Penninsula_Forests

#lowest: 
  #StraitOfBelleIsleBarrens
#13--------------
sap_reg_height <- sap_clean
  group_by(ecoregion)%>%
  summarize(mean(Height))%>%
  print()
  
sap_reg_height_low <- sap_reg_height%>%
  filter(Height < 20)%>%
  print()
#Average Heights < 20
  #North_Shore_Forests(x2)
  #Northern_Peninsula_Forests(x3)
  #Central_Forests(x2)
  #Western_Forests(x2)

#14-------------
sap_spe_browse <- sap_clean%>%
  group_by(Species)%>%
  summarize(BrowsingScore = mean(BrowsingScore))%>%
  print()

avg_browse_spe <- sap_spe_browse%>%
  arrange(desc(BrowsingScore))
  print()

#highest
  #Black_Ash
  
#lowest
  #Black_Spruce
#15-------------
fir_reg_browse <- sap_clean%>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarize(BrowsingScore = mean(BrowsingScore))%>%
  print()

#16---------------
barplot(fir_reg_browse$BrowsingScore, 
          names.arg = fir_reg_browse$Ecoregion, 
          xlab = "Ecoregion", 
          ylab = "Average Balsam Fir Browsing Score", 
          main = "Average Balsam Fir Browsing Score by Ecoregion",
          col = "pink",
          cex.names = 0.6)

#17--------------
spruce_reg_browse <- sap_clean%>%
    filter(Species == "Black_Spruce")%>%
    group_by(Ecoregion)%>%
    summarize(BrowsingScore = mean(BrowsingScore))%>%
    print()

barplot(spruce_reg_browse$BrowsingScore, 
          names.arg = spruce_reg_browse$Ecoregion, 
          xlab = "Ecoregion", 
          ylab = "Average Black Spruce Browsing Score", 
          main = "Average Black Spruce Browsing Score by Ecoregion",
          col = "green",
          cex.names = 0.6)

#Compared to Balsam Fir, both trees are found in the Avalon and Long_Range 
 #forests, however, Black spruce has a much lower Browsing Score 
 #in the Long_Range_Barrens Ecoregion. Black Sprunce is also seen in the
 #Western_Forest Ecoregion while Balsam Fir is seen in the Northern_
 #Penninsula_Forests. 
  
#18--------------
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#While some were equal (e.g. Avalon and Central), the same number of trees 
 #were not counted in every Ecoregion ranging from 1(Belle Isle) to 
 #8(North Shore). 

#19--------------
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#No, the same number of tree saplings for every species was not equal, 
 #wide range 1(Black Ash) to 11(Balsam Fir).
#
#20--------------
#a) I think the Sapling Study is almost evenly distributed. It is very easy to 
  #see in the data set which ecoregions were studied the most (e.g. North Shore,
  #Northern Penninsula, etc.) and which were examined the least, The Strait of 
  #Belle Isle which also only presented one type of species, the Willow Tree. 
  #Yes, some places are more plentiful in different types of trees and large 
  #amounts of them but only presenting one variable in a ecoregion compared to
  #eight definitly presented bias towards ecoregion sampling.

#b) Bias recoongnition is important for a researcher to notice because uneven 
  #sampling can cause imformation to be misleading. For, example over or 
  #underrepresented ecoregions can alter numerical conclusions of which regions 
  #are heavily browsed and which are not. 


#Part III
#21---------------
moose_2020b <- moose_clean%>%
  filter(Year == 2020)%>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)%>%
  print()

moose_sap <- left_join(moose_2020b, sap_clean, 
  by = 'Ecoregion', relationship = "many-to-many")
#22--------------
sum_spe_browse <- moose_sap%>%
  group_by(Ecoregion, Species) %>%
  summarize(BrowsingScore = mean(BrowsingScore), MooseDensity = mean(MooseDensity))%>%
  print()

#23-------------
library(ggplot2)

ggplot(sum_spe_browse, aes(x = MooseDensity, y = BrowsingScore, color= Species)) +
  geom_point(size = 3) + theme_minimal() + 
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#a) The plot mostly supports the hypothesis: In relation to browsing, higher 
  #moose density the plots are closer together (supports generalist idea) and 
  #in lower moose density the plots are more spread out (supporting preference 
  #idea)

#b) Moose favour Willow the most (Pink plots) and it very evident among all 
  #moose density's and browsing scores.Black Spruce on the other hand is ploted 
  #the least (teal plots) with much lower browsing scores, low preference.

#c) The Black Ash species is not shown in this figure probably because of how 
  #low of an amount (zero) the data presented in the original data set, 
  #it would be useless to show it as a point.
#24------------------
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,2300)

study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", 
                 "Long_Range_Barrens","Central_Forests","Western_Forests",
                 "EasternHyperOceanicBarrens","Maritime_Barrens",
                 "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#25-----------------
moose_coll2 <- rename_with(moose_coll, ~ "Ecoregion", .cols = study_sites)

coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")


#26-----------------
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density (moose/sq km)", 
     ylab = "Number of Collisions", 
     main = "Scatter Plot of Moose Density vs Collisions",
     pch = 19,
     col = "purple")

#There is a positive relationship, as the moose density begins t increase the 
  #number of colisions begin to increase. However, there is one point that 
  #sticks out with a very high number of collisions, compared to the more 
  #moderate numbers surrounding it. This could suggest that there are other 
  #factors that play into collision numbers. 
#27-------------
coll_merge_per_capita <- coll_merge%>%
  mutate(coll_per_capita = collisions2020  / human_pop)




#28----------------
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop,
    xlab = "Human Population", 
    ylab = "Collisions per Capita", 
    main = "Scatter Plot of Collisions per Capita vs Human Population",
    pch = 19,
    col = "orange")

#29-------------

#This plot shows that collisions per capita are extremely high in areas with
  #low human populations and the opposite is true in high populations of humans.
  #This trend does make sense because Newfoundland is not as heavily populated 
  #as some of the bigger cities but even in highly populated areas with lower
  #collision rates the numbers per capita are still very high. Whereas there
  #are a lot less moose than humans in Newfoundland so their number of 
  #collisions are a lot lower. 
