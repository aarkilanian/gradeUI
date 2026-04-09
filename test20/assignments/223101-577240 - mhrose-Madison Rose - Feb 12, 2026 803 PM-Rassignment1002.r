# Madison Rose 202312939
# R Assignment 
# Biol 1002 


#Q2
moosedata <- read.csv("~/Downloads/MoosePopulation.csv")
#Q3
moose_clean <- na.omit(moosedata)
#Q4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Q5
Year <- c(2020,1992,1980,1960,1940,1904)

Estimated_Moose_Pop <- c(21740,32000,5300,41250,17000,2200,20450,4040,5,14000,21000,1540,27600,11650,1400,14500,5320,7400,15700,400,13200,3500,600,8300,200,10600,18000,1300,17210,14500,10300,200,3500,9600,11200,2300,4)
#A)
yearmin <- min(Year)
#1904

 #B)
moose_max <- max(Estimated_Moose_Pop)
# 41250

#Q6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland ecoregions over time")
#Q8 A)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
# B)
plot(moose_west$Year, moose_west$MooseDensity, type ="l",
     +      xlab = "year", 
     +      ylab = "Moose per sq km", 
     +      main = "Moose density in western Newfoundland over time")
#Q9
moose_2020 <- filter(moosedata2, Year == "2020")

moose_2020_high <-filter(moose_2020, MooseDensity > "2.0")

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
#Q10
moosefinal <- moosedata2 %>%
  +     filter(Year == 2020) %>%
  +     filter(MooseDensity > 2.0) %>%
  +     arrange(desc(MooseDensity)) %>%
  +     print()

# part 2 Sapling Study
#Q11
SaplingStudy <- read_csv("~/Downloads/SaplingStudy.csv")
 sap_clean <- na.omit(saplings)
#Q12
su
 avg_browse_reg <- group_by(sap_clean, Ecoregion) %>% 
   + summarise(MeanScore = mean(BrowsingScore)) %>%
   + arrange(desc(MeanScore))
 #lowest = 1, highest = 4.571429
#Q13
 sap_reg_height <- group_by(sap_clean, Ecoregion) %>% 
   + summarize(mean(Height))

  sap_reg_height_low <- sap_reg_height%>%filter(.data[["mean(Height)"]] <20) %>%
 + select(Ecoregion) %>%
   + print()
 #Q14
 sap_spe_browse <- group_by(sap_clean, Species) %>%
   + summarize(mean(BrowsingScore)) %>%
   + print()
 
 avg_browse_spe <- sap_spe_browse%>%
   + arrange(desc('mean(BrowsingScore)'))
 #Q15
 fir_reg_browse <- sap_clean%>%
   + filter(Species == "Balsam_Fir")%>%
   + group_by(Ecoregion)%>%
   + summarise(mean(BrowsingScore))
 #Q16 
 par(mar = c(12,5,4,2))
 barplot(fir_reg_browse$'mean(BrowsingScore)', names.arg = fir_reg_browse$Ecoregion, col = "green", cex.names = 0.5, ylab = "Mean Browsing Score", xlab = "Ecoregions", main = "Mean Browsing Scores of Balsam Fir Trees Based on Ecoregion")
 #Q17
 spruce_reg_browse <- sap_clean%>%
   + 
   + filter(Species == "Black_Spruce")%>%
   + group_by(Ecoregion)%>%
   + summarise(mean(BrowsingScore))%>%
   + print()
 barplot(spruce_reg_browse$'mean(BrowsingScore)', names.arg = spruce_reg_browse$Ecoreigon, col = "blue", cex.names = 0.5, xlab = "Ecoregions", ylab = "Mean Browsing Score", main = "Mean Browsing Scores of Spruce Trees in Different Ecoregions")
 # The balsam fir browsing score is larger in the northern and central ecoregions, the black spruce also has larger browsing numbers in northern, central, as well as western regions. The spruce has lower browsing scores for avalon and the barrens ecoregions. 
 #Q18'
 sap_reg_tally <- sap_clean %>%
   + group_by(Ecoregion)%>%
   + tally()%>%
   + print() 
 #Q19
 sap_spe_tally <- sap_clean %>%
   + group_by(Species)%>%
   + tally()%>%
   + print()
 # I noticed that northern and western regions had more trees documented, especially the northern peninsula/shore,than in eastern regions. I also noticed that black spruce and balsam fir were the most predominant species, which could cause the data to shift depend on where moose browse and what trees are preferred.
 # Bias can lead to over or under representation of a variable which can lead to skewed results that may not represent the true numbers. 
 
 #Q21
 moose_2020 <- filter (moose_clean, Year == "2020")
 moose_2020b <- mutate(moose_2020, MooseDensity = Estimated_Moose_Pop / Area)
 #Q22
 sum_spe_browse <- moose_sap %>%
   + group_by(Species, Ecoregion)%>%
   + summarise(mean_BrowsingScore = mean(BrowsingScore, na.rm = TRUE), mean_MooseDensity = mean(MooseDensity, na.rm = TRUE)) %>%
   + print()
 #Q23
 # A) When the moose density is low and there is less competition for food, the moose have a preference towards willow and alder trees and less preference to black spruce. When the moose density increases, the range of browsing scores narrows, meaning they have less preference when there is more competition for food. 
 # Therefore yes, moose have stronger preferences at lower densities. 
 # B) Moose favour willow and alder the most, and black spruce, white birch, and balsam fir the least. 
 # C) Black ash is not on the graph because there is not enough data to plot with there being only one recording. 
 #Q24
 collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
 human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
 study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
 
 moose_coll <- data.frame(collisions2020, human_pop, study_sites)
 #Q25
 moose_coll2 <- moose_coll %>%
   + rename(Ecoregion = study_sites)
 
 coll_merge <- left_join(moose_2020, moose_coll2)
 #Q26
 plot(coll_merge$Estimated_Moose_Pop, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Collisions", main = "Moose Density in Relation to Moose-Vehicle Collisons")
 #B) With higher moose density there are increased number of collisions. The outlier is the Avalon forests region, where there are only about 4000 moose yet over 100 collisions. This could be explained by the majority of the population/ majority of traffic taking place on the avalon. 
 #Q27
 coll_merge_per_capita <- mutate(coll_merge, coll_per_captira = collisions2020 / human_pop)
 #Q28
 plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_captira, xlab = "Human Population", ylab = "Collisions per Capita")
#Q29
# The highest amount of collisions are where there are a large population of moose paired with a large population of people. The avalon has a lot of people but not many moose, leading to a low collision to population ratio. Whereas the northern penisula has a relatively high population and a high moose population, leading to a high collisons per capita value. 
 