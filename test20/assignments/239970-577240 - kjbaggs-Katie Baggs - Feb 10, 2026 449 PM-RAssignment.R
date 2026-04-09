#Katie Baggs
#Bio R Assignment
#February 10th, 2026

#QUESTION #1
install.packages("dplyr")
"dplyr"
library("dplyr")

#QUESTION #2
moosedata <- read.csv("MoosePopulation.csv")
moosedata<-MoosePopulation

#QUESTION #3
moose_clean <- na.omit(moosedata)

#QUESTION #4
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#QUESTION #5A
year_min<-min(moose_sel$Year)


#QUESTION #5B
moose_max<-max(moose_sel$Estimated_Moose_Pop)

#QUESTION #6
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#QUESTION #7 
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question #8A
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Question #8B
plot(moosedata2$Year, moosedata2$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Western Forests ecoregion over time")
#Question #9A
 moose_2020 <- filter(moosedata2, Year == "2020")
#Question #9B
 moose_2020_high <-filter(moosedata2, MooseDensity == "")
#Question #9C
 moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))

 #Question #10
 moosefinal <- moosedata2 %>%
   filter(Year == 2020) %>%
   filter(MooseDensity > 2.0) %>%
   arrange(desc(MooseDensity)) %>%
   print()

 #Question #11A
 saplings<-read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
 #Question #11B
 View (saplings)
 na.omit(saplings)
 sap_clean <- na.omit(saplings)
 
 #Question #12A
 sap_reg_browse <- sap_clean %>%
    group_by(Ecoregion) %>%
    summarize(AverageBrowsing=mean(BrowsingScore)) %>%
   print()
 
#Question #12B
 avg_browse_reg<- arrange(sap_reg_browse,desc(AverageBrowsing))
 #The ecoregion with the average highest score is Northern_Peninsula_Forests

 #Question #13A
 sap_reg_height<-sap_clean %>%
   group_by(Ecoregion) %>%
   summarize(AverageHeight=mean(Height)) %>%
    print()
#Question #13B
 #The Northern Peninsula Forests and the Western Forests are the ecoregions that have average heights less than 20cm
 sap_reg_height<-sap_clean %>%
    group_by(Ecoregion) %>%
   summarize(AverageHeight=mean(Height)) %>%
    print()
 #Question #14A 
 sap_reg_height<-sap_clean %>%
    group_by(Ecoregion) %>%
    summarize(AverageHeight=mean(Height)) %>%
    print()
#Question #14B
 avg_browse_spe <- arrange(sap_spe_browse,desc(AverageBrowsing))
 #The lowest is Black Ash and the highest is Black Spruce
 
 #Question #15
  fir_reg_browse<- sap_clean %>%
        filter(Species=="Balsam_Fir") %>%
        group_by(Ecoregion) %>%
        summarize(MeanBrowseScore=mean(BrowsingScore)) %>%
        print()
#Question #16
   barplot(
         fir_reg_browse$MeanBrowseScore,
         names.arg =fir_reg_browse$Ecoregion,
         xlab = "Ecoregion",
         ylab = "Average Browsing Intensity",
         main = "Average Balsam Fir Browsing Intensity by Ecoregion",
         col = "blue",
         cex.names = 0.6,)
#Question #17A
   spruce_reg_browse<- sap_clean %>%
     filter(Species=="Black_Spruce")%>%
      group_by(Ecoregion) %>%
      summarize(MeanBrowseScore=mean(BrowsingScore))%>%
      print()
#Question #17B
   barplot(
          spruce_reg_browse$MeanBrowseScore,
          names.arg = spruce_reg_browse$Ecoregion,
          xlab = "Ecoregion",
          ylab = "Average Browsing Intensity",
          main = "Average Black Spruce Browsing Intensity by Ecoregion",
          col = "Medium Sea Green",
          cex.names = 0.6,)
#Question#17C
# Black spruce has a lower browsing intensity than Balsam Fir.This means that moose would perfer Balsam Fir

#Question #18
   sap_reg_tally<- sap_clean %>%
     group_by(Ecoregion) %>%
     tally() %>% 
     print()
#Question #19
   sap_spe_tally<- sap_clean %>%
          group_by(Species) %>%
          tally() %>% 
          print()
#Question #20A
#I think the Sapling Study data sets is not evenly distributed. 
#Question #20B
#It is important to recognize bias in ecological data sets because uneven sampling can lead to incorrect conclusions about ecological patterns.

#Question #21A
   moose_2020b<- mutate(MoosePopulation)
#Question #21B
   moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#Question #22 
    sum_reg_tally<- moose_sap%>%
      group_by(Species,Ecoregion) %>%
      summarize(AverageBrowsingScore=mean(BrowsingScore)) %>%
      print()
#Question #23A
#Yes, the figure supports the researchers hypothesis. At low moose density, browsing intensity varies among species. While at high moose density, browsing intensity increases, shifting toward generalist browsing.
#Question #23B
#Moose favors Willow and Alder the most and Black spruce are browsed the least.
#Question #23C
#Black Ash is not clearly shown on the figure because there were not enough observations to calculate data.

#Question #24
    collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
    human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
    study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
    
    moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question #25A
    moose_coll2 <- moose_coll %>% 
           rename(Ecoregion = study_sites)
#Question #25B
    coll_merge <- left_join(moose_coll2, moose_2020)
#Question #26A
    plot(coll_merge$MooseDensity, coll_merge$collisions2020)
#Question #26B
#The graph shows a positive relationship between moose density and the number of moose-vehicle collisions. There is one clear outlier where there is low moose density and a very high number of collisions. 

#Question #27
    coll_merge_per_capita <- coll_merge %>% 
      mutate(coll_per_capita = collisions2020 / human_pop)

#Question #28
    plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita)

#Question #29
#The graph shows a negative relationship between human population size and moose collisions. This trend makes sense because rural areas in Newfoundland have fewer people but high moose densities and more driving on highways, increasing the collision risk per person.
 
 