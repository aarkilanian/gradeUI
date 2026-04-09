#Question1
install.packages("dplyr")
#Question2
> MoosePopulation <- read_csv("MoosePopulation.csv")
#Question3
> moose_clean <- na.omit(MoosePopulation)
#Question4
> moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
#Question5a
> year_min <- min(1904)
#Question5b
> moose_max <- max(41250)
#Question6
> moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Question7a
> plot(moosedata2$Year, moosedata2$MooseDensity, 
       +      xlab = "year", 
       +      ylab = "Moose per sq km", 
       +      main = "Moose density in Newfoundland ecoregions over time")
#Question8a
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
#Quesiton8b
> plot(moose_west$Year,
       +     moose_west$Density,
       +     type = "l",
       +     xlab = "Moose Density",
       +     ylab = "Year",
       +     main = "Change in Moose Density Over Time in Western Forests")
#Question9a
> moose_2020 <- filter(moosedata2, Year == "2020")
#Question9b
> moose_2020_high <-filter(moosedata2, MooseDensity == "")
> #Question9c
  > moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))
  > #Question10
    > moosefinal <-moosedata2 %>%
      + moosefinal <- moosedata2 %>%
        +     filter(Year == 2020) %>%
        +     filter(MooseDensity > 2.0) %>%
        +     arrange(desc(MooseDensity)) %>%
        +     print()
#Question11a
      saplings<-read.csv("SaplingStudy.csv")
#Question11b
      sap_clean<-na.omit(saplings)
      #Question 12a
      sap_reg_browse <- sap_clean %>%
        + group_by(Ecoregion) %>%
        + summarize(AverageBrowsing=mean(BrowsingScore)) %>%
        + print()
      #Question12b
      avg_browse_reg<- arrange(sap_reg_browse,desc(AverageBrowsing))
      #The ecoregion with the average highest score is Northern_Peninsula_Forests
#Question13a 
      sap_reg_height<-sap_clean %>%
        + group_by(Ecoregion) %>%
        + summarize(AverageHeight=mean(Height)) %>%
        + print()
#Question13b
      #The ones under 20 are the Northern_Peninsula_Forests and the Western_Forests
      sap_reg_height<-sap_clean %>%
        + group_by(Ecoregion) %>%
        + summarize(AverageHeight=mean(Height)) %>%
        + print()
#Question14a
      > sap_reg_height<-sap_clean %>%
        + group_by(Ecoregion) %>%
        + summarize(AverageHeight=mean(Height)) %>%
        + print()
#Question14b
      avg_browse_spe <- arrange(sap_spe_browse,desc(AverageBrowsing))
      #The lowest is Black Ash and the highest is Black Spruce
#Question15
      > fir_reg_browse<- sap_clean %>%
        +     filter(Species=="Balsam_Fir") %>%
        +     group_by(Ecoregion) %>%
        +     summarize(MeanBrowseScore=mean(BrowsingScore)) %>%
        +     print()
#Question16
      >> barplot(
        +     fir_reg_browse$MeanBrowseScore,
        +     names.arg =fir_reg_browse$Ecoregion,
        +     xlab = "Ecoregion",
        +     ylab = "Average Browsing Intensity",
        +     main = "Average Balsam Fir Browsing Intensity by Ecoregion",
        +     col = "pink",
        +     cex.names = 0.6,)
#Question17a
      > spruce_reg_browse<- sap_clean %>%
        + filter(Species=="Black_Spruce")%>%
        + group_by(Ecoregion) %>%
        + summarize(MeanBrowseScore=mean(BrowsingScore))%>%
        + print()
#Question17b
      > barplot(
        +     spruce_reg_browse$MeanBrowseScore,
        +     names.arg = spruce_reg_browse$Ecoregion,
        +     xlab = "Ecoregion",
        +     ylab = "Average Browsing Intensity",
        +     main = "Average Black Spruce Browsing Intensity by Ecoregion",
        +     col = "mediumorchid1",
        +     cex.names = 0.6,)
#Question17c
      # Black Spruce tends to have lower browsing intensity than Balsam Fir in most ecoregions,which means moose generally prefer Balsam Fir. Both species still show similar trends across ecoregions, with some areas having higher browsing than others.
#Question18
      sap_reg_tally<- sap_clean %>%
        group_by(Ecoregion) %>%
        tally() %>% 
        print()
#Question19
      > sap_spe_tally<- sap_clean %>%
        +     group_by(Species) %>%
        +     tally() %>% 
        +     print()
#Question20a
      # The SamplingStudy dataset is not evenly distributed, as some ecoregions and tree species have many more observations than others.
#Question20b
      # Being able to recognize bias in ecological datasets is important because uneven sampling can lead to 
      #inaccurate results about patterns in nature. If some areas or species are sampled more than others, the 
      #results may not accurately represent the whole system.
#Question21a
      moose_2020b<- mutate(MoosePopulation)
#Question21b
      moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
#Question22
      > sum_reg_tally<- moose_sap%>%
        + group_by(Species,Ecoregion) %>%
        + summarize(AverageBrowsingScore=mean(BrowsingScore)) %>%
        + print()
#Question23a
      #At low moose densities (0–1), the browsing scores are widely spread, with moose 
      #targeting species like Willow and Alder while ignoring others like Black Spruce. 
      #As density increases toward 2.5, almost all species converge toward the top of the scale (scores 4–5),
      #indicating that moose become less picky and browse everything available.
#Question23b
      #Moose favor Willow and Alder the most, while they browse Black Spruce the least. Willow and Alder 
      #maintain high browsing scores even at lower moose densities, whereas Black Spruce scores remain near zero
#Question23c
      #Black Ash is not shown on the figure because there is no data plotted for it. While it is listed in the legend, 
      #there are no green dots present on the scatterplot, likely because Black Ash was either unavailable in the study 
      #plots or was not browsed at all during the observation period.
#Question24
      collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
      human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
      study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
      
      moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Question25a
      moose_coll2 <- moose_coll %>% 
        +     rename(Ecoregion = study_sites)
#Question25b 
      coll_merge <- left_join(moose_coll2, moose_2020)
#Question26a
      plot(coll_merge$MooseDensity, coll_merge$collisions2020)
#Question26b
      #There is one outlier, where there is a high density but virtually 0 crashes
#Question27
      coll_merge_per_capita <- coll_merge %>% 
        mutate(coll_per_capita = collisions2020 / human_pop)
#Question28
      plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita)
#Question29
      #In Newfoundland, theres a negative trend. This is because higher human populations have more infrastructure 
      #and fewer moose wandering the streets compared to rural ecoregions where humans are sparse but moose are 
      #everywhere.
      
      

      