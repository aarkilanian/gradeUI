install.packages("dplyr") #Q1
library(dplyr)
moosedata <- read.csv(file = "MoosePopulation.csv") #Q2
View(moosedata)
Moosedatac <- na.omit(moosedata) #Q3
View(Moosedatac)
Moosedatas <- select(Moosedatac, Ecoregion, Year, Area, Estimated_Moose_Pop) #Q4
View(Moosedatas)
min(Moosedatas$Year) #1904 #Q5
max(Moosedatas$Estimated_Moose_Pop) #41250 #Q5
Moosedatad <- mutate(Moosedatas, Moosedensity = Estimated_Moose_Pop / Area) #Q6
View(Moosedatad)
plot(Moosedatad$Year, Moosedatad$Moosedensity, #Q7
     xlab = "Years",
     ylab = "Moose per sq Km",
     main = "Moose density in NL over time")
Moosedatawest <- filter(Moosedatad, Ecoregion == "Western_Forests") #Q8
View(Moosedatawest)
plot(Moosedatawest$Year, Moosedatawest$Moosedensity,
     xlab = "Years",
     ylab = "Moose per sq km",
     main = "Moose density in Western forest over time",
     type = "l") #Q8
Moosedata_2020 <- filter(Moosedatad, Year == "2020") #Q9
View(Moosedata_2020)
Moosedata_2020H <- filter(Moosedata_2020, Moosedensity >= "2.0") #Q9
View(Moosedata_2020H)
arrange(Moosedata_2020H, desc(Moosedensity)) #makes a table in console #Q9
MoosedataF <- Moosedatad %>% #Q10
  filter(Year =="2020") %>% 
  filter(Moosedensity > "2.0") %>%
  arrange(desc(Moosedensity)) %>%
  print() #Q10WellTheseThingsArePrettyNeatDon'tForgetAboutThese%>%connects lines toether
  
  Treesap <- read.csv(file = "SaplingStudy.csv") #Q11
  TreesapC <- na.omit(Treesap) #Q11
  Treesaps <- TreesapC %>% #Q12
    group_by(Ecoregion) %>%
    summarise(mean(BrowsingScore)) %>%
    print() #Q12 highest Northern_Peninsula_Forests, lowest StraitOfBelleIsleBarrens
  View(Treesaps)
  Treeheight <- TreesapC %>% #Q13
    group_by(Ecoregion) %>%
    summarise(mean(Height)) %>%
    print() #Q13 Severely browsed areas are Western forests and norther peninsula forest
  Treespecies <- TreesapC %>%
    group_by(Species) %>%
    summarise(mean(BrowsingScore)) %>%
    print() #Q14 Black_Ash highest browsing score Black_spruce lowesr browsing score
  TreeBal <- TreesapC %>% #Q15
    filter(Species == "Balsam_Fir") %>%
    group_by(Ecoregion) %>%
    summarise(mean(BrowsingScore)) %>%
    print() #Q15
  barplot(TreeBal$`mean(BrowsingScore)`, #Q16
          name.arg = TreeBal$Ecoregion,
          xlab = "Ecoregions",
          ylab = "Average Browsing score",
          main = "Moose Grazing score in different ecosystems",
          col = "darkgreen") #Q16
  Treespru <- TreesapC %>% #Q17
    filter(Species == "Black_Spruce") %>%
    group_by(Ecoregion) %>%
    summarise(mean(BrowsingScore)) %>%
    print() #Q17
  barplot(Treespru$`mean(BrowsingScore)`, #Q17
          name.arg = Treespru$Ecoregion,
          xlab = "Ecoregion",
          ylab = "Average Browsing score",
          main = "Moosegrazing score of black spruce in different ecosystem") #Q17
  #Q17 the balsam fir browsing score is higher accros the bord than the spruce browsing score suggesting the moose prefer the balsam fir than the Black spruce
    Ecotally<- TreesapC %>% #Q18
      group_by(Ecoregion) %>%
      tally() %>%
      print() #Q18 no same number trees not counted in eaach ecosystem
    Ecotallys <- TreesapC %>% #Q19 
      group_by(Species) %>%
      tally() %>%
      print() 
    #Q19 again no not a equal count of species
    #Q20 The data wasn't evenly distributed amount ecosystem like straitofbelleislebarrens
    #only being represented one which would give innacurate data with some areas being over
    #represented while some areas where under represented. It's important to be aware of this
    #bias in the data set since if the data gets applied to a wide scope without knowledge of
    #the innacurate distribution of data points sertain areas will be under and overrepresented
    #giving a false narative the data will portrait and if this was going to be offered up as accurate
    #data for a project you can see the issues this would have due to misrepresentation of where the moose
    #are and what plants they are eating.
    
    moose2020B <- Moosedatas%>% #Q21
      filter(Year == "2020") %>%
      mutate(Moosedensity = Estimated_Moose_Pop/ Area) %>%
      print() #Q21
    Moosesap <- left_join(moose2020B, TreesapC, by = 'Ecoregion', relationship = "many-to-many") #Q21
    
    Moosesapavg <- Moosesap %>% #Q22
      group_by(Ecoregion, Species) %>%
      summarise( meanbrowsing = mean(BrowsingScore),
                 meandensity = mean(Moosedensity))%>%
      print() #Q22
    
    install.packages("ggplot2") #Q23
    library(ggplot2)
    ggplot(Moosesapavg, aes(x = meanbrowsing, y = meandensity, color= Species)) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = "Browsing intensity across moose density and species",
           x = "Average moose density",
           y = "Average Browsing score")
    #Q23a) The researchers hypothesis is supported since in high density browsing scored overall increase
    #while in low browsing score areas only a few specific plants are comused probably the most palatable to the moose.
    #b) The moose intensely browsed plant is Black_Spruce the least intensely browsed plant is Willow
    #c) Black_Ash isn't on the graph since it isn't represented in any data from 2020
    
    collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6) #Q24
    human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
    study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
    
    moosehit <- data.frame(collisions2020, human_pop, study_sites) #Q25
    moosehit2 <- moosehit%>%
      rename_with(~ "Ecoregion", study_sites)
  
    moosecollmerge <- left_join(moosehit2, moose2020B, by = 'Ecoregion', relationship = "many-to-many") #Q25
    
    plot(moosecollmerge$Moosedensity, moosecollmerge$collisions2020,#Q26
         xlab = "Moosedensity",
         ylab = "moosecollisions",
         main = "moose collisions compared to density") #Q26 there is a general trend of 
    #as moose density increases the moose collisions also increase although there
    #is one outlier to this trend being on 1.0 moose density with over 100 moose
    #collisions suggesting a data error or some unknow factor playing a role.
    
    moosecollmergeperccap <- mutate(moosecollmerge, collpercapita = collisions2020/human_pop) #Q27
    View(moosecollmergeperccap) #Q27 	Northern_peninsula forest then central forest are the two highest collsions per capita
    
    plot(moosecollmergeperccap$collpercapita, moosecollmergeperccap$human_pop, #Q28
         xlab = "Moose collisions per capita",
         ylab = "human population",
         main = "Human population vesus Moose collisions per capita graph") #Q28
    
    #Q29 based off this graph as the human population decreases the collion rate per
    #capita also decreases which makes sense since if there are less people around
    #driving there is less moose collisions that can occur which is why the graph.
    
                         
      
    
  
  


  
              
