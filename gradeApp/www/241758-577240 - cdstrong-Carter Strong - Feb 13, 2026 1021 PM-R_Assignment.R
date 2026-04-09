library(readr)

Q2:  moosedata <- read.csv("5936993MoosePopulation.csv")

Q3: moose_clean <- na.omit(moosedata)

Q4: moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

Q5:
  A) year_min <- min(moose_sel$Year, na.rm = TRUE)
  
  B) moose_max <- max(moose_sel$Estimated_Moose_Pop, na.rm = TRUE)
  
  Q6: moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
  
  Q7: plot(moosedata2$Year, moosedata2$MooseDensity, 
           +      xlab = "year", 
           +      ylab = "Moose per sq km", 
           +      main = "Moose density in Newfoundland ecoregions over time")
  
  Q8:
    A) moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
    
    B) plot(moose_west$Year, moose_west$MooseDensity,
            +      type = “l”,
            +      xlab = "year", 
            +      ylab = "Moose per sq km"
            +      main = “Moose density in Western Forests over time”)

Q9:
  A)moose_2020 <- filter(moosedata2, Year == 2020)
  
  B) moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
  
  C) moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
  
  Q10: moosefinal <- moosedata2 %>%
    +     filter(Year == 2020) %>%
    +     filter(MooseDensity > 2.0) %>%
    +     arrange(desc(MooseDensity)) %>%
    +     print()
  
  Q11:
    A) saplings <- read.csv("5936994SaplingStudy.csv")
    
    B) sap_clean <- na.omit(saplings)
    
    Q12:
      A) sap_reg_browse <- sap_clean %>%
        +     group_by(Ecoregion) %>%
        +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
        +     print()
      
      B) avg_browse_reg <- sap_reg_browse %>%
        +     arrange(desc(AverageBrowsing))
      > 
        > # Highest browsing is in the first row
        > # Lowest browsing found in last row
        
        Q13:
        A) sap_reg_height <- sap_clean %>%
          +     group_by(Ecoregion) %>%
          +     summarize(AverageHeight = mean(Height)) %>%
          +     print()
        
        B) sap_reg_height_low <- sap_reg_height %>%
          +     filter(AverageHeight < 20) %>%
          +     print()
        >
          > # These ecoregions have an average sapling heigh of below 20 cm, which suggests that large amounts of browsing have likely occurred.
          
          Q14:
          A) sap_spe_browse <- sap_clean %>%
            +     group_by(Species) %>%
            +     summarize(AverageBrowsing = mean(BrowsingScore)) %>%
            +     print()
          
          B) avg_browse_spe <- sap_spe_browse %>%
            +     arrange(desc(AverageBrowsing))
          > 
            > # Species with highest browsing in first row
            > # Species with lowest browsing in last row
            
            Q15: fir_reg_browse <- sap_clean %>%
              +     filter(Species == "Balsam_Fir") %>%
              +     group_by(Ecoregion) %>%
              +     summarize(AverageBrowsing = mean(BrowsingScore))
            
            Q16: barplot(firs_reg_browse$AverageBrowsing,
                         +         names.arg = fir_reg_browse$Ecoregion,
                         +         xlab = "Ecoregion",
                         +         ylab = "Average browsing intensity",
                         +         main = "Balsam Fir browsing intensity by ecoregion",
                         +         col = "forestgreen",
                         +         cex.names = 0.6)
            
            Q17:
              A) spruce_reg_browse <- sap_clean %>%
                +     filter(Species == "Black_Spruce") %>%
                +     group_by(Ecoregion) %>%
                +     summarize(AverageBrowsing = mean(BrowsingScore))
              
              B) barplot(spruce_reg_browse$AverageBrowsing,
                         +         names.arg = spruce_reg_browse$Ecoregion,
                         +         xlab = "Ecoregion",
                         +         ylab = "Average browsing intensity",
                         +         main = "Black Spruce browsing intensity by ecoregion",
                         +         col = "darkolivegreen3",
                         +         cex.names = 0.6)

C)  # Black spruce mostly show lower browsing intensity than the Balsam Fir, # telling us that moose likely prefer browsing Balsam Fir.

Q18: sap_reg_tally <- sap_clean %>%
  +     group_by(Ecoregion) %>%
  +     tally() %>%
  +     print()

Q19: sap_reg_tally <- sap_clean %>%
  +     group_by(Species) %>%
  +     tally() %>%
  +     print()

Q20:
  A)	# I would say that this SaplingStudy dataset is for the most part evenly distributed, aside from the Strait of Belle Isle Barrens which only has one piece of data, all the others have 5 or 4. North Shore Forests and Northern Peninsula Forests are a little overrepresented with 8 and 7 data points respectively.

B)	# Recognizing bias is important in this type of ecological dataset mainly because uneven sampling can manipulate ecological conclusions. If some species or regions are sampled more heavily, results can reflect sampling effort rather than actual ecological patterns.

Q21:
  A) moose_2020b <- moose_clean %>%
    +     filter(Year == 2020) %>%
    +     mutate(MooseDensity = Estimated_Moose_Pop / Area)
  
  B)	moose_sap <- left_join(moose_2020b, sap_clean, by = “Ecoregion”)
  
  Q22: sum_spe_browse <- moose_sap %>%
    +     group_by(Species, Ecoregion) %>%
    +     summarize(
      +         AvgBrowsing = mean(BrowsingScore),
      +         AvgMooseDensity = mean(MooseDensity)
      +     ) %>%
    +     print()
  
  
  
  Q23:
    A)	The above figure does show evidence that supports the researcher's hypothesis. One example is that at lower moose densities, that data shows that browsing is more focused on certain species of tree, while at higher densities browsing rates are more similar across every species.

B)	Moose favour mainly willows, and to a lesser degree alders, while the least browsed species is the black spruce. One thing I find interesting is that despite having a spot on the legend, the black ash doesn’t appear at all in the figure.

C)	The black ash is not shown on the figure, this is likely because in the dataset they were only present in one data point, in the Western Forests, which is likely not enough to be plotted on the graph.

Q24: moose_coll <- data.frame(collisions2020, human_pop, study_sites)

Q25:
A) moose_coll2 <- moose_coll %>%
+     rename(Ecoregion = study_sites)

B) coll_merge <- left_join(moose_2020, moose_coll2, by = “Ecoregion”)
Q26:
A) plot(coll_merge$MooseDensity, coll_merge$collisions2020,,
+         xlab = "Moose Density",
+         ylab = "Moose-Vehicle Collisions (2020)",
+         main = "Relationship between Moose Density and Collisions")

B)Collisions with moose increase as moose density increases, which does make perfect sense as more moose equals more opportunities for collisions to happen. There are a few outliers, potentially suggesting conditions such as geography or artificial road infrastructure could play into these results.

Q27: plot(coll_merge_per_capita <- coll_merge %>%
+     mutate(coll_per_capita = collisions2020 / human_pop)

Q28: plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
+         xlab = "Human Population",
+         ylab = "Collisions Per Person",
+         main = "Relationship between Collisions Per Capita and Human Population")

Q29: Collisions per capita are higher in regions with lower human populations, which at first glance appears to be odd. However, this makes sense if you consider that in higher populated areas, there are more likely to be protective/preventative measures to keep moose away from roads, whereas in more rural/less populated areas these are likely to be less commonplace. 
+
