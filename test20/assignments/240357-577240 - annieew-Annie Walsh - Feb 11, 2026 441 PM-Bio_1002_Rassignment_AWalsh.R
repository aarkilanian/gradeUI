#Title: My R script for Bio 1002
#Author: Annie Walsh
#Date: 06-02-2026

#Package needed
install.packages("tidyverse")

#Part I: Moose Populations in Newfoundland
#Question 1: Set up functions
library("dplyr")

#Question 2: Import data
moosedata <- read.csv("MoosePopulation.csv")

#Question 3: Analyze data
View("MoosePopulation.csv")

#Omitt missing vaules
na.omit(18,27,33,36,39,42,43,44,45,46,47,48,49,51,52,53,54)

moose_clean <- na.omit(moosedata)

#Question4: Simplfy dataset 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5: Find min and max
year_min <- min(moose_sel$Year)

moose_max <- max(moose_sel$Estimated_Moose_Pop)

#Question 6: Standardize data
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7: Plot data
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question 8: Create new dataset
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l",
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

#Question 9: Filter and arrange data
moose_2020 <- filter(moosedata2, Year == "2020")

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Question 10: Pipes
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#Part II: Tree Sapling Study
#Question 11: Load data 
saplings <- read.csv("SaplingStudy.csv")

sap_clean <- na.omit(saplings)

#Question 12: Moose browsing
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
  print(sap_reg_browse)

  avg_browse_reg <- sap_reg_browse %>%
    arrange(desc(AverageBrowsing))
    print(avg_browse_reg)
  
# Highest average browsing: first row of avg_browse_reg
# Lowest average browsing: last row of avg_browse_reg
  

#Question 13: Average Tree Height
    sap_reg_height <- sap_clean %>%
      group_by(Ecoregion) %>%
      summarize(AverageHeight = mean(Height))
      print(sap_reg_height)
    
    sap_reg_height_low <- sap_reg_height %>%
        filter(AverageHeight < 20)
     print(sap_reg_height_low)
      
# Ecoregions listed here have average tree heights less than 20 cm
# and are considered severely browsed by moose
      
    
#Question 14: Average browsing score
     sap_spe_browse <- sap_clean %>%
       group_by(Species) %>%
       summarize(AverageBrowsing = mean(BrowsingScore))
       print(sap_spe_browse)
     
       avg_browse_spe <- sap_spe_browse %>%
         arrange(desc(AverageBrowsing))
        print(avg_browse_spe)
       
# Species at the top has the highest average browsing score
# Species at the bottom has the lowest average browsing score
       
    
#Question 15: Balsam fur browsing
  fir_reg_browse <- sap_clean %>%
      filter(Species == "Balsam_Fir") %>%
        group_by(Ecoregion) %>%
        summarize(AverageBrowsing = mean(BrowsingScore))
        print(fir_reg_browse)
        
#Question 16: Barplot
        barplot(
          fir_reg_browse$AverageBrowsing,
          names.arg = fir_reg_browse$Ecoregion,
          xlab = "Ecoregion",
          ylab = "Average Browsing Intensity",
          main = "Average Moose Browsing on Balsam Fir by Ecoregion",
          col = "forestgreen",
          cex.names = 0.6)
        
#Question 17: Black spruce browsing
        spruce_reg_browse <- sap_clean %>%
          filter(Species == "Black_Spruce") %>%
          group_by(Ecoregion) %>%
          summarize(AverageBrowsing = mean(BrowsingScore))
          print(spruce_reg_browse)
        
          barplot(
            spruce_reg_browse$AverageBrowsing,
            names.arg = spruce_reg_browse$Ecoregion,
            xlab = "Ecoregion",
            ylab = "Average Browsing Intensity",
            main = "Average Moose Browsing on Black Spruce by Ecoregion",
            col = "darkgreen",
            cex.names = 0.6)
          
# Black Spruce generally shows different browsing patterns than Balsam Fir across ecoregions,
# with some regions experiencing higher average browsing on one species than the other.
          
        
#Question 18: Group saplings by ecoregion
  sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Question 19: Tally saplings
sap_reg_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Question 20: Sampling bias
# The SaplingStudy dataset does not appear to be evenly distributed, as some ecoregions
# and tree species have higher sample counts than others, indicating potential
# overrepresentation and underrepresentation in the dataset.

# Recognizing bias in ecological datasets is important because uneven sampling can
# lead to misleading conclusions about species behavior and ecosystem patterns.
# Bias can affect how accurately results reflect real-world conditions.

#Part III: Creating/Joining Datasets
#Question 21: Filter data 
filter(moose_clean, Year == "2020")

moose_2020b <- mutate(moose_clean, MooseDensity = Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sap_clean, 
          by = 'Ecoregion', relationship = "many-to-many")


#Question 22: Average brwosing score
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(
    AvgBrowsing = mean(BrowsingScore),
    AvgDensity = mean(MooseDensity)
  )

print(sum_spe_browse)


#Question 23: Average browsing and moose density graph

#a
# The figure provides evidence supporting the researchers’ hypothesis, as browsing intensity
# appears more selective at low moose densities and becomes more uniform across species
# at higher moose densities.

#b
# Moose appear to favour Willow and Alder most, as they consistently show higher average
# browsing scores, while Black Spruce and Black Ash tend to have lower browsing intensity.

#c
# Black Ash is not shown across all density levels, likely due to limited data or
# absence in certain ecoregions, preventing calculation of average browsing values.


#Question 24: Vectors and new dataset
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25: Join datasets 
moose_coll2 <- rename_with(moose_coll, ~"Ecoregion", study_sites)

  coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
  
#Question 26: Plot and trends
  plot(
    coll_merge$MooseDensity,
    coll_merge$collisions2020,
    xlab = "Moose Density",
    ylab = "Moose-Vehicle Collisions (2020)",
    main = "Moose Density vs Moose-Vehicle Collisions"
  )
  
# There is a positive relationship between moose density and the number of collisions,
# with some ecoregions showing higher collision rates than expected, suggesting outliers.
  
  
#Question 27: Create new column
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
  
#Question 28: Plot
  plot(
    coll_merge_per_capita$human_pop,
    coll_merge_per_capita$coll_per_capita,
    xlab = "Human Population",
    ylab = "Moose Collisions per Capita",
    main = "Moose Collisions per Capita vs Human Population")
  
  
#Question 29: Describing trends
    # Moose collisions per capita tend to be higher in regions with lower human populations, 
    # which makes sense because rural areas often have higher moose densities and fewer
    # mitigation measures such as fencing or traffic control.
    





