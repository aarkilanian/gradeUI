# Title: R script for Assignment 1
# Author: Alexis Hodder
# Date: 05-04-2026

# Question 1 Load libraries needed ---------------------------
install.packages("tidyverse")

library(dplyr)

# Set working directory ---------------------------
getwd()
setwd("~/Desktop/RStudio-Files-Folder")


# Question 2 load data ---------------------------
moosedata <- read.csv("MoosePopulation.csv")
View(moosedata)

# Question 3 remove NA
moose_clean <- na.omit(moosedata)
View(moose_clean)

# Question 4 select columns 
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5 min and max
year_min <- select(moose_sel,min("Year"))
moose_max <- select(moose_sel, max("Estimated_Moose_Pop")
                    
 # Question 6 mutate
 moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
                    
 # Question 7 plot()
plot(moosedata2$Year, moosedata2$MooseDensity, 
  +      xlab = "year", 
  +      ylab = "Moose per sq km", 
                         +      main = "Moose density in Newfoundland ecoregions over time")
                    
 # Question 8 filter () plot ()
 moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
 plot(moose_west$Year, moose_west$MooseDensity, type = "l", main = "Western_Forests Moose Density", xlab = "Year", ylab = "Moose per sq km")
                    
                    
  # Question 9 filter () arrange ()
 moose_2020 <- filter(moosedata2, Year == "2020")
                    
moose_2020_high <- filter(moose_2020, MooseDensity >2)
                    
 moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
                    
 
 #Question 10
                    
  > moosefinal <- moosedata2 %>%
 +     filter(Year == 2020) %>%
 +     filter(MooseDensity > 2.0) %>%
 +     arrange(desc(MooseDensity)) %>%
                      +     print()
                    
                    
 # Question 11 sapling data 
 saplings <- read.csv("SaplingStudy.csv")
 sap_clean <- na.omit (saplings)
 View(sap_clean)
                    
                    
# Question 12 group_by(), summerise (), mean ()
sap_reg_browse <- sap_clean %>%
+ group_by(Ecoregion)%>% summarise(mean(BrowsingScore))
                    
sap_reg_browse <- rename(sap_reg_browse,AverageBrowse = "mean(BrowsingScore)")
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowse))
                    
# Highest Northern_Peninsula_Forests
# Lowest StraitOfBelleIsleBarrens
                    
                    
# Question 13 Pipes
sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarise(mean(Height))
group_by(Ecoregion)%>% summarise(mean(BrowsingScore))
sap_reg_height_low <- sap_reg_height%>% filter(averageheight >20)
                    
print(sap_reg_height_low)
## Northern_Peninsula_Forests 19.91
## Western_Forests 19.94 
                    
                    
# Question 14
avg_browsing_spe <- arrange(sap_spe_browse, desc( "AverageBrowsingScore"))
print(avg_browsing_spe)
sap_spe_browse <- rename(sap_spe_browse, AverageBrowsingScore = "mean(BrowsingScore)")
avg_browsing_spe <- arrange(sap_spe_browse, desc( "AverageBrowsingScore"))
# highest: Alder Lowest: Black Spruce
    
                
# Question 15
                    
fir_reg_browse <- sap_clean %>%
filter(Species= "Balsam_fir")%>%
group_by(Ecoregion) %>% summarise(mean(BrowsingScore))
         
           
# Question 16
                    
> fir_reg_browse <- rename(fir_reg_browse, AverageBrowsingScore = "mean(BrowsingScore)")
> barplot(fir_reg_browse$AverageBrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "mean(BrowsingScore)", main = "Avg BrowsingScore by Region", col = "forestgreen", cex.names = 0.6)
         
           
# Question 17 
spruce_reg_browse <- sap_clean %>%
filter(Species== "Black_Spruce") %>% 
group_by(Ecoregion)%>% summarise(mean(BrowsingScore))
spruce_reg_browse <- rename(spruce_reg_browse, AverageBrowsingScore = "mean(BrowsingScore)")
barplot(spruce_reg_browse$AverageBrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "mean(BrowsingScore)", main = "Avg BrowsingScore by Ecoregion", col = "blue", cex.names = 0,6)
                    
# In the North Shore Forests, both of the trees grow best. The Balsam Fir trees have an overall better rate of growing in all areas compared to the Black Spruce trees who have lower scores overall. Black Spruce grows the least to none in the Maritime Barrens. 
                    
                    
# Question 18 
                    
sap_reg_tally <- sap_clean %>%
group by(Ecoregion) %>%
tally()%>%
print ()
                    
# No
                    
                    
# Question 19 
sap_spe_tally <- sap_clean %>%
group by(Species) %>%
tally()%>%
print ()
                    
# No 
                    
                    
# Question 20 
                    
View(SaplingStudy)
# The tally of Ecoregion data ranges from 1-8, and the species tally ranges from 1-11
# so when take the average browsing score, there might bea Ecoregion that has a high average, but only
# one sample in the dataset, and same for the species. 
                    
# It is important to recognize bias, because your data may be skewed under or over, which can throw off your findings. 
                    
                    
# Question 21 
                    
moose_2020b <- moose_clean%>%
filter(Year == "2020") %>%
mutate(moose_2020b, MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
                    
                    
# Question 22 
                    
sum_spe_browse <- moose_sap %>%
group_by(Species, Ecoregion)%>%
summarise(AvgBrowse = mean(BrowsingScore, na.rm = TRUE) AvgDensity = mean(MooseDensity, na.rm = TRUE))
                    
# Question 23
                    
library(ggplot2)
                    
ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowse, color = Species)) +
geom_point(size = 3) +
                      
theme_minimal() +
labs(title = "Browsing Intensity Across Moose Density by Species",
     x = "Average Moose Density",
     y = "Average Browsing Score")

#a. Yes, the evidence supports the researchers' hypothesis.

#b. Moose favour Alder the most, and Black Spruce the least.

#c. The sapling species "Black" is not shown on the figure.


# Question 24

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <- c("North_Shore_Forests", "Northern_Peninsula_Forests", "Long_Range_Barrens", "Central_Forests", "Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collision2020, human_pop, study_sites)

# Question 25 

moose_coll2 <- rename(moose_coll, Ecoregion = "study_sides")
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion". relationship = "many-to-many")

# Question 26 

ggplot(coll_merge, aes(x = MooseDensity, y = collisions2020, color = collisions2020)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "MooseDensity vs Moose Collisions in 2020",
       x = "Average Moose Density",
       y = "Number of Collisions in 2020")


# The higher the density, the more collisions that occur with the exception of one area where the population is low
# but the human population is the highest, so there is more traffic. 


# Question 27 

coll_merge_per_capita <- mutate(coll_merge, coll_per_capita = collisions2020 / human_pop )


# Question 28 

plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop, 
     main = "Collision per Capita vs Human Population", 
     xlab = "Collision per Capita", 
     ylab = "Human Population")

# Question 29 

#The scatterplot shows a negative correlation between human population and collisions per capita. As the human population in an ecoregion increases, the number of moose-vehicle collisions per person tends to decrease. 








