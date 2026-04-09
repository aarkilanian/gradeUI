library(dplyr)

#Question 11
#a
saplings <- read.csv("SaplingStudy.csv")
#b
sap_clean <- na.omit(saplings)
#Question 12
#a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_browse = mean(BrowsingScore)) %>%
  print()
#b
avg_browse_reg <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore)) %>%
  arrange(desc(AverageBrowsing)) %>%
  print()
#Northern Peninsula Forests have the highest Average Browsing score at 4.57. 
#Strait Of Belle Isle Barrens have the lowest Average Browsing score at 1.

#Question 13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height)) %>%
  print()

sap_reg_height_low <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height)) %>%
  filter(mean_height < 20) %>%
  print()
#Northern Peninsula Forests and Western Forests both have average heights under 20 cm. 

#Question 14
#a
spa_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_browse = mean(BrowsingScore)) %>%
  print()
#b
avg_browse_spe <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_browse = mean(BrowsingScore)) %>%
  arrange(desc(mean_browse)) %>%
  print()
#The Highest browsing species is Black Ash
#The Lowest browsing species is Black Spruce

#Question 15

fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browse = mean(BrowsingScore)) %>%
  print()

#Question 16
barplot(fir_reg_browse$mean_browse, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Intensity",
        main = "Balsam Fir Ecoregion vs Browsing",
        col = c("yellow", "purple")) 

#Question 17
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browse = mean(BrowsingScore)) %>%
  print()
  
  barplot(spruce_reg_browse$mean_browse, names.arg = spruce_reg_browse$Ecoregion,
          xlab = "Ecoregion",
          ylab = "Average Browsing Intensity",
          main = "Black Spruce Browsing vs Ecoregion",
          col = c("red", "blue")) 
  
  #Balsam Fir is a lot more grown in places such as the Avalon Forest and Long Range Barrens.
  #As for the Black Spruce it doesn't grow much in the places that Balsam Fir does
  #But rather grows more so in the Western Forests. 
  
  #Question 18
  #No, there were different results in each Ecoregion, though some did have the same number
  #of tree saplings.
  sap_reg_tally <- sap_clean %>%
    group_by(Ecoregion) %>%
    tally() %>%
    print() 
  
  #Question 19
  #No, the number of tree saplings were different for each species.
  sap_spe_tally <- sap_clean %>%
    group_by(Species) %>%
    tally() %>%
    print()
  
  #Question 20
  #a 
  #No, I don't think the saplingstudy data set is evenly distributed.
  #This is because I find that specifically the species have very different results
  #With Balsam Fir having the most recorded.
  
  #b
  #It is important to recognize bias in ecological data sets because it can significantly
  #affect the decision that people make based on the recorded data.
