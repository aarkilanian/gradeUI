library(dplyr)
saplings = SaplingStudy
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
           group_by(Ecoregion) %>%
           summarize(BrowsingScore=mean(BrowsingScore))
sap_reg_browse <- print(sap_reg_browse)
avg_reg_browse <- arrange(sap_reg_browse, desc(BrowsingScore))
#Northern_Peninsula_forests had the highest, StraitOfBelleIsleBarrens had the lowest
sap_reg_height <- sap_clean %>%
     group_by(Ecoregion) %>%
     summarize(Height=mean(Height))
sap_reg_height <- print(sap_reg_height)
#Northern_Peninsula_Forests and Western_Forests have average heights less than 20cm
sap_reg_height_low <- print(filter(sap_reg_height, Height < 20))
sap_spe_browse <- sap_clean %>%
     group_by(Species) %>%
     summarize(BrowsingScore=mean(BrowsingScore))
sap_spe_browse <- print(sap_spe_browse)
avg_browse_spe <- arrange(sap_spe_browse, desc(BrowsingScore))
#Black_Ash had the highest score, Black_Spruce had the lowest
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing=mean(BrowsingScore))
fir_reg_browse <- print(fir_reg_browse)
barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing", main = "Average Browsing Intensity per Ecoregion for Balsam Fir", col = "forestgreen", cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing=mean(BrowsingScore))
barplot(spruce_reg_browse$AverageBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing", main = "Average Browsing Intensity per Ecoregion for Black Spruce", col = "lightblue", cex.names = 0.6)
#Black Spruce browsing occurred a lot less or not at all in long range barrens, overall there was more browsing in all regions for Balsam Firs.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()
#For the most part, the dataset is evenly distributed, except for the fact that the ecoregion "StraitOfBelleIsleBarrens" is underpresented with only one of its kind on the table.
#It is important to recognize bias in ecological datasets because it can affect how accurate the conclusions are by causing over/underestimations.