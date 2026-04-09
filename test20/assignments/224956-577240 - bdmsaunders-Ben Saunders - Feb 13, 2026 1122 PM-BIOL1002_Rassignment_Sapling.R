saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
 sap_reg_browse <- sap_clean %>%
 group_by(Ecoregion) %>%
 summarize(mean_BrowsingScore = mean(BrowsingScore))
print(sap_reg_browse)
avg_browse_reg <- arrange(sap_reg_browse, desc(mean_BrowsingScore))
# Northern_Peninsula_Forests had the highest browsing score, while had the lowest.
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_Height = mean(Height))
print(sap_reg_height)
sap_reg_height_low <- filter(sap_reg_height, mean_Height < 20)
print(sap_reg_height_low)
# Northern_Peninsula_Forests and Western_Forests would be considered severely browsed by moose.
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
barplot(fir_reg_browse$mean_BrowsingScore,
  names.arg = fir_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Intensity",
  main = "Average Balsam Fir Browsing by Ecoregion",
  col = "forestgreen",
  cex.names = 0.6)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
print(spruce_reg_browse)
barplot(spruce_reg_browse$mean_BrowsingScore,
 names.arg = spruce_reg_browse$Ecoregion,
  xlab = "Ecoregion",
  ylab = "Average Browsing Intensity",
  main = "Average Black Spruce Browsing by Ecoregion",
  col = "darkolivegreen",
  cex.names = 0.6)
#Across the ecoregions, fir saplings have higher average intensities.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#The dataset isn't very equally distributed. There are 8 sets from North_Shore_Forests, while there is only one of StraitOfBelleIsleBarrens.
#Sampling bias can misrepresent the actual amount of a sample by making some species or regions appear more or less affected than they really are.