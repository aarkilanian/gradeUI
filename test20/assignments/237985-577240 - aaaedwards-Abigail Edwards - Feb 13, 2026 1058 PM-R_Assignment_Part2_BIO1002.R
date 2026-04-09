###QUESTION 11
###a)
getwd()
setwd("C:/Users/gaile/Downloads")
saplings <- read.csv ("SaplingStudy.csv")

###b)
sap_clean <- na.omit(saplings)

###QUESTION 12
###a)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore=mean(BrowsingScore)) %>%
  print()

###b)
AverageBrowsing <- sap_reg_browse$BrowsingScore
min_avg <- min(AverageBrowsing)
max_avg <- max(AverageBrowsing)
min_avg
#1
max_avg
#4.571429
avg_browse_score <- sort(AverageBrowsing, decreasing = TRUE)

###QUESTION 13
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(Height=mean(Height)) %>%
  print()

###b)
sap_reg_height_low <- filter(sap_reg_height, Height < 20)
sap_reg_height_low
#Northern_Peninsula_Forests, Western_Forests

###QUESTION 14
###a)
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(BrowsingScore=mean(BrowsingScore)) %>%
  print()

###b)
avg_browse_spe <- sort(BrowsingScore, decreasing = TRUE)
min_avg <- min(BrowsingScore)
max_avg <- max(BrowsingScore)
min_avg
###Black_Spruce
max_avg
###Black_Ash

###QUESTION 15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore=mean(BrowsingScore)) %>%
  print()

###QUESTION 16
barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "BrowsingScore", ylab = "Ecoregion", main = "Average Broswing Intensity By Ecoregion for Balsam Fir", col = "forestgreen", cex.names = 0.2)

###QUESTION 17
###a)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore=mean(BrowsingScore)) %>%
  print()

###b)
barplot(spruce_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "BrowsingScore", ylab = "Ecoregion", main = "Average Browsing Intensity By Ecoregion for Black Spruce", col = "palegreen4", cex.names = 0.18)

###c)
# Balsam Fir has an average browsing score for all ecoregions, the highest being 
# North_Shore_Forest at 4.0, and the lowest being Long_Range_Barrens at 1.0. Black 
# Spruce does not have an average for all ecoregions, but it does have multiple 
# significantly high scores. North_Shore_Forests and Northern_Peninsula_Forests at 4.0.
# EasternHyperOceanicBarrens and Maritime_Barrens are tied for 1.0 A similarity between
# Balsam Fir and Black Spruce is that their highest average browsing scores are both 
# at North_Shore_Forests.

###QUESTION 18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

###QUESTION 19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

###QUESTION 20
###a) 
# StraitOfBelleIsleBarrens is an underrepresented ecoregion, seeing as it has only 
# one line of data. Black_Ash is an underrepresented tree species, as it also has only
# one line. The rest of the tree species and ecoregions are more equal amongst each
# other. 

###b)
# It's important to recognize bias as it will cause certain variables such as ecoregions
# to have less data equaling less accurate results.