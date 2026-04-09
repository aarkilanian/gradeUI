# Title: My R script for assignment 1
# Author : Merridy Hynes
# Date : 09-02-2026

# load libraries needed
library(dplyr)
install.packages("dplyr")

# Set working directory
setwd("/Users/merridyhynes/Documents/MyProject")

# Load Data
moosedata<- MoosePopulation
saplings <- SaplingStudy


# Analyze Data
moose_clean<- na.omit(moosedata)
moose_sel<-select(moose_clean,Ecoregion,Year,Area,Estimated_Moose_Pop)
year_min<-(1904)
moose_max<-(41250)
moosedata2<-mutate(moose_sel,MooseDensity= Estimated_Moose_Pop/Area)
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_2020 <- filter(moosedata2, Year ==2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
na.omit(saplings)
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion)%>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(sap_reg_browse)
avg_browse_reg <- arrange (sap_reg_browse, desc (mean_browsing))
print(avg_browse_reg)
#Northern_Peninsula_Forests has the highest average browsing scores
#StraitOfBelleIsleBarrens has the lowest average browsing scores
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean_height = mean(Height, na.rm = TRUE))
print(sap_reg_height)
sap_reg_height_low <- filter(sap_reg_height, mean_height < 20)
print(sap_reg_height_low)
#The regions with average heights lower than 20cm is Northern_Peninsula_Forests and Western_Forests.
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm =TRUE))
print(sap_spe_browse)
avg_browse_spe <- arrange(sap_spe_browse, desc(mean_browsing))
print(avg_browse_spe)
#Black_Ash has the highest average browsing species
#Black_Spruce has the lowest average browsing score
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion)%>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm =TRUE))
print(fir_reg_browse)
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE))
print(spruce_reg_browse)
#Overall, Black Spruce tends to have lower average browsing scores across
#ecoregions compared to Balsam Fir
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally()
# The SaplingStudy dataset is not completley evenly distrubuted.
#Some ecoregions or species may have more saplings counted than others,
#so certain areas or species could be overrepresented while others are underrepresneted.

#Recognizing bias is important because it ensures that conclusions about moose browsing patterns
#accurately reflect the real ecosystem rather than sampling artifact.
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop/Area)
print(moose_2020b)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species,Ecoregion) %>%
  summarize(mean_browsing = mean(BrowsingScore, na.rm = TRUE),
            mean_moose_density = mean(MooseDensity, na.rm =TRUE))
print(sum_spe_browse)
#Question23
#Yes. Moose Prefer a few species at low density and browse more species at higher density.
# Willow is most browsed; black spruce is least browsed.
# Black Ash is not shown, likely due to no or missing data.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
print(coll_merge_per_capita)


# Plot Data
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density in Newfoundland ecoregions over time")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose density over time in Western Forests Ecoregion")
barplot(fir_reg_browse$mean_browsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Balsam Fir Browsing by Ecoregion",
        col ="forestgreen",
        cex.names = 0.6)
barplot(spruce_reg_browse$mean_browsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Average Black Spruce Browsing by Ecoregion",
        col = "darkgreen",
        cex.names = 0.6)
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density(moose/km^2)",
     ylab = "Number of Moose-vehicle Collisions",
     main = "Moose-Vehicle Collisions vs Moose Density",
     pch = 19,
     col = "darkred")
#Moose-vehicle collisions increase as moose density increases, showing a positive,
#relationship.however, There is a one outlier around density 1.0 with unusually
#high collisions compared to the overall trend.  
plot(coll_merge_per_capita$human_pop/ 1000, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population",
     ylab = "Collisions per Person",
     main = "Moose-Vehicle Collisions per Capita vs Human Population",
     pch = 19,
     col = "blue")
# there is no clear trend between moose vehicle-collisions, and human population density.This makes sense in NL because moose density vary across different areas making certain areas having more moose vehicle collisions which doesnt have to do with the population.




      

        

        


