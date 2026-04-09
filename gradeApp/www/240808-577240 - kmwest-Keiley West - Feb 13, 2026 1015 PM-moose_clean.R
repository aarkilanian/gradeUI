C:Users/Keile/OneDrive/Documents/BIOL1002_Rassignment
getwd()
setwd("C:/Users/Keile/OneDrive/Documents/BIOL1002_Rassignment")
#Q1
install.packages("dplyr")


# Part I ------------------------------------------------------------------

#Q2
moosedata <-read.csv("MoosePopulation.csv")
#Q3
View(moosedata)
moose_clean <- na.omit(moosedata)
View(moosedata)
library(dplyr)
#Q4
moose_sel <-select(moose_clean,Ecoregion, Year, Area, Estimated_Moose_Pop)
#Q5a
year_min <- min(moose_sel$Year)
#Q5b
moose_max <-max(moose_sel$Estimated_Moose_Pop)
#Q6
moosedata2 <-mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
#Q7
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab = "year",
     ylab = "Moose per sq km",
     main = "Moose desnity in Newfoundland ecoregions over time")
#Q8a
moose_west <-filter(moosedata2, Ecoregion == "Western_Forests")
#Q8b
plot(moose_west$Year,moose_west$MooseDensity,
        pch = "17",type = "l", 
        xlab = "year",
        ylab = "Moose per sq km",
        main = "Moose density in Newfoundland Western Forests over time")
#Q9a
moose_2020_high <-filter(moosedata2,Year==2020)
#Q9b
moose_2020_high <-filter (moosedata2,MooseDensity > "2.0")
#Q9c
moose_2020_high_byD <- arrange (moosedata2, desc(MooseDensity))
#Q10
moosefinal <- moosedata2 %>%
  filter(Year ==2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()


# Part II -----------------------------------------------------------------
#Q11a
saplings <- read.csv("SaplingStudy.csv")
View(saplings)
#Q11b
sap_clean <-na.omit(saplings)
#Q12a
sap_reg_browse <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(avgbrowsing = mean(BrowsingScore)) %>% 
  print()
#Q12b
avg_browse_reg <- arrange(sap_reg_browse,
                          desc(avgbrowsing))
print(avg_browse_reg)
#Northern Peninsula Forests had the highest average browsing scores, and the Strait Of Belle Isle Barrens had the Lowest average browsing scores
#Q13a
sap_reg_height <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  summarize(Tree_height = mean(Height)) %>% 
            print()
#Q13b
sap_reg_height_low<- sap_clean %>% 
  filter(Height <20) %>% 
  print()
#Q14a
sap_spe_browse <- sap_clean %>% 
  group_by(Species) %>% 
  summarize(BrowsingScore = mean(BrowsingScore)) %>% 
  print()
#Q14b
avg_browse_spe <- arrange(sap_spe_browse,desc(BrowsingScore))
# The species with the highest browsing score is the black ash, and the species with the lowest browsing score is the black spruce.
#Q15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>%
  summarize(BrowsingScore = mean(BrowsingScore)) %>% 
  print()
#Q16
barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main ="Average browsing score of the balsam fir based off ecoregion", col="forestgreen", cex.names= 0.6) 
#Q17a
spruce_reg_browse <-sap_clean %>% 
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(BrowsingScore = mean(BrowsingScore))
#Q17b
barplot(spruce_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average browsing score", main = "Average browsing score of the black spruce based off ecoregion", col="forestgreen", cex.names = 0.6)
# By comparing the 2 bar graphs,we can see that the browsing of the Black Spruce is much more wide spread across the province, as well as just slightly higher compared to the Balsam Fir
#Q18
sap_reg_tally <- sap_clean %>% 
  group_by(Ecoregion) %>% 
  tally() %>% 
  print()
#Q19
sap_spe_tally <- sap_clean %>% 
  group_by(Species) %>% 
  tally() %>% 
  print()
#Q20a I don't think that the Sapling Study data set is evenly distributed, as the Black Ash only has 1 species counted, whereas the Balsam Fir has 11. The same thing is seen in the ecoregions, with the Strait of Belle Isle Barrens only having 1 sample, where the North Shore Forests have 8
#Q20b It's very important to recognize bias in ecological data sets because when we have regions or species that only have say, 1 sample, and others with 10+, it creates an uneven spread of data, and means that we might not be getting the most accurate results.
#Q21a
moose_2020 <-filter(moose_clean, Year == "2020")
moose_2020b <- mutate(moose_2020, MooseDensity=Estimated_Moose_Pop/Area)
#Q21b
moose_sap<- left_join(moose_2020b, sap_clean, by = "Ecoregion", relationship = "many-to-many")
#Q22
sum_spe_browse<- moose_sap %>%
  group_by(Species) %>% 
  group_by(Species) %>% 
  summarize(BrowsingScore = mean(BrowsingScore),
            MooseDensity = mean(MooseDensity)) %>% 
  print()
#Q23
install.packages("ggplot2")
library(ggplot2)
# No, the plot does not support the researcher's hypothesis, because if we look the higher the density of the moose, the more fine tuned their browsing preferences become  
# The moose seem to favor the Willow, White Birch, and Black Spruce. They show the least interest in the Black Ash and Balsam Fir
# The sapling species that is not shown in the figure is the Black Ash, and the reason for this is because the moose showed the least interest in browsing upon it

# Part III ----------------------------------------------------------------
#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
#Q25a
moose_coll2 <- moose_coll %>% 
  rename(Ecoregion = study_sites)
#Q25b
coll_merge <- left_join(moose_coll2, moose_2020b,by="Ecoregion")
#Q26a
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab = "Moose Density", ylab = "Collisions 2020")
# Q26b 
# Q27
coll_merge_per_capita <-coll_merge %>% 
  mutate(coll_per_capita = collisions2020/human_pop)
#Q28
plot(coll_merge_per_capita$collisions2020, coll_merge_per_capita$human_pop, xlab= "Human Population",ylab = "Collisions 2020")
