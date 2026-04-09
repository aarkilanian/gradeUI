library(dplyr)
moosedata <- read.csv("MoosePopulation.csv")
moose_clean<-na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min<- min(moose_sel$Year)
year_max<- max(moose_sel$Year)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     type = "l", 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l", 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_2020 <- moosedata2 %>% filter(Year == "2020")
moose_2020_high <- filter(moose_2020 ,MooseDensity > 2)
moose_2020_high_byD <- arrange(moose_2020_high, desc=(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore)) %>% print()
avg_browse_reg <- sap_reg_browse %>% arrange(desc(`mean(BrowsingScore)`)) %>% print()                
# According to the avg_browse_reg StrautOfBellIsleBarrens has the lowest mean which is 1 and Northern_Peninsula_Forests has the highest mean of 4.57
sap_reg_hieght <- avg_browse_reg %>% group_by(Ecoregion) %>% summarize(AverageTreeHeight = mean(`mean(BrowsingScore)`)) 
sap_reg_hieght_low <- sap_reg_hieght %>% filter(AverageTreeHeight < "20") %>%  print()
#All of the tress under the sap_reg_hieght are less than 20cm
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>% 
  summarize(mean(BrowsingScore))
print()

#14 (b)
avg_browse_spe <- sap_spe_browse %>% arrange(desc("mean(BrowsingScore)")) %>% print ()
#Black_Ash have the highest browsing score and Black_Spruce has the lowest browsing score 
fir_reg_browse <- sap_clean %>% filter (Species == "Balsam_Fir") %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore))

barplot(fir_reg_browse$"mean(BrowsingScore)", names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Adverage browsing intensity", main = "Adverage browsing intensity in different ecoregions for Balsam Fir", col = "forestgreen")

fir_reg_browse <- sap_clean %>% filter (Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(mean(BrowsingScore))    

barplot(fir_reg_browse$"mean(BrowsingScore)", names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Adverage browsing intensity", main = "Adverage browsing intensity in different ecoregions for Black Spruce", col = "Black")
# Black spruce is a less intense in certain regions compared to Balsam Fir. It is to the point where there is only a faint line for Black Spruce on the bar graph compared to the larger lines on the bar graph in those same regions for Balsam Fir. 
sap_reg_tally<- sap_clean %>% 
  group_by(Ecoregion) %>% 
  tally() %>% 
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

# Question 20 a 
# Yes, I believe that the SaplingStudy is evenly distributed. For Balsam Fir the Maritime Barrens and Avalon Forests were the most under presented, Long Range Barrens was also slightly under presented compared to the others.Balsam Fir in the North Shore Forest and Northern Peninsula Forests were over presented. For Black spruce it was under presented in StraitOfBelleisleBarrens as well as in Maritime Barrens but was over presented in the Western Forests, North Shore Forests, and Central Forests.

# Question 20 b 
# Getting numerical data is important because it allows ecologists to have numerical data to show results not allow pre-existing bias have any influence on their data, potential future decisions, etc. 

#Question 21 a
moose_2020b <- moose_2020

#Question 21 b
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship= "many-to-many")

#Question 22
sum_spe_browse <- moose_sap %>% 
  group_by(Species, Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

#Question 23
install.packages("ggplot2")
library(ggplot2)

ggplot(sum_spe_browse, aes( x = AvgDensity, y= AvgBrowsing, colour=Species)) +
  geom_point(size=3) + 
theme_minimal() +
labs (title= "Browsing Intensity Across Moose Density by Species", 
      x= "Average Moose Density",
      y= "Average Browsing Score")

#Question 23 a
#Yes, there is evidence that supports the research hypothesis which is our graph. Looking at the graph we can see that there is a resemblance of a increasing diagonal line. Another thing we can gather from the data is that blue/teal colors (Black Spruce and White Birch) are beneath the "diagonal line" which red and pink (Alder and Willow) are on the top of the "diagonal line". 

#Question 23 b
#We can see by looking at the graph that the sapling species moose favor most is willow and white birch. 

#Question 23 c 
#Black ash is not shown on the graph. Since it is not on the graph that indicates that is not a species consumed by moose (or consumed very little). 

#Question 24 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>% rename(Ecoregion = study_sites)
#Asked a TA and they had the "answer sheet" and we could not figure out why this is popping up error and not working. What i have written is correct according to the answer sheet. 

merge <- left_join(moose_coll2, moose_2020, by="Ecoregion")

#Question 26 
plot(MooseDensity$study_sites, MooseDensity$collisions2020)
plot(moose_west$Year, moose_west$MooseDensity, 
     type = "l", 
     xlab = "study_sites", 
     ylab = "collisions2020", 
     main = "Moose Veichle Collisions")
#By looking at this line graph we can  see an outlier in the year 1980, there is a large dip in the line before it continues to increase. 

#Question 27
coll_merge_per_captia <-  mutate(merge, coll_per_captia =
  collisions2020 / human_pop)
  
#Question 28
plot(coll_merge_per_captia$coll_per_captia,
     coll_merge_per_captia$human_pop,
xlab = "Human population",
ylab = "Per captia",
main = "Human population vs per captia")

#Question 29
#Looking at this scatter plot we can see that the lower the human population the higher collision rate. This trend makes sense based off what i know about Newfoundland because there is typically moose vs vehicle collisions on the road between towns (such as on highways). 