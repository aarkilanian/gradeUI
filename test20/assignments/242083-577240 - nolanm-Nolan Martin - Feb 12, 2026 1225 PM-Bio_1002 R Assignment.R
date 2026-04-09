# Nolan Martin Biology 1002 Activity 1
install.packages("dplyr")
moosedata<-read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv")
View(moosedata)
mooseclean<-na.omit(moosedata) 
library(dplyr)
moose_sel <- select(mooseclean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_sel$Year)
year_min<-min(moose_sel$Year)
5# min =1905
year_max<-max(moose_sel$Year)
5# max =2020
moosedata2 <- mutate(moose_sel, MooseDensity=Estimated_Moose_Pop/Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab="Year",
     ylab="Moose per sq km",
     main="Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab="Year",
     ylab="Moose per sq km",
     main="Moose density in Newfoundland's Western Forrests over time")
moose_2020 <- filter(moosedata2, Year == 2020)
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
arrange(moose_2020_high, desc(MooseDensity))
library(dplyr)
moosefinal<- moosedata2 %>%
  filter(Year==2020)%>%
  filter(MooseDensity>2.0)%>%
  arrange(desc(MooseDensity))%>%
  print()

saplings<- read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
library(dplyr)
sap_clean <- na.omit(saplings)
sap_reg_browse <- saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print ()
avg_browse_reg <- arrange(sap_reg_browse, desc(mean_BrowsingScore))
# Northern Penisula Forests have the highest average browsing score of 4.57, the straight of Bell Island Barrens has the lowest average browing score with 1.
library(dplyr)
sap_reg_height <- saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_Height = mean(Height)) %>%
  print ()
# The Ecoregions with a mean sapling height of less than 20cm includes: the Western Forests(18.9cm) and the Nothern Penusula Forsts(19.9cm).
sap_reg_height_low <- filter(sap_reg_height, mean_Height<20.0)
sap_spe_browse <- saplings %>%
  group_by(Species) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()
sap_spe_browse <- arrange(sap_reg_browse, desc(mean_BrowsingScore))
#The Alder has the highest mean browsing score of 4.25, the Black Spruce has the lowest mean browsing score of 2.33.
install.packages("crayon")
library(dplyr)
fir_reg_browse <- saplings %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
barplot(fir_reg_browse$mean_BrowsingScore, names.arg= fir_reg_browse$Ecoregion,
        xlab="Ecoregions", ylab="Average Browsing Score", main= "Average Browsing Score of Balsam Firs over the Different Ecoregions of Newfoundland",
        col= "forestgreen",, cex.names = 0.6)
spruce_reg_browse <- saplings %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
# Overall Balsam Firs experience more browsing than Black Spruces. Black Spruces experience high levels of browsing in Northern Shore and Northern Penisula forsests, yet they experience no browsing in Eastern Hyper Oceanic Barrens and Maritime Barrens.
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
tally() %>%
print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
# I do not think the Sapling Study data set is evenly distributed and that there is sampling bias present. The mode sample number for ecoregions is 5, yet the "Strait of Belle Island Barrens" only has 1 sample, while, the "North Shore Forests" have 8 samples. The same differences in sampling distribution is present in the number of samples for each species as well. With the "Black Ash" species being vastly under sampled.
# It is important to recognize bias in ecological datasets because bias results in our data to be misleading. The data no longer accurately represents the browsing of different plant species across different ecoregions, it only reflects a small part of reality.
names(mooseclean)
moose_2020b <- mooseclean %>%
  filter(Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by ="Ecoregion", relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species,Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore),
  mean_MooseDensity= mean(MooseDensity))
  print(sum_spe_browse)
library(ggplot2)
  ggplot(sum_spe_browse, aes(x= mean_MooseDensity, y= mean_BrowsingScore, color= Species)) +
    geom_point(size=3) +
  theme_minimal() +
    labs(title= "Browsing Intensity Across Moose Density by Species",
         x="Average Moose Density",
         y="Average Browsing Score")
# Based on the figure, there is evidence that supports the researcher hypothosis that moose show strong preferences in which species that browse when the average moose density is low. We see in the graph that when the average moose density is high, the moose are browsing various species of trees at high scores, whereas, when the average moose density is low, only specififc species of plants display high browsing scores.
# The moose favour Alder and Willow the most because the average browsing score when moose density is low is high amoungst those species. In contrast, moose browse Black Spruce trees the least.
# The only species not shown in the figure is Black Ash, there is only on sampling collection of Black Ash, it's only data point is (2,5.5). Upon, further inspection of the graph, there is already a data point displayed for this specific point. Therefore that point must be covering up the Black Ash point.
  
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020,human_pop,study_sites)
library(dplyr)
moose_coll2 <- rename(moose_coll, Ecoregion=study_sites)

coll_merge <- left_join(moose_coll2, moose_2020, by ="Ecoregion", relationship = "many-to-many")

ggplot(coll_merge, aes(x= MooseDensity, y= collisions2020,)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title= "How Moose Density Relates to Moose Vehical Collisions",
       x="Moose Density",
       y="Vehical Collisions")
#The trend we see from this graph is that as moose density increases, the amount of moose related vehicle collisions increases as well. There is an obvious outlyer in this data, the ecoregion that has produce the outlyer is the Avalon Penisula.

coll_merge_par_capita <- coll_merge %>%
  mutate(coll_per_capita=collisions2020/human_pop)

plot(coll_merge_par_capita$coll_per_capita,coll_merge_par_capita$human_pop,
     type = "p",
     xlab="Human Population",
     ylab="Moose Collisions per Capita",
     main="Moose Collisions per Capita vs. Human Population")
# Th trend we see from this graph is that when human population increases, the moose collisions per capita decreases. This makes sense based on the what we know about urban and rural life in Newfoundland. A person who lives in a rural community has to drive longer distances in forest areas that are populated by moose. This vastly increases their chances of being in a collision with a moose.

ggplot(coll_merge, aes(x= MooseDensity, y= collisions2020, color= Ecoregion)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title= "How Moose Density Relates to Moose Vehical Collisions by Ecoregion",
       x="Average Moose Density",
       y="Average Browsing Score")