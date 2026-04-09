# This assignment was already completed before the object names instructed on the assignment were changed, so the names I used are slightly different because we were instructed to keep our previous work. Sorry.
setwd("C:/Users/keile/OneDrive/BIOL1002_Rassignment")

# Part I

install.packages(dyplr)
library(dplyr)

Moosedata <- read.csv("MoosePopulation.csv")

View(Moosedata)
Moosedata_clean <- na.omit(Moosedata)

Moosedata_sel <- select(Moosedata_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

min(Moosedata_sel$Year)
max(Moosedata_sel$Estimated_Moose_Pop)

Moosedata2 <- mutate(Moosedata_sel, MooseDensity = Estimated_Moose_Pop / Area)

plot(Moosedata2$Year, Moosedata2$MooseDensity,xlab = "Year",ylab = "Moose per sq km",main = "Newfoundland ecoregion moose density over time")

MooseDataWest <- filter(Moosedata2, Ecoregion == "Western_Forests")
plot(MooseDataWest$Year, MooseDataWest$MooseDensity, type="l", xlab="Year", ylab = "Moose per sq km", main = "Newfoundland ecoregion moose density over time")

MooseData_2020 <- filter(Moosedata2, Year == "2020")
MooseData_2020_high <- filter(MooseData_2020, MooseDensity > 2.0)
arrange(MooseData_2020_high, desc(MooseDensity))

MooseData_final <- Moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

# Part II

SaplingData <- read.csv("SaplingStudy.csv")
SaplingData<-na.omit(SaplingData)

Sapling_EcoBrowse <- SaplingData %>%
  group_by(Ecoregion)%>%
  summarize(BrowsingScore=mean(BrowsingScore))%>%
  print()
Sapling_EcoBrowse_desc<-arrange(Sapling_EcoBrowse, desc(BrowsingScore))
# The Northern Peninsula Forests have the most moose browsing and the Strait of Belle Isle Barrens have the least.

Sapling_EcoHeight<- SaplingData %>%
  group_by(Ecoregion)%>%
  summarize(Height=mean(Height))%>%
  print()
Sapling_EcoHeight_low<-filter(Sapling_EcoHeight, Height<20)%>%
  print()
# The Northern Peninsula Forests and the Western Forests have average heights less than 20 cm.

Sapling_SpecBrowse <- SaplingData %>%
  group_by(Species)%>%
  summarize(BrowsingScore=mean(BrowsingScore))%>%
  print()
Sapling_SpecBrowse_desc<-arrange(Sapling_SpecBrowse, desc(BrowsingScore))%>%
  print()
# The Black Ash species has the highest browsing score and the Black Spruce species has the lowest browsing score.

Balsam_Fir<-SaplingData %>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarize(BrowsingScore=mean(BrowsingScore))%>%
  print()

barplot(Balsam_Fir$BrowsingScore, names.arg = Balsam_Fir$Ecoregion, xlab = "Ecoregion", ylab = "Mean Browsing Score", main = "Balsam Fir Browsing by Ecoregion", col = "forestgreen", cex.names = 0.22)

Black_Spruce<-SaplingData %>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarize(BrowsingScore=mean(BrowsingScore))%>%
  print()
barplot(Black_Spruce$BrowsingScore, names.arg = Black_Spruce$Ecoregion, xlab = "Ecoregion", ylab = "Mean Browsing Score", main = "Black Spruce Browsing by Ecoregion", col = "forestgreen", cex.names = 0.22)
# Both Balsam Fir and Black Spruce trees have relatively high browsing scores in the Northern Shore Forests and Northern Peninsula Forests, But Black Spruce trees have significantly lower browsing scores in the Avalon Forests and Eastern Hyper Oceanic Barrens. There is no browsing score for Balsam Fir trees in the Western Forests.

EcoregionTally<- SaplingData %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

SpeciesTally<- SaplingData %>%
  group_by(Species) %>%
  tally() %>% 
  print()

# The dataset is not perfectly evenly distributed, with Balsam fir trees slightly overrepresented and Black Ash trees severely underrepresented.The ecoregions are slightly less skewed but the Strait of Belle Isle Barrens is underrepresented.
# Recognizing bias in ecological datasets is important because unrecognized bias can lead to invalid results and inaccurate conclusions. This can lead to significant consequences for current and future environmental/ecological management. 

# Part III

MooseData_2020_b<-Moosedata_clean %>%
  filter(Year == "2020") %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
MooseSaplingData <- left_join(MooseData_2020_b, SaplingData, by = 'Ecoregion', relationship = "many-to-many")

Browsing_SpecDensity<-MooseSaplingData %>%
  group_by(Species,Ecoregion)%>%
  summarize(BrowsingScore=mean(BrowsingScore),MooseDensity=mean(MooseDensity))%>%
  print(n=38)

# There is some evidence to support the researchers hypothesis. Lower moose densities show more significant differences in browsing scores between species, while higher moose densities show very little browsing score differences between species.
# The Willow tree has the highest browsing scores and is favoured the most. The Black Spruce tree has the lowest browsing scores and is favoured the least.
# The Black Ash tree is not shown on the figure because there is only one tree of this species in the dataset.

collisions2020<- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop<- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites<- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll<- data.frame(collisions2020, human_pop, study_sites)

moose_coll2<-moose_coll%>%rename(Ecoregion=study_sites)
coll_merge <- left_join(MooseData_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")

plot(coll_merge$MooseDensity,coll_merge$collisions2020, main="Moose Density vs Collisions (2020)", xlab="Moose Density",ylab="Collisions (2020)")
# There is a general positive correlation between the moose density and the number of collisions during the year 2020. There is one major outlier.

coll_merge_per_capita <- mutate(coll_merge, coll_per_capita= collisions2020/human_pop)

plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita, main = "Human Population vs Collisions Per Capita", xlab = "Human Population", ylab="Collisions Per Capita")

# There are more collisions per capita with lower human populations. This makes sense because the greater the ratio of human population to moose population, the less likely each person is to experience a collision with a moose.