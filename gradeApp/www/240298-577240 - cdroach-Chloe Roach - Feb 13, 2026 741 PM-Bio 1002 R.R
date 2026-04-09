# Chloe's R assignment
install.packages("dplyr")
moosedata<-read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/MoosePopulation.csv")
View(moosedata)
mooseclean<-na.omit(moosedata)
library(dplyr)
moose_sel <-select(mooseclean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_sel$Year)
year_min<-min(moose_sel$Year)
5#min=1904
year_max<-max(moose_sel$Year)
5# max=2020
moosedata2 <-mutate(moose_sel, MooseDensity=Estimated_Moose_Pop/Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
     xlab="Year",
     ylab="Moose per sq km",
     main="Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion =="Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity,
     type = "l",
     xlab="year",
     ylab="Moose per sq km",
     main="Moose density in Newfoundlands Western Forests over time")
moose_2020 <-filter(moosedata2, Year == 2020)
moose_2020_high <-filter(moose_2020, MooseDensity > 2.0)
arrange(moose_2020_high, desc(MooseDensity))
library(dplyr)
moosefinal <-moosedata2 %>%
  filter(Year==2020) %>%
  filter(MooseDensity>2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings<-read.csv("https://raw.githubusercontent.com/ahurford/quant-guide-data/refs/heads/main/BIOL1002-data/SaplingStudy.csv")
library(dplyr)
sap_clean <- na.omit(saplings)
sap_reg_browse <- saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_BrowsingScore = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- arrange(sap_reg_browse, desc(mean_BrowsingScore))  
library(dplyr)
sap_reg_height <- saplings %>%
  group_by(Ecoregion) %>% 
  summarize(mean_Height = mean(Height)) %>%
  print()
#The Ecoregions with a mean sapling height of less than 20cm includes: The western forests, and the northen penisula forests 
sap_reg_height_low <- filter(sap_reg_height, mean_Height<2.0)
sap_spe_browse <- saplings %>%
  group_by(Ecoregion) %>%
  summarize(mean_Height = mean(Height)) %>%
  print()
sap_spe_browse <- arrange (sap_reg_browse,desc(mean_BrowsingScore))
#alder has the heightest mean browsing score of 4.25, the black spruce has the lowest of 2.33
install.packages("crayon")
library(dplyr)
fir_reg_browse <- saplings %>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
barplot(fir_reg_browse$mean_BrowsingScore, names.arg= fir_reg_browse$Ecoregion,
        xlab="Ecoregions", ylab="Average Browsing Score", main="Average Browsing Score of Balsam Firsover different Ecoregions",
        col="forestgreen",, cex.names =0.6)
Spruce_reg_browse <- saplings %>%
  filter(Species =="Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarize(mean_BrowsingScore = mean(BrowsingScore))
#Overall Balsam Firs Experience more browsing than Back Spruce 
sap_reg_tally<- sap_clean%>%
  group_by(Ecoregion)%>%
  tally()%>%
  print()
sap_spe_tally<- sap_clean %>%
  group_by(Species)%>%
  tally()%>%
  print()
#Sapling study data set is not evenly distributed so there is a sampling bias present 
#It is important to recognize bias in ecological datasets because bias can cause misleading data
names(mooseclean)
moose_2020b <- mooseclean%>%
  filter(Year == "2020")%>%
  mutate(MooseDensity = Estimated_Moose_Pop/Area)
moose_sap <-left_join(moose_2020b, sap_clean,by="Ecoregion",relationship="many-to-many")
sum_spe_browse<-moose_sap%>%
  group_by(Species,Ecoregion)%>%
  summarize(mean_BrowsingScore= mean(BrowsingScore),
  mean_MooseDensity=mean(MooseDensity))
print(sum_spe_browse)
install.packages("ggplot2")
library(ggplot2)
ggplot(sum_spe_browse,aes(x=mean_MooseDensity, y=mean_BrowsingScore,color=Species))+
  geom_point(size=3)+
  theme_minimal()+
  labs(title="Browsing Intenstiy across moose density by Species",
       x="Average Moose Density",
       y="Average Browsing Score")
#based on the figure, there is evidence that supports the researcher hypothesis
# The moose tend to favour Alder and willow the most 
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
study_sites <-c("North_Shore_Forests", "Northern_penisula_Forests", "Long_Range_Barrens", "Central_Forests","Western_Forests", "EasternOceanicBarrens", "Martitimes_Barrens","Avalon_Forests", "StraitOfBellIsleBarrens")
moose_coll <-data.frame(collisions2020,human_pop,study_sites)
library(dplyr)
moose_coll2<- rename(moose_coll, Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2,moose_2020, by="Ecoregion", relationship="many-to-many")
ggplot(coll_merge,aes(x=MooseDensity, y=collisions2020,))+
  geom_point(size=3)+
  theme_minimal()+
  labs(title="how Moose Desnity relates to Moose Vehical Collisions",
       x="Moose Density",
       y="vehical collisions")
#the trend seen in the graph is that as density increases, the number of moose related vehical collisions increases. 
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita=collisions2020/human_pop)
plot(coll_merge_per_capita$coll_per_capita,coll_merge_per_capita$human_pop,
     type="p",
     xlab="human Population",
     ylab="Moose Collisons Per Capita",
     main= "moose collisions per capita vs Human population")
#The trend seen from this graph is human population increases, the mosse collisions per captia decreases
