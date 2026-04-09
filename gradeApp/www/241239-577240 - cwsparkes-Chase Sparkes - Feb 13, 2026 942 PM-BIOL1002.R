install.packages("dplyr")
#Question 1
library("dplyr")
#Question 2
moosedata<-read.csv("MoosePopulation.csv")
#Question 3
View(moosedata)
moose_clean<-na.omit(moosedata)
#Question 4
moose_sel<-select(moose_clean,Ecoregion,Year,Area,Estimated_Moose_Pop)
#Question 5a
year_min<-min(moose_sel$Year)
year_min
#Question 5b
moose_max<-max(moose_sel$Estimated_Moose_Pop)
moose_max
#Question 6
moosedata2<-mutate(moose_sel,MooseDensity=Estimated_Moose_Pop/Area)
#Question 7
plot(moosedata2$Year,moosedata2$MooseDensity,xlab="year",ylab="Moose per sq km", main="Moose density in Newfoundland ecoregions over time")
#Question 8a
moose_west<-filter(moosedata2,Ecoregion=="Western_Forests")
#Question 8b
plot(moose_west$Year,moose_west$MooseDensity,type="l",xlab="year",ylab="Moose per sq km",main="Moose density in Newfoundland western forests over time ")
#Question 9a
moose_2020<-filter(moosedata2,Year=="2020")
#Question 9b
moose_2020_high<-filter(moose_2020,MooseDensity>"2.0")
#Question 9c
moose_2020_high_byD<-arrange(moose_2020_high,desc(MooseDensity))
#Question 10
moosefinal<-moosedata2%>%filter(Year=="2020")%>%filter(MooseDensity>"2.0")%>%arrange(desc(MooseDensity))%>%print()
#Question 11a
saplings<-read.csv("SaplingStudy.csv")
#Question 11b
sap_clean<-na.omit(saplings)
#Question 12a
sap_reg_browse<-sap_clean%>%group_by(Ecoregion)%>%summarize(AverageBrowsing=mean(BrowsingScore))%>%print()
#Question 12b
avg_browse_reg<-sap_reg_browse%>%arrange(desc(AverageBrowsing))%>%print() 
#Highest Average Browsing Score: Northern_Peninsula_Forests
#Lowest Average Browsing Score: StraitOfBelleIsleBarrens
#Question 13a
sap_reg_height<-sap_clean%>%group_by(Ecoregion)%>%summarize(AverageHeight=mean(Height))%>%print()
#Question 13b
sap_reg_height_low<-sap_reg_height%>%filter(AverageHeight<"20")%>%print()
#Northern_Peninsula_Forests and Western_Forests have average heights less than 20cm.
#Question 14a
sap_spe_browse<-sap_clean%>%group_by(Species)%>%summarize(AverageBrowsing=mean(BrowsingScore))%>%print()
#Question 14b
avg_browse_spe<-sap_spe_browse%>%arrange(desc(AverageBrowsing))%>%print()
#Highest Average Browsing Score: Black_Ash
#Lowest Average Browsing Score: Black_Sprice
#Question 15
fir_reg_browse<-sap_clean%>%filter(Species=="Balsam_Fir")%>%group_by(Ecoregion)%>%summarize(AverageBrowsing=mean(BrowsingScore))%>%print()
#Question 16
barplot(fir_reg_browse$AverageBrowsing,names.arg=fir_reg_browse$Ecoregion,xlab="Ecoregion",ylab="Average Browsing Score",main="Balsam Fir Browsing by Ecoregion",col="forestgreen",cex.names=0.6)
#Question 17a
spruce_reg_browse<-sap_clean%>%filter(Species=="Black_Spruce")%>%group_by(Ecoregion)%>%summarize(AverageBrowsing=mean(BrowsingScore))%>%print()
#Question 17b
barplot(spruce_reg_browse$AverageBrowsing,names.arg=spruce_reg_browse$"Ecoregion",xlab="Ecoregion",ylab="Average Browsing Score",main="Black Spruce Browsing by Ecoregion",col="forestgreen",cex.names=0.6)
#Question 17c
#Balsam Fir experiences slightly more browsing than Black Spruce across most ecoregions.
#Question 18
sap_reg_tally<-sap_clean%>%group_by(Ecoregion)%>%tally()%>%print()
#Question 19
sap_spe_tally<-sap_clean%>%group_by(Species)%>%tally()%>%print()
#Question 20a
#The sapling study is not evenly distributed. Some ecoregions and species have more observations than others, indicating a sampling bias. 
#Question 20b
#Recognizing bias in ecological datasets is important because inconsistent sampling can yield misleading conclusions and less accurate results.
#Question 21a
moose_2020b<-moose_clean%>%filter(Year=="2020")%>%mutate(MooseDensity=Estimated_Moose_Pop/Area)
moose_sap<-left_join(moose_2020b,sap_clean,by="Ecoregion",relationship="many-to-many")
#Question 22
sum_spe_browse<-moose_sap%>%group_by(Species,Ecoregion)%>%summarize(AverageBrowsing=mean(BrowsingScore),AverageDensity=mean(MooseDensity))%>%print()
#Question 23a
#Yes, at low moose densities, browsing intensity varies more among species, indicating preference. At higher densities, browsing increases across most species, suggesting more generalist browsing.
#Question 23b
#Moose favor willow saplings the most, and browse black spruce the least.
#Question 23c
#Black ash is not shown on the figure, indicating no observations or insufficient data for that species.
#Question 24
collisions2020<-c(56,60,14,36,48,10,40,110,6)
human_pop<-c(18000,12000,4000,75100,24000,3500,3200,27000,2300)
study_sites<-c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll<-data.frame(collisions2020,human_pop,study_sites)
#Question 25a
moose_coll2<-moose_coll%>%rename(Ecoregion=study_sites)
#Question 25b
coll_merge<-left_join(moose_2020b,moose_coll2,by="Ecoregion")
#Question 26a
plot(coll_merge$MooseDensity,coll_merge$collisions2020)
#Question 26b
#Collisions tend to increase as moose density increases. There is an outlier around a moose density of 1.0, where collisions are much higher than anywhere else.
#Question 27
coll_merge_per_capita<-coll_merge%>%mutate(coll_per_capita=collisions2020/human_pop)
#Question 28
plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita)
#Question 29
#Collisions per capita tend to decrease as human population increases. This makes sense, as in Newfoundland less moose reside in more populated regions.