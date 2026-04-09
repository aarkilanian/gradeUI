install.packages("dplyr")
library(dplyr)
View(MooseData)
moose_clean <- na.omit(MooseData)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_sel$Year)
max(moose_sel$Estimated_Moose_Pop)
MooseData2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(MooseData2$Year, MooseData2$MooseDensity,xlab = "year",ylab = "Moose per sq km",main = "Moose density in Newfoundland ecoregions over time")
MooseDataWest <- filter(MooseData2, Ecoregion == "Western_Forests")
plot(MooseDataWest$Year,MooseDataWest$MooseDensity,pch=2,type="l",xlab="Time (years)",ylab="Moose per sq km", main="Moose Density in Western Forests Over Time")
moose_2020<-filter(MooseData2,Year==2020)
moose_2020_high<-filter(moose_2020,MooseDensity>2.0)
arrange(moose_2020_high,desc(MooseDensity))
moose_2020_high_byD<-arrange(moose_2020_high,desc(MooseDensity))
moosefinal <- MooseData2 %>% filter(Year == 2020) %>% filter(MooseDensity > 2.0) %>% arrange(desc(MooseDensity)) %>% print()

saplings<-read.csv(file="SaplingStudy.csv")
na.omit(saplings)
sap_clean<-na.omit(saplings)
sap_reg_browse<-group_by(sap_clean,Ecoregion)%>%summarize(BrowsingScore=mean(BrowsingScore))%>%print()
arrange(sap_reg_browse,desc(BrowsingScore))
avg_browse_reg<-arrange(sap_reg_browse,desc(BrowsingScore))
#highest browsing score region was Northern Peninsula and lowest was Strait of Belle Isle Barrens
sap_reg_height<-group_by(sap_clean,Ecoregion)%>%summarize(Height=mean(Height))%>%print()
#Northen Peninsula Forests and Western Forests have average heights of less than 20cm
sap_spe_browse<-group_by(sap_clean,Species)%>%summarize(BrowsingScore=mean(BrowsingScore))%>%print()
arrange(sap_spe_browse,desc(BrowsingScore))
avg_browse_spe<-arrange(sap_spe_browse,desc(BrowsingScore))
#The highest browsing score is Black Ash and the lowest browsing score is Black Spruce
fir_reg_browse<-filter(sap_clean, Species == "Balsam_Fir")%>%group_by(Ecoregion)%>%summarize(BrowsingScore=mean(BrowsingScore))%>%print()
barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Scores of Balsam Firs in Various Ecoregions", col = "darkgreen", cex.names = 0.6)
spruce_reg_browse<-filter(sap_clean, Species == "Black_Spruce")%>%group_by(Ecoregion)%>%summarize(BrowsingScore=mean(BrowsingScore))%>%print()
barplot(spruce_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Browsing Scores of Black Spruce Trees in Various Ecoregions", col = "brown", cex.names = 0.6)
#Black spruce browsing has a significantly higher average browsing score, meaning that the browsing is less intense for balsam firs
sap_reg_tally<- sap_clean %>%group_by(Ecoregion) %>%tally() %>% print()
sap_spe_tally<- sap_clean %>%group_by(Species) %>%tally() %>% print()
#It is not evenly distributed. The balsam fir had 11 individuals counted, but only 1 was counted for the black ash. This shows clear bias for the balsam firs being studied, underrepresenting the black ash. 
#Only one individual was accounted for in the Strait of Belle Isle Barrens where as the North Shore Forests had 8 individuals counted, overrepresenting the North Shore Forests and showing bias.
#It is important to recognize bias in data sets because it can give a false sense of reality, so it is important to make sure that everything is studied equally. 
moose_2020b<-filter(moose_clean,Year=="2020")%>%mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse<-group_by(moose_sap,Species)%>%group_by(Ecoregion)%>%summarize(BrowsingScore=mean(BrowsingScore),MooseDensity=mean(MooseDensity))%>%print()
#a) yes, this supports their hypothesis because they would rarely eat many black spruce at low moose density, but would eat a lot of it at high density because there was a lot of competition between the mooses for food.
#b)The species they ate the most was the willow, and the species they ate the least was the black spruce. However, when competition was high, the browsing scores were significantly closer in intensity, 
#c)The black ash was not shown and this was likely because there was only one individual black ash accounted for.

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2<-moose_coll%>%rename(Ecoregion=study_sites)
coll_merge<-left_join(moose_2020,moose_coll2)
plot(coll_merge$MooseDensity,coll_merge$collisions2020,xlab="Moose Density",ylab="Number of Collisions",main="Collisions with Moose Based on Density in 2020")
#There is an positive upward trend relating high density to more collisions. There is one outlier however around a density of 1 moose/km^2, but this may be correlation, not necesssarily causation. 
coll_merge_per_capita<-mutate(coll_merge,coll_per_capita=collisions2020/human_pop)
plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita,ylab="Collisions Per Capita",xlab="Human Population",main="Collisions with Moose Per Capita compared to Human Population")
#A trend I see is that as the population increased,the number of collisions decreased. This makes sense as moose wouldn't want to live in highly populated areas.
