#Part 1 - Moose Population in Newfoundland and Labrador
install.packages("dplyr")
library("dplyr")
moosedata<-read.csv("MoosePopulation(1).csv")
View(moosedata)
moose_clean<-na.omit(moosedata)
moose_sel<-select(moose_clean,Ecoregion,Year,Area,Estimated_Moose_Pop)
year_min<-min(moose_sel$Year)
#According to the above code, the oldest observation is from 1904
moose_max<-max(moose_sel$Estimated_Moose_Pop)
#According to the above code, the maximum moose population was 41,250
moosedata2<-mutate(moose_sel,MooseDensity=Estimated_Moose_Pop/Area)
plot(moosedata2$Year, moosedata2$MooseDensity,xlab="Year",ylab="Moose per square km",main="Moose Density in Newfoundland Ecoregions Over Time")
moose_west<-filter(moosedata2,Ecoregion=="Western_Forests")
plot(moose_west$Year,moose_west$MooseDensity,type="l",xlab="Year",ylab="Moose per square km",main="Moose Density in Newfoundland Western Forests Over Time")
moose_2020<-filter(moosedata2, Year=="2020")
moose_2020_high<-filter(moosedata2,Year=="2020",MooseDensity>2)
moose_2020_high_byD<-arrange(moose_2020_high,desc(MooseDensity))
moosefinal<-moosedata2%>%
filter(Year==2020)%>%  
filter(MooseDensity>2.0)%>%  
arrange(desc(MooseDensity))%>%  
print()  
# Ecoregion Year  Area Estimated_Moose_Pop MooseDensity
# Northern_Peninsula_Forests 2020 12000               32000     2.666667
#        North_Shore_Forests 2020  8400               21740     2.588095
#            Western_Forests 2020  6800               17000     2.500000
#            Central_Forests 2020 20500               41250     2.012195
#Part 2 - Tree Sapling Study
saplings<-read.csv("SaplingStudy.csv")
View(saplings)
sap_clean<-na.omit(saplings)
sap_reg_browse<-sap_clean%>%
group_by(Ecoregion)%>%  
summarise(BrowsingScore=mean(BrowsingScore))%>%
print()
#Ecoregion                  BrowsingScore
#<chr>                              <dbl>
#  Avalon_Forests                      2   
# Central_Forests                     4   
# EasternHyperOceanicBarrens          2.4 
# Long_Range_Barrens                  2.6 
# Maritime_Barrens                    1.83
# North_Shore_Forests                 4.38
# Northern_Peninsula_Forests          4.57
# StraitOfBelleIsleBarrens            1   
# Western_Forests                     4.5
avg_browse_reg<-arrange(sap_reg_browse, desc(BrowsingScore))
print(avg_browse_reg)
#The Northern Peninsula Forests had the highest average browsing score.
#The Strait of Belle Isle Barrens had the lowest average browsing score.
sap_reg_height<-sap_clean%>%
group_by(Ecoregion)%>%
summarise(Height=mean(Height))%>% 
print()  
#Ecoregion                  Height
#<chr>                       <dbl>
#  Avalon_Forests               32.4
# Central_Forests              23.8
# EasternHyperOceanicBarrens   31.6
# Long_Range_Barrens           29.9
# Maritime_Barrens             26.7
# North_Shore_Forests          22.3
# Northern_Peninsula_Forests   19.9
# StraitOfBelleIsleBarrens     25.4
# Western_Forests              18.9
sap_reg_height_low<-filter(sap_reg_height,Height<20)
print(sap_reg_height_low)
#Ecoregion                  Height
#<chr>                       <dbl>
#  Northern_Peninsula_Forests   19.9
# Western_Forests              18.9
#The Norther Peninsula and Western Peninsula forests have average heights lower than 20
sap_spe_browse<-sap_clean%>%
group_by(Species)%>%
summarise(BrowsingScore=mean(BrowsingScore))%>%
print()  
#Species        BrowsingScore
#<chr>                  <dbl>
#  "Alder "                4.25
# "Balsam_Fir"            3.14
# "Black_Ash"             5   
# "Black_Spruce"          2.33
# "White_Birch"           3.14
# "Willow"                4.31
avg_browse_spe<-arrange(sap_spe_browse,desc(BrowsingScore))
print(avg_browse_spe)
#Black Ash has the highest browsing score, and Black Spruce has the lowest.
fir_reg_browse<-sap_clean%>%
filter(Species=="Balsam_Fir")%>%
group_by(Ecoregion)%>%
summarise(BrowsingScore=mean(BrowsingScore))
barplot(fir_reg_browse$BrowsingScore,names.arg=fir_reg_browse$Ecoregion,xlab="Ecoregion",ylab="Average Browsing Score",main="Average Browsing Score of the Balsam Fir by Ecoregion",col="purple",cex.names=0.3)
spruce_reg_browse<-sap_clean%>%
filter(Species=="Black_Spruce")%>%
group_by(Ecoregion)%>%
summarise(BrowsingScore=mean(BrowsingScore)) 
barplot(spruce_reg_browse$BrowsingScore,names.arg=spruce_reg_browse$Ecoregion,xlab="Ecoregion",ylab="Average Browsing Score",main="Average Browsing Score of the Black Spruce by Ecoregion",col="pink",cex.name=0.2)
#The Black Spruce has a generally lower Browsing Score in all ecoregions when compared to the Balsam Fir, but has a higher score in the Western Forests.
sap_reg_tally<-sap_clean%/%
group_by(Ecoregion)%>%
tally()%>%
print()
#"n" this table shows that a different amount of trees were counted in each ecoregion.
sap_spe_tally<-sap_clean%>%
group_by(Species)%>% 
tally()%>%  
print()
#"n" in this table shows that a different amount of saplings were collected for each species.
#This sapling study is not evenly distributed, there is sampling bias. There are variations in the amout of samples taken in each species and ecoregion.The North Shore forests were over represented while Strait of Belle Isle Barrens were underrepresented.
#Recognizing sampling bias is important because you must ensure the data is accurate in its portrayl and samples are taken fairly.
#Part 3- Creating and Joining Datasets
moose_2020b<-moose_clean%>%
filter(Year=="2020")%>%
mutate(MooseDensity=Estimated_Moose_Pop/Area)  
moose_sap<-left_join(moose_2020b,sap_clean,by='Ecoregion',relationship="many-to-many")
sum_spe_browse<-moose_sap%>%
group_by(Species,Ecoregion)%>%  
summarise(BrowsingScore=mean(BrowsingScore),MooseDensity=mean(MooseDesity))%>%
print()
#According to the figure plot:
#a) When moose density increases, browsing score of all species increases. This means when there is more competition, browsimng is less selective.
#b) The most browsed (preferred) species is willow, the least is Black Spruce
#c) Black ash is not present because it was only to be measured in each ecoregion.
collisions_2020<-c(56,60,14,36,48,10,40,110,6)
human_pop<-c(18000,12000,4000,75100,24000,3500,3200,270000,2300)
study_sites<-c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
moose_coll<-data.frame(collisions_2020,human_pop,study_sites)
moose_coll2<-rename(moose_coll,Ecoregion=study_sites)
#rename with function suggests it's unstable
coll_merge<-left_join(moose_2020, moose_coll2,by="Ecoregion",relationship="many-to-many")
plot(coll_merge$MooseDensity, coll_merge$collisions_2020,type="p",xlab="Moose Density",ylab="Collisions in 2020",main="Collisions in 2020 v.s. Moose Density")
#According to the scatterplott, areas with higher moose density generally had more collisions in 2020 (go figure)
coll_merge_per_capita<-mutate(coll_merge,coll_per_capita=collisions_2020/human_pop)
plot(coll_merge_per_capita$coll_per_capita,coll_merge_per_capita$human_pop,type="p",xlab="Collisions per Capita",ylab="Human Population",main="Collisions per Capita v.s Human Population")
#Accprding to the scatterplot, there are more collisions per capita as the human population decreases. Rural communities have higher moose populations therefore this information makes sense.