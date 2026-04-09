#Spencer_T_King
#Biol_1002
#Feb_13th_2026

#Q1
library(dplyr)

#Q2
moosedata<-read.csv("~/Downloads/MoosePopulation.csv")

#Q3
moose_clean<-na.omit(moosedata)

#Q4
moose_sel<-select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Q5A
min_year<-min(moose_sel$Year)
min_year

#Q5B
max_moose<-max(moose_sel$Estimated_Moose_Pop)
max_moose

#Q6
moosedata2<-mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Q7
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab="year", 
     ylab="Moose per sq km", 
     main="Moose density in Newfoundland ecoregions over time")

#Q8A
moose_west<-filter(moosedata2, Ecoregion == "Western_Forests")

#Q8B
plot(moose_west$Year, moose_west$MooseDensity, type = "l",
     xlab="year", 
     ylab="Moose per sq km", 
     main="Moose density in Western Forests over time")

#Q9A
moose_2020<-filter(moosedata2, Year == 2020)

#Q9B
moose_2020_high<-filter(moose_2020, MooseDensity > 2)

#Q9C
moose_2020_high_byD<-arrange(moose_2020_high, desc(MooseDensity))

#Q10
moosefinal<-moosedata2%>%
  filter(Year == 2020)%>%
  filter(MooseDensity > 2.0)%>%
  arrange(desc(MooseDensity))%>%
  print()

#Q11A
saplings<-read.csv("~/Downloads/SaplingStudy.csv")

#Q11B
sap_clean<-na.omit(saplings)

#Q12A
sap_reg_browse<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarize(mean(BrowsingScore))%>%
  print()

#Q12B
avg_browsing_reg<-sap_reg_browse%>%
  arrange(desc(`mean(BrowsingScore)`))%>%
  print()
#Northern_Peninsula_Forests: Highest
#StraitOfBelleIsleBarrens: Lowest

#Q13A
sap_reg_height<-sap_clean%>%
  group_by(Ecoregion)%>%
  summarize(mean(Height))%>%
  print()

#Q13B
sap_reg_height_low<-sap_reg_height%>%
  filter(`mean(Height)`<20)%>%
  print()
#Northern_Peninsula_Forests: <20
#Western_Forests: <20

#Q14A
sap_spe_browse<-sap_clean%>%
  group_by(Species)%>%
  summarize(mean(BrowsingScore))%>%
  print()

#Q14B
avg_spe_browse<-sap_spe_browse%>%
  arrange(desc(`mean(BrowsingScore)`))%>%
  print()
#Black_Ash: Highest
#Black_Spruce: Lowest

#Q15
fir_reg_browse<-sap_clean%>%
  filter(Species == "Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarize(mean(BrowsingScore))%>%
  print()

#Q16
barplot(fir_reg_browse$`mean(BrowsingScore)`, 
        names.arg = fir_reg_browse$Ecoregion, 
        xlab="Ecoregion", 
        ylab="Balsam Fir Browsing Score", 
        main="Balsam Fir Browsing Scores in different ecoregions",
        col="forestgreen",
        cex.names=0.6)

#Q17A
spruce_reg_browse<-sap_clean%>%
  filter(Species == "Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarize(mean(BrowsingScore))%>%
  print()

#Q17B
barplot(spruce_reg_browse$`mean(BrowsingScore)`, 
        names.arg = spruce_reg_browse$Ecoregion, 
        xlab="Ecoregion", 
        ylab="Black Spruce Browsing Score", 
        main="Black Spruce Browsing Scores in different ecoregions",
        col="black",
        cex.names=0.6)

#Q17C
#Balsam Fir is browsed in a greater number of ecoregions than Black Spruce
#On average, in the ecoregions where it is browsed, Balsam Fir scores higher than Black Spruce

#Q18
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Q19
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Q20A
#The SaplingStudy dataset is not evenly distrubuted 
#North_Shore_Forests and Northern_Peninsula_Forests are overrepresented, as are Balsam_Fir
#StraitOfBelleIsleBarrens and Maritime_Barrens are underrepresented, as are Black_Ash

#Q20B
#Bias in ecological datasets can create misleading results, creating a false depicting of an area's ecosystem
#This can have potential real world consequences as misleading datasets can result in conservation efforts being allocated to places they may not be needed and not to those where they are

#Q21A
moose_2020b <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

#Q21B
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', 
                       relationship = "many-to-many")

#Q22
sum_spe_browse<-moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(mean(BrowsingScore), mean(MooseDensity)) %>%
  print()

#Q23A
# Yes, there is evidence that supports the researchers initial hypothesis
# As the moose density increases, the disparity between tree browsing scores decreases and the trees are consumed at a more uniform rate

#Q23B
# The saplings most browsed by the moose are Willows and Alders
# The saplings least browsed by the moose are Black Spruce, whilst White Birch and Balsam Fir are both consistently near the bottom

#Q23C
# The species not present on this plot is the Black Ash sapling
# This is likely a result of the severe under-representation of this species, where only one sample was observed

#Q24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Q25A
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)

#Q25B
coll_merge <- left_join(moose_coll2, moose_2020, by = 'Ecoregion', 
                        relationship = "many-to-many")

#Q26A
plot(coll_merge$MooseDensity, coll_merge$collisions2020, 
     ylab="Collisions in 2020", 
     xlab="Moose per sq km", 
     main="Relation between collisions in 2020 and moose density")

#Q26B
# The primary trend is that as the moose density in an area increases, so too does the amount of collisions
# An outlier to this trend would be the point for Avalon_Forests, where with a moose density of 1 per sq km, there was 100+ collisions in 2020

#Q27
coll_merge_per_capita<-mutate(coll_merge, coll_per_capita = collisions2020 / human_pop)

#Q28
plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop, 
     ylab="Human population", 
     xlab="Collisions per capita", 
     main="Relation between collisions per capita and human populations")

#Q29
# The general trend is that areas with larger populations have less collisions per capita
# This does align with population trends in NL, as areas with higher populations are generally urban, and would not be in the vicinity of places where collisions are likely
# On the other hand, areas with lower population are generally rural, and their surroundings are generally more conducive of moose collisions