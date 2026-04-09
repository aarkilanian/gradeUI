install.packages("dplyr")
library("dplyr")
moosedata <- read.csv("MoosePopulation.csv")
View("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
year_min <- min("year")
moose_max <- max("41250")
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
moose_2020 <- filter(moosedata2, moosedata2$Year == "2020")
moose_2020_high <- filter(moosedata2,
                          moosedata2$MooseDensity >= 2.0 )
moose_2020_high_byD <- arrange(moosedata2,desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageBrowsing = mean(BrowsingScore))
sap_reg_browse
print(sap_reg_browse)
avg_browe_reg <- arrange(sap_reg_browse,desc(AverageBrowsing))
#highest:northern_pennisula forest #lowest:straitoffbelleislebarrens
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(AverageHeight = mean(height))
print(sap_reg_height)
#Northern_peninsula_forest and western_forest
sap_reg_height_low <- sap_clean %>% filter(Height<=20)
print(sap_reg_height_low)
sap_spe_browse <-sap_clean %>%
  group_by(species)%>%
  summarize(Averagebrowsing = mean(BrowsingScore))
print(sap_spe_browse)
avg_browse_spe<- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))
#highest:Browsing Score =Black_Ash Lowest:Browsing Score =Black spruce
fir_reg_browse <-sap_clean %>%
  filter(Species=="balsam_fir")
group_by(Ecoregion) %>%
  summarize(AverageBrowsing= mean(BrowsingScore))
barplot(fir_reg_browse$Height, names.arg fir_reg_browse= $, xlab = "Ecoregion", ylab = "Height", main = "Average Browsing Intensity of the Balsam Fir", col = "pink", 
spruce_reg_browse <- sap_clean %>%
  filter(species =="black_spruce")
group_by(Ecoregion)%>%
  summarize (AverageBRowsing = mean(BrowsingScore))%>%
  print()
#they are a lot alike such like, they both stay within 0.5-4.5 region. 
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally<- sap_reg_tally %>%
  group_by(Ecoregion) %>%
  tally(n) %>% 
  print() 
#for the most part its evenally distrubuted around five per species however staitofbelleislebarrens and maritime_barrens are underrepresented and north_shore_forest and northern_peninsula_forest are overrepresented.
#Its important because it can mess up with accuracy and reliability, and trusting these can cause bad management decisions.
moose_2020B <- moose_clean %>%
  filter(Year == 2020) %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020B, sap_clean,by ='Ecoregion', relationship = "many to many" )
sap_spe_browse<- sap_clean %>%
  group_by(species,Ecoregion)%>%
  summarize(AverageBrowsing =mean(BrowsingScore))
print(sum_spe_browse)
#the figure shows moose are strong at low density and shift to more generalist browsing at high density.
#moose favor Willow the most and White Birch the least
#Lack_ash,they dont show all saplings cause they only studied the ones relevant.
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")          
moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll%>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_2020, moose_coll2, by = "Ecoregion")
plot(coll_merge$MooseDensity, collmerge$collisions2020,
     xlab = "Moose Density"
     ylab = "Number of Moose-Vechicle Colloisions"
     main = "Moose Density vs Moose-Vechicle Collisions")
#26b) theres a relationship between moose density and the number of moose vechicle collisions 
#whennthe moose density is high there is a trend of higher moose vechicle colloisions
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita) = collisions2020 / human_pop)
#27. long barren have the highest number of moose collisions per person because
#there was 14 collisions in 2020 with a population of only 4000.
plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,
     xlab = "Human Population"
     ylab = "Moose Collision Per Capita",
     main = "Moose Collision per Capita vs Human population")
#29 theres seems to be a postive correltion between moose and human population.


