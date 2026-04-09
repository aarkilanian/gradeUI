# Install Dyplyr package 
install.packages("dplyr")

# Libraries used
library(dplyr)
# Where workings are located
getwd()

#Load moose data
moosedata <- read.csv("MoosePopulation.csv")

# View moose data
View(moosedata)

# Moose data excluding NAs
moose_clean <- na.omit(moosedata)
moose_clean <- na.omit(moosedata)

summary(moosedata)
str(moosedata)
View(moosedata)
moose_clean <- na.omit(moosedata)
View(moose_clean)

# Select column of Interest
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Oldest year recorded
year_min <- min(moose_sel$Year)

# Highest Moose population recorded
moose_max <- max(moose_sel$Estimated_Moose_Pop)

# Calculate Moose density
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Plot moose density over-time 
plot(moosedata2$Year, moosedata2$MooseDensity,xlab = "Year",ylab = "Moose per sq km",main = "Moose Density in Newfoundland Ecoregions Over Time")

#Filter only western forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#Plot western forests, line graph 
plot(moose_west$Year, moose_west$MooseDensity,type = "l",xlab = "Year",ylab = "Moose per sq km",main = "Moose Density in Western Forests Over Time")

#Filter for year 2020
moose_2020 <- filter(moosedata2, Year == 2020)

#Filter for density
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)

#arrange in a decreasing manor  
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Using pipes / final 
moosefinal <- moosedata2 %>%filter(Year == 2020) %>%filter(MooseDensity > 2.0) %>%arrange(desc(MooseDensity)) %>%print()


#average browsing by ecoregion 
sap_reg_browse <- sap_clean %>%group_by(Ecoregion) %>%summarize(AverageBrowsing = mean(BrowsingScore)) %>%print()
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
#Northern_Peninsula_forests had highest average browsing 
#StraightOfBelleIslBarrens had lowest average browsing 

#View browsing 
View(avg_browse_reg)

#average tree height by ecoregion 
sap_reg_height <- sap_clean %>%group_by(Ecoregion) %>%summarize(AverageHeight = mean(Height)) %>%print()
sap_reg_height_low <- sap_reg_height %>%filter(AverageHeight < 20) %>%print()

#Browsing by tree sapling species 
sap_spe_browse <- sap_clean %>%group_by(Species) %>%summarize(AverageBrowsing = mean(BrowsingScore)) %>%print()

#Highest browsing and lowerest browsing by species 
avg_browse_spe <- sap_spe_browse %>%arrange(desc(AverageBrowsing)) %>%print()
#Black_ash has the highest browsing 
#Black_spruce has the lowest browsing 

#Balsam_fir browsing intensity
fir_reg_browse <- sap_clean %>%filter(Species == "Balsam_Fir") %>%group_by(Ecoregion) %>%summarize(AverageBrowsing = mean(BrowsingScore)) %>%print()

#Dataset Balsam_fir Bar Graph 
barplot(fir_reg_browse$AverageBrowsing,names.arg = fir_reg_browse$Ecoregion,xlab = "Ecoregion",ylab = "Average Browsing Score",main = "Average Balsam Fir Browsing by Ecoregion",col = "forestgreen",cex.names = 0.6)

#Black spruce intensity 
spruce_reg_browse <- sap_clean %>%filter(Species == "Black_Spruce") %>%group_by(Ecoregion) %>%summarize(AverageBrowsing = mean(BrowsingScore)) %>%print()

#Dataset Black_spruce Bar Graph
barplot(spruce_reg_browse$AverageBrowsing,names.arg = spruce_reg_browse$Ecoregion,xlab = "Ecoregion",ylab = "Average Browsing Score",main = "Average Black Spruce Browsing by Ecoregion",col = "seagreen",cex.names = 0.6)
#black_spruce shows lower browsing activity 
#Balsam_fir shows higher intensity in browsing activity, this suggests moose may have a preference on sapling species 

#Number of saplings in each ecoregion (tally)
sap_reg_tally <- sap_clean %>%group_by(Ecoregion) %>%tally() %>%print()

#Number of saplings for each species 
sap_spe_tally <- sap_clean %>%group_by(Species) %>%tally() %>%print()

# Some ecoregions/species were sampled more than others      
# suggesting that the data was not evenly distributed. may be due to commonly,or differences in sample sizes

#it is important to recognize sampling bias because uneven representation can alter how we view and understand moose browsing patterns

# Creating density & filtering 2020
moose_2020b <- moose_clean %>%filter(Year == 2020) %>%mutate(MooseDensity = Estimated_Moose_Pop / Area)

# Joining data with sapling dataset 
moose_sap <- left_join(moose_2020b,sap_clean,by = "Ecoregion",relationship = "many-to-many")

# Species and ecoregion summary 
sum_spe_browse <- moose_sap %>%group_by(Species, Ecoregion) %>%summarize(AvgBrowsing = mean(BrowsingScore),AvgDensity = mean(MooseDensity)) %>%print()

# The figure supports the researchers hypothesis with evidence from the ggplot 
# At lower moose densities browsing widely varies, selectivity shown which leads to preferences. 
# Higher density browsing scores are high/higher across more species suggesting less selective feeding

# Most moose seem to favour "Willow" the most because it has highest browsing score
# Black spruce seem to least favoured or browsed the least, especially in lower moose densities 

#Black ash was not recorded on the figure because there were no recorded Black Saplings in the merged 2020 dataset 
# Creating numeric vector (Moose collisions in 2020) 

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

# creating Human Population vector

human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500,32000, 270000, 2300)

# Ecoregion vector 

study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

#Create Dataset 

moose_coll <- data.frame(collisions2020,human_pop,study_sites)

#Rename and Rejoining 

moose_coll2 <- moose_coll %>%rename(Ecoregion = study_sites)

#Joinied with 2020 Moose dataset

coll_merge <- left_join(moose_2020b,moose_coll2,by = "Ecoregion")

#Plot of density vs collisions 

plot(coll_merge$MooseDensity,coll_merge$collisions2020,xlab = "Moose Density",ylab = "Number of Collisions (2020)",main = "Moose Density vs Vehicle Collisions (2020)")

#Ecoregions with higher moose density show an increase in moose collision accidents
# showing that Moose density and Vehicle collisions form a relationship between them 
#Outliers will be found in few areas

#Collisions per Capita 
coll_merge_per_capita <- coll_merge %>%mutate(coll_per_capita = collisions2020 / human_pop)

#Highest per Capita 
arrange(coll_merge_per_capita,desc(coll_per_capita))

#Collisions per Capita vs Human Population 
plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita,xlab = "Human Population",ylab = "Collisions Per Person",main = "Collisions Per Capita vs Human Population")

# Collision rates per person seem to be higher in less populated regions/rural areas, this could be due
#to the fact that that less populated places have more moose habitat surrounding them and less industrial units.




