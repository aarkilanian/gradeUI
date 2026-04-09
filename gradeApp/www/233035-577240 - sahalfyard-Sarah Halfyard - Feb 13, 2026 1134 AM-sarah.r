# Install dyplr Package 
install.packages("dplyr")

#Libraries used
library(dplyr)

#Loading moose data
moosedata <- read.csv("MoosePopulation.csv")

#View moose data 
View(moosedata)

#Clean moose data
moose_clean <- na.omit(moosedata)

#Summary 
summary(moosedata)
str(moosedata)
View(moosedata)
moose_clean <- na.omit(moosedata)
View(moose_clean)
moose_clean <- na.omit(moosedata) 
View(moose_clean) 

#Column of Selected Interest
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop) 

#Oldest Recorded Year
year_min <- min(moose_sel$Year) 

#Highest Population of Moose Recorded 
moose_max <- max(moose_sel$Estimated_Moose_Pop) 

#Calculate Moose Density 
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area) 

#Plot of Moose Density Over-Time
plot(moosedata2$Year, moosedata2$MooseDensity,xlab = "Year",ylab = "Moose per sq km",main = "Moose Density in Newfoundland Ecoregions Over Time") 

#Filter Western Forests Only 
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests") 

#Plot Western Forests, Line Graph 
plot(moose_west$Year, moose_west$MooseDensity,type = "l",xlab = "Year",ylab = "Moose per sq km",main = "Moose Density in Western Forests Over Time") 

#Filter Year 2020
moose_2020 <- filter(moosedata2, Year == 2020) 

#Filter Density
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0) 

#Arrange in Decreasing Manor
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity)) 

#Using Pipes / Final
moosefinal <- moosedata2 %>%filter(Year == 2020) %>%filter(MooseDensity > 2.0) %>%arrange(desc(MooseDensity)) %>%print() 


#Average Browsing by Ecoregion
sap_reg_browse <- sap_clean %>%group_by(Ecoregion) %>%summarize(AverageBrowsing = mean(BrowsingScore)) %>%print() 
avg_browse_reg <- arrange(sap_reg_browse, desc(AverageBrowsing)) 
#Northern_Peninsula_forests had highest average browsing
#StraightOfBelleIslBarrens had lowest average browsing 

#View Browsing
View(avg_browse_reg) 

#Average Tree Height by Ecoregion 
sap_reg_height <- sap_clean %>%group_by(Ecoregion) %>%summarize(AverageHeight = mean(Height)) %>%print() 
sap_reg_height_low <- sap_reg_height %>%filter(AverageHeight < 20) %>%print() 

#Browsing by Tree Sampling Species
sap_spe_browse <- sap_clean %>%group_by(Species) %>%summarize(AverageBrowsing = mean(BrowsingScore)) %>%print() 

#Highest and Lowest Browsing by Species
avg_browse_spe <- sap_spe_browse %>%arrange(desc(AverageBrowsing)) %>%print() 
#Black_ash has Highest Browsing 
#Black_spruce has Lowest Browsing 

#Balsam_fir Browsing Intensity
fir_reg_browse <- sap_clean %>%filter(Species == "Balsam_Fir") %>%group_by(Ecoregion) %>%summarize(AverageBrowsing = mean(BrowsingScore)) %>%print() 

#Dataset Balsam_fir Bar Graph 
barplot(fir_reg_browse$AverageBrowsing,names.arg = fir_reg_browse$Ecoregion,xlab = "Ecoregion",ylab = "Average Browsing Score",main = "Average Balsam Fir Browsing by Ecoregion",col = "forestgreen",cex.names = 0.6) 

#Black Spruce Intensity 
spruce_reg_browse <- sap_clean %>%filter(Species == "Black_Spruce") %>%group_by(Ecoregion) %>%summarize(AverageBrowsing = mean(BrowsingScore)) %>%print() 

#Dataset Black_spruce Bar Graph
barplot(spruce_reg_browse$AverageBrowsing,names.arg = spruce_reg_browse$Ecoregion,xlab = "Ecoregion",ylab = "Average Browsing Score",main = "Average Black Spruce Browsing by Ecoregion",col = "lightgreen",cex.names = 0.6) 
#Black_Spruce Shows Lower Browsing Activity 
#Balsam_fir Shows Higher Intensity in Browsing Activity, this Suggests Moose May Have a Preference on Sapling Species 

#Number of Saplings in Each Ecoregion (Tally)
sap_reg_tally <- sap_clean %>%group_by(Ecoregion) %>%tally() %>%print() 

#Number of Saplings for Each Species 
sap_spe_tally <- sap_clean %>%group_by(Species) %>%tally() %>%print() 

# Some Ecoregions/Species were Sampled More Than Others      
#Suggesting That the Data Was Not Evenly Distributed. May Be Due to Commonly, or Differences in Sample Sizes

#It is Important to Recognize Sampling Bias because Uneven Representation Can Alter How We View and Understand Moose Browsing Patterns

#Creating Density & Filtering 2020
moose_2020b <- moose_clean %>%filter(Year == 2020) %>%mutate(MooseDensity = Estimated_Moose_Pop / Area) 

#Joining Data with Sapling Dataset 
moose_sap <- left_join(moose_2020b,sap_clean,by = "Ecoregion",relationship = "many-to-many") 

#Species and Ecoregion Summary 
sum_spe_browse <- moose_sap %>%group_by(Species, Ecoregion) %>%summarize(AvgBrowsing = mean(BrowsingScore),AvgDensity = mean(MooseDensity)) %>%print() 

#The Figure Supports the Researchers Hypothesis With Evidence From the ggplot 
#At Lower Moose Densities Browsing Widely Varies, Selectivity Shown Which Leads to Preferences. 
#Higher Density Browsing Scores are High/Higher Across More Species Suggesting Less Selective Feeding

#Most Moose Seem to Favour "Willow" the Most Because It Has Highest Browsing Score
#Black Spruce Seem to Least Favoured or Browsed the Least, Especially in Lower Moose Densities 

#Black Ash Was Not Recorded On the Figure Because There Were No Recorded Black Saplings in the Merged 2020 Dataset 
#Creating Numeric Vector (Moose Collisions in 2020) 

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6) 

#Creating Human Population Vector

human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500,32000, 270000, 2300) 

#Ecoregion Vector 

study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens") 

#Create Dataset 

moose_coll <- data.frame(collisions2020,human_pop,study_sites) 

#Rename and Rejoining 

moose_coll2 <- moose_coll %>%rename(Ecoregion = study_sites) 

#Joinied with 2020 Moose Dataset

coll_merge <- left_join(moose_2020b,moose_coll2,by = "Ecoregion") 

#Plot of Density vs Collisions 

plot(coll_merge$MooseDensity,coll_merge$collisions2020,xlab = "Moose Density",ylab = "Number of Collisions (2020)",main = "Moose Density vs Vehicle Collisions (2020)") 

#Ecoregions with Higher Moose Density Show an Increase in Moose Collision Accidents
#Showing that Moose Density and Vehicle Collisions Form a Relationship Between Them 
#Outliers Will be Found in Few Areas

#Collisions Per Capita 
coll_merge_per_capita <- coll_merge %>%mutate(coll_per_capita = collisions2020 / human_pop) 

#Highest Per Capita 
arrange(coll_merge_per_capita,desc(coll_per_capita)) 

#Collisions Per Capita vs Human Population 
plot(coll_merge_per_capita$human_pop,coll_merge_per_capita$coll_per_capita,xlab = "Human Population",ylab = "Collisions Per Person",main = "Collisions Per Capita vs Human Population") 

#Collision Rates Per Person Seem to be Higher in Less Populated Regions/Rural Areas, This Could be Due
#To the Fact that Less Populated Places Have More Moose Habitat Surrounding Them and Less Industrial Units.

