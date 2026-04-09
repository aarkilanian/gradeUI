# Title: R assignment
#Author: Nina Bryson
#Date: 07-02-2026

#Set working directory
setwd("/cloud/project/R assignment/")

#Load dplyr
install.packages("dplyr")

#Load libraries needed
library(dplyr)

#Load data
moosedata <- read.csv("MoosePopulation.csv")

#Remove missing values
moose_clean <- na.omit(moosedata)

#Assign columns of interest
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Finding oldest year
min("Year")
year_min <- c("1904")

#Finding Highest estimated moose population recorded
max("Esitmated_Moose_Pop")
moose_max <- c("41250")

#Calculating moose density for each ecoregion
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Visualizing data
plot(moosedata2$Year,moosedata2$MooseDensity,xlab="year",ylab="Moose per sq km",main="Moose density in Newfoundland ecoregions over time")

#New dataset for only western forests
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

#line graph
plot(moose_west$Year, moose_west$MooseDensity,type = "l", xlab = "Year",ylab = "Moose per sq km", main = "Moose density change over time in western forest regions")

#Filtering for 2020
moose2020 <- filter(moosedata2, Year == "2020")

#Filtering for moose density greater that 2.0
moose_2020_high <- filter(moose2020, MooseDensity > 2.0)

#Sorting moose density
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#Loading library
library(dplyr)

#adding pipes
moose2020 <- filter(moosedata2, Year == "2020")%>%
moose_2020_high <- filter(moose2020, MooseDensity > 2.0)%>%
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity))
print(moosefinal)

#Loading sapling data
sapling <- read.csv("SaplingStudy.csv")

#Omiting NA
sap_clean <- na.omit(sapling)

#Creating new database
sap_reg_browse <- sap_clean %>%
group_by(Ecoregion) %>%
summarise(mean(BrowsingScore))
print(sap_reg_browse)

#Creating new data set arranging average browsing score
avg_browse_reg <- sap_reg_browse %>%
arrange(desc(`mean(BrowsingScore)`))
print(avg_browse_reg)     
#Strait of belle isle barrens has the lowest score of 1
#Norther Peninsula Forest has the highest score of 4.57

#Creating new data set averaging tree height in different ecoregions
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>%
summarise(mean(Height))
print(sap_reg_height)

#New data set of average height less than 20cm
sap_reg_height_low <- sap_reg_height %>%
filter(`mean(Height)` < 20)
print(sap_reg_height_low)
#Northern peninsula forest and western forested are the most browsed by moose.

#New dataset for average browsing across different sappling species
sap_spe_browse <- sap_clean %>%
group_by(Species) %>%
summarise(mean(BrowsingScore))
print(sap_spe_browse)

#rearranging dataset for highest and lowest browsing 
avg_browse_spe <- sap_spe_browse %>%
arrange(desc(`mean(BrowsingScore)`))
print(avg_browse_spe) 
#Black Ash has highest browsing score and Black Spruce has lowest browsing score

#Filtering for Balsam fir
fir_reg_browse <- sap_clean %>%
group_by(Ecoregion)  %>%
filter(Species == "Balsam_Fir")
mean(BrowsingScore)
print(fir_reg_browse)

#Creating bar plot 
barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "average browsing intensity", main = "Average browsing intensity of Balsam fir in different ecoregions", col = "forestgreen", cex.names = 0.6)

#Filtering for Black Spruce
spruce_reg_browse <- sap_clean %>%
group_by(Ecoregion)  %>%
filter(Species == "Black_Spruce")
mean(BrowsingScore)
print(spruce_reg_browse)

#Creating bar plot
barplot(spruce_reg_browse$BrowsingScore, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "average browsing intensity", main = "average browsing intensity of Black Spruce in different ecoregions", col = "forestgreen", cex.names = 0.6)

#The North Shore Forest has the height browsing intensity for both the Black Spruce and Balsam fir.
#In the Maritimes Barrens there is a higher browsing intensity of Balsam firs then Black Spruce.

#Tallying for number of tree saplings counted in each Ecoregion
sap_reg_tally <- sap_clean %>%
group_by(Ecoregion) %>%
tally()  
print(sap_reg_tally)

#Tallying for species
sap_spe_tally <- sap_clean %>%
group_by(Species) %>%
tally()  
print(sap_spe_tally)

#A) The SaplingStudy dataset is uneveningly distributed. The North Shore forest has the most representation in the set, 8 times more then the Strait of belle isle barrens which is very underrepresented.  
#A)The Balsam Fir is extremely overrepersented and the Black ash species is very underrepresented.
#B)Bias in ecological dataset is important to note because it throws off the accurancy of the data being anaylzed. 

#Creating new dataset
moose_2020b <- moose_clean %>%
filter(Year==2020) %>%  
mutate(MooseDensity = Estimated_Moose_Pop / Area)
print(moose_2020b)

#Joining datasets
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

#calculating average browsing score and moose density for each species in each ecoregion
sum_spe_browse <- moose_sap %>%
group_by(Ecoregion) %>%
summarise(mean(BrowsingScore), mean(MooseDensity))
print(sum_spe_browse)

#Answering graph questions
#A) Yes there is evidence the suppores researcher hypothesis. At low density the browsing scores differ among species showing preference.
#At higher densities the scores gather together and become consitient across species, showing more generalized browsing.
#B) Moose seem to favor Willow and Alder and don't like Black Spruce which has the lowest browsing score.
#C) Black Ash is not seen in the graph.

#Adding Vectors
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Correcting dataset names
moose_coll2 <- rename(moose_coll, Ecoregion = study_sites)

#Joining datasets
coll_merg <- left_join(moose_coll2, moose2020, by = 'Ecoregion', relationship = "many-to-many")

#ploting data
plot(y= coll_merg$collisions2020, x=coll_merg$MooseDensity, main = "Moose density and number of moose vechicle collusions in 2020", ylab= "number of moose vechicle collusions", xlab = "Moose Density", pch= 19, col="pink")
#I see a upward trend with the higher moose density has higher rates of collusions. There is a outlier with 1.0 moose density showing over 100 colluisons.

#Creating new column
coll_merge_per_capita <- mutate(coll_merg, coll_per_capita = collisions2020 / human_pop)

#ploting data
plot(x =coll_merge_per_capita$human_pop, y=coll_merge_per_capita$coll_per_capita, main = "collusions caused by moose per captia verus human population", xlab = "Human Population", ylab = "Colluisons per capita", pch= 19, col="coral")
#I see a trend that there are more collusions in areas with less humans, this makes sense as there are not as many moose in urban spaces and there is in rural spaces.
