# Title: The Moose Populations in Newfoundland
# Author: Avery Ponticelli
# Date: 13-02-2026

#Part 1: Moose Populations in Newfoundland

# Question 1
# Load dplyr (via tidyverse)
# --------------------------------------------------
# Install packages (only run once)
install.packages("tidyverse")

# Load the library
library(tidyverse)

# Question 2
# Import the MoosePopulation dataset
# --------------------------------------------------

# Read the CSV file and name it moosedata
moosedata <- read.csv("MoosePopulation (1).csv")


# Question 3
# remove rows with missing values
# --------------------------------------------------
moose_clean <- na.omit(moosedata)

# Question 4
# simplify the dataset to only include the columns of interest
# --------------------------------------------------
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5
# a. What is the oldest observation in the dataset?
# b. Use the max() function to what is the highest ‘Estimated_Moose_Pop’ recorded? Save the result of max() as moose_max.
# --------------------------------------------------
#a
year_min <- min(moose_sel$Year)

#b
year_max <- max(moose_sel$Year)

# Question 6
# calculating moose density for each ecoregion
# --------------------------------------------------
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

# Question 7
# visualize the data
# --------------------------------------------------
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")

# Question 8
# Create a new dataset, where you use the filter() function to only include observations from the Western_Forests ecoregion. Save the result as moose_west.
# --------------------------------------------------
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 9
# a. Using the original, unfiltered dataset you created in Question 6 above, use the filter() function to filter for the year 2020, and save the dataset as moose_2020
# b. Using the dataset you just created, filter() the MooseDensity to only show ecoregions where moose density is greater than 2.0. Save the dataset as moose_2020_high
# c.With the dataset you just created, use the arrange() function to sort the MooseDensity column in descending order. Save the result as moose_2020_high_byD.
# --------------------------------------------------
#a. 
moose_2020 <- filter(moosedata, Ecoregion == "Western_Forests")

#b. 
moose_2020_high <- filter(moosedata2, MooseDensity > "2.0")

#c.
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

# Question 10
# repeat step but use pipes
# --------------------------------------------------
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

#________________________________________________________________________________________________________________________________________________________
#part 2: Tree sapling study

# Question 11
# a. load csv
# b. remove na
# --------------------------------------------------
#a.
saplings<- read.csv("SaplingStudy.csv")

#b.
sap_clean<-na.omit(saplings)

# Question 12
# a.create a new database using the group_by() function to group the data by Ecoregion, and then use summarize() to calculate the mean() moose BrowsingScore for each site. Print the result using the print() function. Save the result as sap_reg_browse
# b.Rearrange your dataset in order of decreasing average browsing score
# --------------------------------------------------
# a. 
sap_reg_browse<-sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

#b. 
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(`mean(BrowsingScore)`))%>%
  print()
#The region with the highest browsing score is Northern Peninsula Forests and the one with the lowest is Strait of Belles is Barrnes

# Question 13
# a. use the group_by function to group the data by Ecoregion, and then use summarize() to calculate the mean() tree Height for each group. Print the result using the print() function. Save the result as sap_reg_height
# b. The team considered average heights less than 20 cm to be severely browsed by moose. Add a short comment # to describe which ecoregions have average heights less than 20 cm. Do this by looking at your data or using a filter(). Print the result using the print() function. Save the result as sap_reg_height_low.
# --------------------------------------------------
#a. 
sap_reg_height<-sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(mean(Height))%>%
  print()
#b. 
sap_reg_height_low <- sap_reg_height%>%
  filter(`mean(Height)`< "20")%>%
  print()
# Northern Peninsula Forests and Western Forests have trees have samller than 20cm

# Question 14
# a. Use the group_by function to group the data by Species, and then use summarize() tocalculate the mean tree BrowsingScore for each group. Print the result using the print() function. Save the result as sap_spe_browse.
# b. Which species have the highest and lowest browsing? Rearrange the data according to decreasing mean browsing score. Save the rearranged results as avg_browse_spe. Add a comment # saying which species has the highest browsing score, which species has the lowest?
# --------------------------------------------------
#a. 
sap_spe_browse<-sap_clean %>%
  group_by(Species) %>%
  summarize(mean(BrowsingScore)) %>%
  print()

#b. 
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(`mean(BrowsingScore)`))%>%
  print()
#the highest browsing score is black ash and the lowest one was black spruce

# Question 15
# pipes %>%, use the filter() function to filter the Species column for only Balsam_Fir, then use group_by() function to sort by Ecoregion , and then determine mean() moose BrowsingScore. Save the resulting summary table as fir_reg_browse
# --------------------------------------------------

fir_reg_browse <- sap_clean %>%
  filter(Species=="Balsam_Fir")%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()

# Question 16 
# make bar plot
# ------------------------------------------------------
barplot(fir_reg_browse$`mean(BrowsingScore)`, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Mean of Balsam Fir Browsing ", main = "Mean of Balsam Fir Browsing per Ecoregions", col = "blue",cex.names = 0.6)  

# Question 17 
# Repeat the steps from question 15 and 16 but now with Black Spruce.
# ------------------------------------------------------
#a.
spruce_reg_browse<- sap_clean%>%
  filter(Species=="Black_Spruce")%>%
  group_by(Ecoregion)%>%
  summarise(mean(BrowsingScore))%>%
  print()

#b. 
barplot(spruce_reg_browse$`mean(BrowsingScore)`, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Mean of Black Spruce Browsing ", main = "Mean of Black Spruce Browsing per Ecoregions", col = "yellow",cex.names = 0.6)  

#c. over all Balsam fir show less browsing from moose when compaired to black spruce, however black spruce do marginly better in some regions.

# Question 18 
# determine how many trees were counted in each Ecoregion
# ------------------------------------------------------
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

# Question 19 
# determine how many individual trees were counted in for each Species
# ------------------------------------------------------
sap_spe_tally<- sap_clean %>%
  group_by(Species) %>%
  tally() %>% 
  print()

# Question 20 
# a. Add a short comment # saying if you think the SaplingStudy dataset is evenly distributed. Are any ecoregion(s) or tree species overrepresented, are any underepresented in the dataset.
# b. Why is it important to recognize bias in ecological datasets. Write 1-2 sentences as a comment # with your answer.
# ------------------------------------------------------
#a. For the most part the data sheet SaplingStudy was well distruibuted however I believe that balsam firs were over reported and black ash trees were under reported. 

#b. It is important to recognize bias in ecological datasets because bisa data can distort the true picture of an ecosystem.

#________________________________________________________________________________________________________________________________________________________
# part 3: Creating and Joining Datasets

# Question 21 
#a. Using the original moose_clean dataset, filter() function to select only the rows for the year 2020. Then create a new column called MooseDensity using the mutate() function. Save the dataset under a new name called moose_2020b. (HINT: you previously did this for question 9)
#b. Use left_join() to join moose_2020b with the sap_clean dataset, matching rows by the common Ecoregion column. Save the result as moose_sap.
# ------------------------------------------------------
#a. 
moose_2020a<- filter(moose_clean, Year == "2020")
moose_2020b<- mutate(moose_2020a, MooseDensity= Estimated_Moose_Pop/Area )

#b.
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

# Question 22
# calculate the average browsing score and average moose density for each species within each ecoregion.
# ------------------------------------------------------
sum_spe_browse<- moose_sap%>%
  group_by(Species, Ecoregion)%>%
  summarize(mean(BrowsingScore), mean(MooseDensity))%>%
  print()

#Question 23
#a. Is there evidence that supports the researchers’ hypothesis? Do moose show strong preferences at low density and shift to more generalist browsing at higher density? Add a short comment (1-2 sentences) with your answer.
#b. Which sapling specie(s) do moose favour the most? Which do they browse the least? Add a short comment (1-2 sentences) with your answer.
#c. Which sapling species is not shown on the figure and why? Add a short comment (1-2 sentences) with your answer.
#---------------------------------------------------------------------------------
#a. Yes it appers that the hypothis is correct. Moose do show strong preferences at low density and shift to more generalist browsing at higher density. However this may be due to the fact that they know where the perferable trees are therfore they will congragate there more frequently.
#b. The moose seems to prefer the Alder tree over there other species of trees in the region. They often do not prefer the black spruce trees.
#c. The sppecies of saplings not shown on the figure is the black ash trees. This is due to it only having one data point and would not show any change over the graph.

#question 24
#add a line of code (example given below) where you use the data.frame() function to create a dataset using your vectors. Save this dataset as moose_coll.
#----------------------------------------------------------------------------------------

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#Question 25
#a. Rename the column holding site information in the moose_coll dataset and save the renamed result as moose_coll2
#b. join the datasets
#------------------------------------------------------------------------------------
#a.
moose_coll2<-moose_coll%>%
  rename(Ecoregion = study_sites)

#b.
coll_merge <- left_join(moose_coll2, moose_2020b, by = 'Ecoregion', relationship = "many-to-many")

#Question 26
#a. How does moose density relate to the number of moose-vehicle collisions? Use the plot() function to create a scatterplot of MooseDensity and collisions2020
#b. What trends do you see? Are there any outliers? Write 1-2 sentences as a comment.
#-----------------------------------------------------------------------------------
#a. 
plot(coll_merge$MooseDensity, coll_merge$collisions2020, xlab="Moose Density", ylab= "collisions in 2020", main="Moose Density vs. Collionsion in 2020",)

#b. There is an upwarder trend, as the density increases the ammount of collision increase. Yes there is an outlier, when there is a density of 1 moose there is over 100 collision. 

#Question 27 
#Create a new column called coll_per_capita that is equal to collisions2020 divided by human_pop
#-----------------------------------------------------------------------------------
coll_per_capita <-mutate(coll_merge, coll_merge_per_capita=collisions2020/human_pop)

#Question 28 
#Use the plot() function to create a scatterplot of coll_per_capita versus human_pop
#-----------------------------------------------------------------------------------
plot(coll_per_capita$human_pop, coll_per_capita$coll_merge_per_capita, xlab="Human Population", ylab="Collisons Per Capita", main="Collisons per Capaita vs. Human Population")


#Question 29
#Write 1-2 sentences describing what trends you see. Does this trend make sense based on what you know about moose and human populations in Newfoundland?
#-----------------------------------------------------------------------------------------
#The trend shows a decreasing slope: as human populations increase, the number of moose collisions decreases. This makes sense given the relationship between humans and moose. As urban areas expand and human populations grow, moose are driven out of these regions due to habitat destruction. The loss of forests and natural habitat reduces the number of moose living near populated areas, which in turn leads to fewer collisions.