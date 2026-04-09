# R Assignment\
# Chelsea Skanes- 202412832\

### R Assignment Part 1 - 1002 ###
# set Working Directory\
setwd("RAssignment")

# Question 1
install.packages("tidyverse")
library(dplyr)

# Question 2 read.csv\
moosedata <- read.csv("MoosePopulation.csv")

# Question 3 removing data\
moose_clean <- na.omit(moosedata)

# Question 4\
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)

# Question 5 a\
select(moose_sel, Year)
year_min<- min(moose_sel$Year)
#Question 5 b\
moose_max <- max(moose_sel$Estimated_Moose_Pop)

#Question 6 moose density\
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7 a plotting data\
plot(moosedata2$Year, moosedata2$MooseDensity,xlab = "year", ylab = "Moose per sq km", main = "Moose density in Newfoundland Ecoregions over time")

# Question 8 western forests data\
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")

# Question 8 b\
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density in Western forest regions over time")

# Question 9 a\
moose_2020<- filter(moosedata2, Year == "2020)

# Question 9 b highest moose density\
moose_2020_high <- filter(moose_2020, MooseDensity >2.0000000)

# Question 9 c arranging dataset\
moose_2020_high_byd <- arrange(moose_2020_high,desc(MooseDensity))

# Question 10 connecting code lines\
moosefinal <- moosedata2 %>%
filter(Year == 2020) %>%
filter(MooseDensity > 2.0) %>%
arrange(desc(MooseDensity)) %>%
print()

# Question 11 a rename dataset\
saplings <- read.csv("SaplingStudy")

# Question 11b removing missing data\
sap_clean <- na.omit(saplings)

# Question 12a moose browsing pressure\
sap_reg_browse <- sap_clean%>%
group_by(Ecoregion)%>%
summarize(mean(BrowsingScore))%>%
print()

# Question 12b high/low moose browsing\
avg_browse_reg <- sap_clean %>%
group_by(Ecoregion) %>%
summarize (AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
arrange(desc(AverageBrowsing)) %>%
print()
# Highest Browsing 5.0 (North_Shore_Forests and Northern_Peninsula_Forests)\
# Lowest Browsing: 0.0 (Eastern_Hyper_Oceanic_Barrens and Maritime_Barrens)\

#Question 13 a\
sap_reg_height <- sap_clean %>%
group_by(Ecoregion) %>%
summarize(mean(Height)) %>%
print()

# Question 13b\
finding avg heights less than 20cm\
sap_reg_height_low <- # northen_peninsula_forests and western_forests have avg heights <20cm print()

# Question 14a sapling species browsing score\
sap_spe_browse<- sap_clean %>%
group_by(Species) %>%
summarize(mean(BrowsingScore)) %>%
print()

# Question 14b\
avg_browse_spe <- sap_clean %>%
group_by(Species) %>%
summarize (AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%
arrange(desc(AverageBrowsing)) %>%
print()
# black_ash has the highest browsing score and black_spruce as the lowest

#Question 15 balsam fir browsing intensity\
fir_reg_browse <- sap_clean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion)%>%
summarize(MeanBrowsing = mean(BrowsingScore)) %>%
print()

# Question 16\
barplot(fir_reg_browse$MeanBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Intensity", main = "Balsam Fir Browsing Intensity Variety in Diffrent ecoregions", col = "forestgreen",cex.names = 0.5)

# Question 17a black spruce browsing intensity\
spruce_reg_browse <- sap_clean %>%
filter(Species == "Black_Spruce") %>%
group_by(Ecoregion) %>%
summarize(MeanBrowsing = mean(BrowsingScore)) %>%
print()

# Question 17b dataset\
barplot(spruce_reg_browse$MeanBrowsing, names.arg = spruce_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Average Browsing Intensity", main = "Black Spruce Browsing Intensity Variety in Different Ecoregions", col = "black",cex.names = 0.5)

# Question 17c answer: The black spruce showeds higher browsing intensity than the balsam fir in the Eastern abd Maritime ecoregions, showing the increase in moose in the other regions.

# Question 18\
sap_reg_tally<- sap_clean %>%
group_by(Ecoregion) %>%
tally() %>%
print()

# Question 19 tree sapling within species\
sap_spe_tally<- sap_clean %>%
group_by(Species) %>%
tally() %>%
print()

# Question 20a ans: 

