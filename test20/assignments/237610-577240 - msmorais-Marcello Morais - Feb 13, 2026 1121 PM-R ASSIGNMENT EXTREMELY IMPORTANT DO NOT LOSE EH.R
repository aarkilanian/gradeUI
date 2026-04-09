##PART 1 BIOL 1002 ASSIGNMENT

(library)
library()
installed.packages(tcltk)
install.packages(graphic)
library(graphic)
install.packages(graphic)
install.packages(toolpack)
4+5+6+6
library(tools, lib.loc = "C:/Program Files/R/R-4.5.2/library")
library(translations, lib.loc = "C:/Program Files/R/R-4.5.2/library")
library(translations, lib.loc = "C:/Program Files/R/R-4.5.2/library")
library(tcltk, lib.loc = "C:/Program Files/R/R-4.5.2/library")
library(readxl)
FalikDataset <- read_excel("C:/Users/GASOSO/Downloads/BIOL1001RASSIGNMENT/FalikDataset.csv")
install.packages("dplyr")

## Question 1

library(dplyr)
library(readxl)
MoosePopulation <- read_excel("\"C:\\Users\\GASOSO\\Desktop\\MEMORIAL UNIVERSITY CLASS NOTES, ASSIGNEMENTS, COURSE MATERIAL, ETC\\BIOL 1002 Principles of Biology\\BIOLOGY1002RASSIGNMENT\\MoosePopulation.csv\"")
library(readxl)
MoosePopulation <- read_excel("\"C:\\Users\\GASOSO\\Desktop\\BIOLOGY1002RASSIGNMENT\\MoosePopulation.csv\"")
library(readxl)
MoosePopulation <- read_excel("C:/Users/GASOSO/Desktop/MoosePopulation.csv")
getwd()
setwd"BIOLOGY1002RASSIGNMENT"
setwd"dir"BIOLOGY1002RASSIGNMENT
setwd"C:\Users\GASOSO\Desktop"
"C:\Users\GASOSO\Desktop\BIOLOGY1002RASSIGNMENT"
library(readxl)
BIOLOGY1002RASSIGNMENT_ <- read_excel("\"C://Users//GASOSO//Desktop//BIOLOGY1002RASSIGNMENT/"")
setwd("C:\Users\GASOSO\Desktop\BIOLOGY1002RASSIGNMENT")
getwd()
setwd("C:\Users\GASOSO\Desktop\BIOLOGY1002RASSIGNMENT")
setwd("C:/Users/GASOSO/Desktop/BIOLOGY1002RASSIGNMENT")

## QUESTION 2

Moose_Population <- read.csv("C:/Users/GASOSO/Desktop/BIOLOGY1002RASSIGNMENT/MoosePopulation.csv")

## QUEsTION 3

View(Moose_Population)
moose_clean<-na.omit(Moose_Population)

## QUESTION 4

moose_sel<-select(moose_clean,Ecoregion,Year,Area,Estimated_Moose_Pop)

## QUESTION5

year_min<-min("year")
year_min<-min(moose_sel$Year)
moose_max<-max(moose_sel$Estimated_Moose_Pop)

## QUESTION 6

moosedata2<-mutate(moose_sel,MooseDensity=Estimated_Moose_Pop/Area)

## QUESTION 7

plot(moosedata2$Year, moosedata2$MooseDensity, xlab = "year", ylab = "Moose per sq km",main = "Moose density in Newfoundland ecoregions over time")

## QUESTION 8

moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "year", ylab = "Moose per sq km", main = "Moose density in NL Western forests over time")
library(ggplot2)

## QUESTION 9

moose_2020<-filter(moosedata2,Year==2020)
View(moose_2020)
moose_2020_high<-filter(moose_2020,MooseDensity>2.0)
moose_2020_high_byD<-arrange(moose_2020_high,desc(MooseDensity))

## QUESTION 10

moosefinal <- moosedata2 %>%filter(Year == 2020) moosedata2%>%filter(MooseDensity > 2.0) moosedata2%>%arrange(desc(MooseDensity)) moosedata2%>%print(moosedata2)
moosefinal <- moosedata2 %>%filter(Year == 2020) %>%filter(MooseDensity > 2.0) %>%arrange(desc(MooseDensity)) %>%print()

## PART 2

## QUESTION 11

saplings <- read.csv("C:/Users/GASOSO/Desktop/BIOLOGY1002RASSIGNMENT/saplingstudy.csv")

sap_clean<-na.omit(saplings)

#QUESTION 12

#a

sap_reg_browse<-sap_clean%>%group_by(Ecoregion) %>%summarize(mean(BrowsingScore))%>%print()
 sap_clean%>%summarize(mean(BrowsingScore)) sap_clean%>%(BrowsingScore) %>%print()

#b

sap_reg_browse<-sap_reg_browse%>%rename(Average_Browsing=mean(BrowsingScore))

setnames(sap_reg_browse,"mean(BrowsingScore)","Average_Browsing"))

avg_browse_reg<-arrange(desc(BrowsingScore)

# Region North Shore Forests, Northern Peninsula Forests, Central Forests, and western forests had the highest, Easter  Hyperoceaninc Barriers and Maritime Barriers had the Lowests

##QUESTION 13

#a

sap_reg_height<- sap_clean%>%group_by(Ecoregion) %>%summarize(Height=mean(Height)) %>% print()

#b 

sap_reg_height_low <- filter(sap_reg_height, Height<20) %>%print()
##Northern Peninsula Forests and Western Forests are the Only ecoregions in which the average height is less than 20 cm eh b'y

## Question 14

#a

sap_spe_browse<-sap_clean%>%group_by(Species) %>%summarize(BrowsingScore=mean(BrowsingScore)) %>%print()

#b

avg_browse_spe<-arrange(sap_spe_browse,desc(BrowsingScore))
print(avg_browse_spe)
##Black ash has the highest browsing score, and Black Spruce has the Lowest Browsing Score eh b'y

## Question 15

fir_reg_browse<- filter(sap_clean,Species == "Balsam_Fir") %>%group_by(Ecoregion) %>%summarize(BrowsingScore=mean(BrowsingScore)) %>%print() 

## Question 16

barplot(fir_reg_browse$BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Browsing Score", main = "Balsam Fir Browsing Intensity accross Ecoregions", col = "red", cex.names = 0.34) 

## Question 17

#a

BlackSpruce_reg_browse<- filter(sap_clean,Species == "Black_Spruce") %>%group_by(Ecoregion) %>%summarize(BrowsingScore=mean(BrowsingScore)) %>%print() 

#b

barplot(BlackSpruce_reg_browse$BrowsingScore, names.arg = BlackSpruce_reg_browse$Ecoregion, xlab = "Ecoregions", ylab = "Browsing Score", main = "Black Spruce Browsing Intensity accross Ecoregions", col = "red", cex.names = 0.34) 

#c

## The Black Spruce's browsing rate is more variable than the browsing rate exhibited by the Balsam Fir, with Black Spruce Presenting a null browsing rate in two eco regions, but maximun browsing rate in more regions than the balsma fir, which does not exibit null browsing rate in any ecoregion. Balsam Fir Browsing rate is thus, less variable than black spruces browsing rate.

## Question 18

sap_reg_tally<- sap_clean %>%group_by(Ecoregion) %>%tally() %>% print()

## Different Numbers of Saplings were counted for each region eh yes b'y

## Question 19

sap_spe_tally<-sap_clean %>%group_by(Species) %>%tally() %>% print()

## Different Numbers of Saplings were counted for each species

## Question 20

#a

## No, I do not think that the sanpling study dataset is evenly distributed, becaseu some species, like black ash, are extremely underrepresented (1 individual) while other, like balsam fir, have more than the others, being overrepresented (11 for balsam fir). The ecoregions are also not evenly distributed, with Strait Of Belle Isle Barrens with only one observation being thus, underrrepresented, while north shore forests has 8, being overlyrepresented. Even excluding these extreme comparisons, almost every species and ecoregion has different numbers of observations from each other.

#b It is important to recognize biases in ecological datasets because both overrepresented and under represented species are differently affected by biological, physical and chemical variables. For example, if a sampling method favour species of high corporal volume against species of small corporal volume, one may end up with a higher number of individuals of large sized species than individuals of small sized species, even though the small sized species may very well be more common in a certain area than big sized species, but the researcher, having caught more individuals big individuals, from large bodied species than small individuals from small bodied species, may think that the opposite is true, that large bodied species are the norm, not the exception, and this conclusion, result of a sampling bias, would not correspond with reality.  

###PART III

## Question 21

#a

moose_2020b<-filter(moose_clean,Year==2020) %>%mutate(MooseDensity=Estimated_Moose_Pop/Area)

#b

moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")

## Question 22

sum_spe_browsing<- moose_sap %>%group_by(Species,Ecoregion) %>%summarize(BrowsingScoremean=mean(BrowsingScore),MooseDensitymean=mean(MooseDensity)) %>%print()
sum_spe_browsing

## Question 23

ggplot(sum_spe_browsing, aes(x = MooseDensitymean, y = BrowsingScoremean, color= Species)) + geom_point(size = 3) + theme_minimal() + labs(title = "Browsing Intensity Across Moose Density by Species",x = "MooseDensitymean", y = "BrowsingScoremean")

#a 

## yes, moose show strong preferences at low densities, with willow and alder being the preffered prey, and slowly turn to generalists at higher densities, with densities above 2 exhibiting a high levels (>2.5) of Browsing score accross all saplings.

#b

## moose favour Willow saplings the most, possessing the highest Browsing Score Mean when compared to other crops at the same densities. The only instance Willow is not prefferred, is when it is not available, which happened in the Moose Density Mean of 1 in the graph, where no Willow saplings were present at this density, but it is likely that willow saplings would still be prefferred at this density, if they were present in the observations at this specific moose density 

#c

## Black ash is not shown in the figure. There is only one instance of black ash in the data frame, and it had the same Browsing Score Mean value at the same Moose density Mean as Willow sapling, so Willow sapling was shown at the graph instead of black ash, and, having only this "opportunity" to appear in the graph, it lost its position to willow, and didnt appear again, since this would be the only instance black ash would appear in the graph, if not for willow

## Question 24

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)

## Question 25

#a

moose_coll2<- moose_coll %>%rename(Ecoregion = study_sites)

#b

coll_merge_alpha <- left_join(moose_2020, moose_coll2, by = 'Ecoregion', relationship = "many-to-many")

## Question 26

#a

plot(coll_merge_alpha$collisions2020, coll_merge_alpha$MooseDensity, xlab = "Collisions IN 2020", ylab = "Moose per sq km",main = "Moose Collisions in Newfoundland ecoregions according to moose per sq km")

#b

Up to around 3 moose per sq kilometer, it has a general, upward trend, with a positive correlation between moose per sq km and collisions in the year 2020. However, In one Ecoregion, there was a high number of collisions, even though the moose density per sq km was much lower than other places, which makes it an outlier

## Question 27

coll_merge_per_capita<-mutate(coll_merge_alpha,coll_per_capita=collisions2020/human_pop)

## Question 28

plot(coll_merge_per_capita$coll_per_capita, coll_merge_per_capita$human_pop, xlab = "collisions per capita", ylab = "human population",main = "Moose Collisions in Newfoundland ecoregions according to ")

## Question 29

According to the graph, the higher the human population, the less collisions per capita occur in Newfoudnland. It makes sense, as areas with a high human population will be more devoid of forests and preserved areas, inhibiting moose populations, and areas with less population will not inhibit moose populations of moving throughout the space, and thus, more collisions will happen. Since those regions in which collisions happen already have a lower human population, the collisions per capita will increase even more, since each each collision will have a higher impact on the collisionns per capita ratio.
It makes sense according to what I know about moose and human populations in newfoundland, as there are many moose, but not that many humans.
