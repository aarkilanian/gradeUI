install.packages("dplyr")
min("Year")
[1] "Year"
> min(moose_sel$Year)
[1] 1904
year_min 
moose_west <- moose_west
> moose_west <- filter(moosedata2, Ecoregion == "Western_Forests", type = "l",
 
plot(moose_west$Year, moose_west$MooseDensity, 
                            xlab = "year", 
                            ylab = "Moose per sq km", 
                            main = "Moose density in Newfoundland ecoregions over time", type = "l")
moose_2020 <- filter(moosedata2, Year == 2020)

moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
saplings <- read.csv("X5936994SaplingStudy")
na.omit(saplings)
sap_clean <- na.omit(saplings)
SaplingStudy <- read.csv("X5936994SaplingStudy")
saplings <- (SaplingStudy)
sapclean <- na.omit(saplings)
sap_reg_browse <- sapclean%>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing= mean(BrowsingScore))%>%
print()

avg_browse_reg <- sapclean %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing= mean(BrowsingScore)) %>%  
arrange(desc(AverageBrowsing)) %>%
print()
#Northern_Peninsula_Forests have the most moose browsing
#StraitOfBelleIsleBarrens have the least moose browsing

sap_reg_height <- sapclean %>%
group_by(Ecoregion) %>%
summarize(AverageBrowsing= mean(BrowsingScore)) %>%
print()

sap_reg_height_low <- sapclean %>%
group_by(Ecoregion) %>%
summarize(mean_height = mean(Height)) %>%  
filter(mean_height < 20) %>%  
print()  
# Ecoregions with average tree heights less than 20cm are considered severly browsed
# Northern_Peninsula_Forests and Western_Forests

avg_browse_reg <- sapclean %>%
 group_by(Species) %>%  
summarize(AverageBrowsing= mean(BrowsingScore)) %>%  
arrange(desc(AverageBrowsing)) %>%  
print()

# Black_Ash has the highest browsing score of 5
#Black_Spruce has the lowesr browsing score of 2.33

fir_reg_browse <- sapclean %>%
filter(Species == "Balsam_Fir") %>%
group_by(Ecoregion) %>%  
summarize(mean_BrowsingScore =mean(BrowsingScore)) %>%  
print()  

barplot(fir_reg_browse$mean_BrowsingScore, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Mean_Browsing", main = "Mean Browsing of Balsam Fir in Each Ecoregion", col = "magenta", cex.names =0.6)
spruce_reg_browse <-sapclean %>%
 filter(Species == "Black_Spruce") %>%  
group_by(Ecoregion) %>%  
summarize(mean_browsing = mean(BrowsingScore)) %>%  
print()  
barplot(spruce_reg_browse$mean_browsing, names.arg = spruce_reg_browse$Ecoregion, xlab ="Ecoregion", ylab= "Mean_Browsing", main = "Mean Browsing of Black Spruce in Each Ecoregion", col = "yellow", cex.names =0.6)
 #There are no black spruce in the Northern_Peninsula_Forests, while there are balsam fir in the Northern_Peninsula_Forests with a browsing score of 1. There are more balsam fir in the Avalon_Forests than there are black spruce, with a 2 browsing score for balsam fir and a 0.5 score for black spruce.
sap_reg_tally <- sapclean%>%
 group_by(Ecoregion)%>%
tally()%>%
print()  
sap_spe_tally<- sapclean%>%
group_by(Species)%>%
tally() %>% 
print()
#No, the same number of tree saplings were not counted for each species
#No, I think the SaplingStudy dataset is not evenly distrubted across all Ecoregions. 
#Strait_of_Bell_Isle is an underrepresented ecoregion at 1 tree, North_Shore_Forests is an overreprented at 8 trees
#Recognizing bias in ecological datasets is important because bias can skew results unfairly and lead to inaccurately represented data. Ultimately leading to consumers of the research being mislead and informed about incorrect information

moose_2020b <- moose_clean%>%
  filter(Year == "2020")%>%
  mutate(MooseDensity=Estimated_Moose_Pop / Area)

moose_sap <- left_join(moose_2020b, sapclean, by = 'Ecoregion',relationship = "many-to-many")

sum_spe_browse <- moose_sap%>%
group_by(Species, Ecoregion)%>%
summarize(mean_BrowsingScore = mean(BrowsingScore), mean_MooseDensity =mean(MooseDensity))%>%  
print()  
library(ggplot2)

ggplot(sum_spe_browse, aes(x = mean_BrowsingScore, y = mean_MooseDensity, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +c
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")
#Yes, there is evidence supporting the researchers hypothesis. At Average Moose Density 0, only Willow is browsed highly with a  score of 4 meaning there is a strong preference. But at higher densities, multiple species have a higher browsing score. This suggests that there is a shift to more generalist browsing
#Willow is the most favoured sapling species. Black_Spruce is the least browsed
  #Black_Ash is not shown on the figure since it likely had no browsing activity recorded because it might have not been found in the study plots
  collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
  human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
  study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")
  coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
  moose_coll <- data.frame(collisions2020, human_pop, study_sites)
 
   
  
  plot(coll_merge$MooseDensity, coll_merge$collisions2020,
       xlab = "Moose Density",
       ylab = "Collisions 2020",
       main = "Moose Density vs. Collisions")
  
  #A positive correlation is shown, higher moose density is associated with an increased number of collisions. There is an outlier where there is a moose density of 1.0, seeing above 100 collisions in 2020 
   
  plot(human_pop, coll_merge_per_capita)
  plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita, xlab = "Human Population", ylab = "Collisions Per Capita", main = "Moose Collisions vs. Human Pop") 
  
  #The trend shows that ecoregions with smaller human populations have higher collisions per capita. This makes sense, especially for Newfoundland. We have so many rural communities, which results in more moose resting spots and less people, meaning you are more than likely going to see a moose there rather than Downtown St John's.
  