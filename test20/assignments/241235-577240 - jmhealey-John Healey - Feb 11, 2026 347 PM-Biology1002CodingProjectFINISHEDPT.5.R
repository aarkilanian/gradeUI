##########Moose Population (Part I)#############

#1-6 Kinda forgot to do them out one by one ngl.
moosedata <- read.csv("MoosePopulation.csv")
head(moosedata)
View(moosedata)
moose_clean <- na.omit(moosedata)
nrow(moosedata)
nrow(moose_clean)

moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
names(moose_sel)
head(moose_sel)

year_min <- min(moose_sel$Year)
year_min
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max

moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)

head(moosedata2)

#7a (rewrote so it wasn't janky)
moose_avg <- moosedata2 %>%
  group_by(Year) %>%
  summarise(mean_density = mean(MooseDensity))

plot(moose_avg$Year, moose_avg$mean_density,
     type = "l",
     xlab = "Year",
     ylab = "Average Moose per sq km",
     main = "Average Moose density over time")

#8a
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
unique(moose_west$Ecoregion)
nrow(moose_west)

#8b (also rewrote so it isnt janky)
moose_west_avg <- moose_west %>%
  group_by(Year) %>%
  summarise(mean_density = mean(MooseDensity))

plot(moose_west_avg$Year, moose_west_avg$mean_density,
     type = "l",
     xlab = "Year",
     ylab = "Average Moose per sq km",
     main = "Average Moose density in Western Forests over time")

#9a
moose_2020 <- filter(moosedata2, Year == 2020)
unique(moose_2020$Year)
nrow(moose_2020)

#9b
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high

#9c
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moose_2020_high_byD

#10
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()

moosefinal

############Sampling Study (part II)##########################

#11a
saplings <- read.csv("SaplingStudy.csv")
head(saplings)
View(saplings)

#11b
sap_clean <- na.omit(saplings)
nrow(saplings)
nrow(sap_clean)

#12a
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#12b
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(AverageBrowsing))

avg_browse_reg

#highest avg browsing is 4.57, lowest is 1.

#13a
sap_reg_height <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarise(AverageHeight = mean(Height)) %>%
  print()

#13b
sap_reg_height_low <- sap_reg_height %>%
  filter(AverageHeight < 20) %>%
  print()

##### Eco regions w/ avg height of < 20cm: #####
#Northern_Peninsula_Forests
#Western_Forests  

#14a
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#14b
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(AverageBrowsing))

avg_browse_spe
head(avg_browse_spe, 1)   #highest
tail(avg_browse_spe, 1)   #lowest

#highest = 5 (black_ash)
#lowest = 2.33 (Black spruce)

#15
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

nrow(fir_reg_browse)

#16
barplot(fir_reg_browse$AverageBrowsing,
        names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score (Balsam Fir)",
        main = "Balsam Fir browsing intensity by ecoregion",
        cex.names = 0.6)

#17a
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()

#17b
barplot(spruce_reg_browse$AverageBrowsing,
        names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average browsing score (Black Spruce)",
        main = "Black Spruce browsing intensity by ecoregion",
        las = 2,
        cex.names = 0.6)

#17c
# Across most eco regions, Black Spruce shows lower average browsing scores than Balsam Fir
# suggesting moose tend to browse balsam Fir more heavily than Black Spruce.

#18
sap_reg_tally <- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>%
  print()

#19
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()

#20a
#Sampling is uneven: some eco regions (North_Shore_Forests) and species (Balsam_Fir)
#are over represented, while others (Black_Ash) are underrepresented.

#20b

#Recognizing sampling bias is important bc uneven sampling can distort estimates of browsing
#intensity and lead to misleading conclusions about moose preferences or ecosystem impacts.

################Creating and Joining Datas ets (Part III)#######################

#21a
moose_2020b <- moose_clean %>%
  filter(Year == 2020)

moose_2020b <- moose_2020b %>%
  mutate(MooseDensity = Estimated_Moose_Pop / Area)

head(moose_2020b)
names(moose_2020b)

#21b
moose_sap <- left_join(moose_2020b, sap_clean, 
                       by = "Ecoregion",
                       relationship = "many-to-many")

#21a
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarise(
    MeanBrowsing = mean(BrowsingScore),
    MeanMooseDensity = mean(MooseDensity)
  ) %>%
  print()

#23a
#this is just pasted for me to explore the graph

install.packages("ggplot2")

library(ggplot2)

ggplot(sum_spe_browse, aes(x = AvgDensity, y = AvgBrowsing, color= Species)) +
  geom_point(size = 3) +
  
  theme_minimal() +
  labs(title = "Browsing Intensity Across Moose Density by Species",
       x = "Average Moose Density",
       y = "Average Browsing Score")

#update: nvm i aint got time to try and fix ts bc it aint working

#At low moose densities browsing scores are very different among species meaning selectivity, 
#while at higher densities most species show similarly high browsing, supporting the researchers hypothesis.

#23b
#Moose favor Willow and alder the most, showing the highest average browsing scores, 
#while Black Spruce and black ash are browsed the least.

#23c
#Black ash is largely absent from the figure because very few black ash saplings were sampled, 
#so there are too few data points to meaningfully show its trend across densities.


#24
collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)

human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)

study_sites <- c("North_Shore_Forests",
                 "Northern_Peninsula_Forests",
                 "Long_Range_Barrens",
                 "Central_Forests",
                 "Western_Forests",
                 "StraitOfBelleIsleBarrens",
                 "Avalon_Forests",
                 "EasternHyperOceanicBarrens",
                 "Maritime_Barrens")

moose_coll <- data.frame(collisions2020,
                         human_pop,
                         study_sites)

names(moose_coll) <- c("Collisions2020", "HumanPopulation", "Ecoregion")
head(moose_coll)

#25a
moose_coll2 <- moose_coll #already an eco region name.
names(moose_coll)

#25b
coll_merge <- left_join(moose_2020b, moose_coll2,
                        by = "Ecoregion",
                        relationship = "many-to-many")

names(coll_merge)
head(coll_merge)


#26
plot(coll_merge$MooseDensity, coll_merge$Collisions2020,
     xlab = "Moose Density",
     ylab = "Moose-vehicle collisions (2020)",
     main = "Moose density vs collisions by ecoregion (2020)")

#Collisions do not increase perfectly with moose density. some high density eco  regions have higher collisions,
#but there is also a major outlier suggesting other factors like human population/roads matter.

#27
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = Collisions2020 / HumanPopulation)

#28
plot(coll_merge_per_capita$HumanPopulation,
     coll_merge_per_capita$coll_per_capita,
     xlab = "Human population",
     ylab = "Moose collisions per person",
     main = "Collisions per capita vs human population (2020)")

#29
#Collisions per capita tend to be higher in regions with smaller human populations and lower in highly populated regions.
#This makes sense because in rural areas there are fewer people but similar numbers of moose and roads, increasing per-person risk.

#DONEEEEEE FINALLLY

