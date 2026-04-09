######### moose part 1
read.csv("~/Downloads/MoosePopulation.csv")
read.csv("~/Downloads/SaplingStudy.csv")
na.omit(MoosePopulation)
moose_clean <- na.omit(MoosePopulation)
select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
min(moose_sel$Year)
#min year was 1904
max(moose_sel$Estimated_Moose_Pop)
#max estimated moose population was 41250
year_min = min(moose_sel$Year)
moose_max = max(moose_sel$Estimated_Moose_Pop)
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity, 
     xlab = "year", 
     ylab = "Moose per sq km", 
     main = "Moose density in Newfoundland ecoregions over time")
moose_west <- filter(moosedata2, Ecoregion == "Western_Forests")
plot(type = "l", moose_west$Year, moose_west$Year,
     xlab = "Year",
     ylab = "Moose per sq km",
     main = "Moose Density in Newfoundland Western Forests Over Time")
moose_2020 <- filter(moosedata2, Year == "2020")
moose_2020_high <- filter(moose_2020, MooseDensity >= "2.0")
arrange(moose_2020_high, desc(MooseDensity))
#Northern peninusla 2.66, North Shore 2.59, Western 2.50, Central 2.01
moosefinal <- moosedata2 %>%
  filter(Year == 2020) %>%
  filter(MooseDensity > 2.0) %>%
  arrange(desc(MooseDensity)) %>%
  print()
#Northern Peninsula was highest, Central forest was lowest
######################sapling part 2
saplings <- SaplingStudy  
sap_clean <- na.omit(saplings)
sap_reg_browse <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(averagebrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_reg <- sap_reg_browse %>%
  arrange(desc(averagebrowsing)) %>%
  print()
#Northern_peninsula_forests had highest, StraitOfbelleIsleBarrens had lowest
sap_reg_hight <- sap_clean %>%
  group_by(Ecoregion) %>%
  summarize(averageheight = mean(Height)) %>%
  print()
sap_reg_height_low <- filter(sap_reg_hight, averageheight<20) %>%
  print()
#Northern peninsula forests, and western forests had averages under 20cm
sap_spe_browse <- sap_clean %>%
  group_by(Species) %>%
  summarize(averagebrowsing = mean(BrowsingScore)) %>%
  print()
avg_browse_spe <- sap_spe_browse %>%
  arrange(desc(averagebrowsing)) %>%
  print()
#Black ash had highest, Black Spruce had lowest
fir_reg_browse <- sap_clean %>%
  filter(Species == "Balsam_Fir") %>%
  group_by(Ecoregion) %>%
  summarize(averagebrowsing = mean(BrowsingScore))
barplot(fir_reg_browse$averagebrowsing, names.arg = fir_reg_browse$Ecoregion,
        xlab = "Ecoregions",
        ylab = "Average Amount of Browsing",
        main = "Average Browsing Amount in Ecoregions Across NL",
        col = "pink",
        cex.names = 0.6)
#
spruce_reg_browse <- sap_clean %>%
  filter(Species == "Black_Spruce") %>%
  group_by(Ecoregion) %>%
  summarize(averagebrowsing = mean(BrowsingScore))
barplot(spruce_reg_browse$averagebrowsing, names.arg = spruce_reg_browse$Ecoregion,
        xlab = "Ecoregions",
        ylab = "Average Amount of Browsing",
        main = "Average Browsing Amount in Ecoregions Across NL",
        col = "pink",
        cex.names = 0.6)
#Both are very similar in the northern peninsula, north shore, western, 
#central,  and log range areas. However the balsam fir is more browsed in the avalon, eastern,
#and maritime areas.
sap_reg_tally<- sap_clean %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()
sap_spe_tally <- sap_clean %>%
  group_by(Species) %>%
  tally() %>%
  print()
#Some species like the Balsam fir have been overrepresnetd in this study,
#meanwhile the black ash has been severley underrepresented. So this dataset would
#not be considered evenly distributed.

#It is important to recognize this kind of bias since it effects our understnading
#of what were studying, in this case, we don't know the true data for the 
#black ash, as we only have one subject.
#####part 3
moose_2020b <- filter(moosedata2, Year == "2020")
mutate(moose_2020b, MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationship = "many-to-many")
sum_spe_browse <- moose_sap %>%
  group_by(Species, Ecoregion) %>%
  summarize(averagebrowsing = mean(BrowsingScore), averagedensity = mean(MooseDensity)) %>%
  print()
#Their hypothesis is supported, the moose show higher browsing at 
#lower densities and vice versa.
#Moose seem to prefer Willow the most with Alder close behind
#The only sapling not shown is the black ash as it only had one sample.

collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
human_pop <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens",
                "Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens",
                "Avalon_Forests","StraitOfBelleIsleBarrens")

moose_coll <- data.frame(collisions2020, human_pop, study_sites)
moose_coll2 <- moose_coll %>%
  rename(Ecoregion = study_sites)
coll_merge <- left_join(moose_coll2, moose_2020, by = "Ecoregion")
plot(coll_merge$MooseDensity, coll_merge$collisions2020,
     xlab = "Moose Density",
     ylab = "Number of Collisions")
#The trend is that the denser the moose population get, the more collisions
#there are, however there are outliers in this trend at 1.0 Moose Density
coll_merge_per_capita <- coll_merge %>%
  mutate(coll_per_capita = collisions2020 / human_pop)
plot(coll_merge_per_capita$collisions2020, coll_merge_per_capita$human_pop,
     xlab = "Number of collisions",
     ylab= "human population")
#As the human population goes up, the number of collisions goes up.
#This trend makes sense considering the more people/moose that are around
#The more likely it is they will collide.