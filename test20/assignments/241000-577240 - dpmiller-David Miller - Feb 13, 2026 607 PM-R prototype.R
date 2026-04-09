# David P Miller Rassignment.

#1.)library(readr)

#2.)MoosePopulation <- read_csv("C:/Users/David/Desktop/BIOL1002_Rassignment/MoosePopulation.csv")

#3.)moose_clean <- na.omit(MoosePopulation)

#5.) The earliest year of observation in the dataset is 1904, the highest estimated population of the moose was 41 250) min <- min(MoosePopulation$Year, na.rm = TRUE) year_min,moose_max <- max(MoosePopulation$Estimated_Moose_Pop, na.rm = TRUE),moose_max,moose_sel <- MoosePopulation %>% filter(Year == 2020),moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area) library(dplyr)

#6.) moosedata2 <- MoosePopulation %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)

#7.)plot(moosedata2$Year, moosedata2$MooseDensity,   type = "l",   xlab = "Year",   ylab = "Moose per sq km",   main = "Moose density in Newfoundland ecoregions over time")

#8.) moose_west <- filter(moosedata2, Ecoregion == "Western_Forests") plot(moose_west$Year, moose_west$MooseDensity, type = "l", xlab = "Year", ylab = "Moose per sq km", main = "Moose density over time in the Western Forests ecoregion")

#9.)moose_2020 <- filter(moosedata2, Year == 2020) moose_2020_high <- filter(moose_2020, MooseDensity > 2.0) moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))

#10.) moosefinal <- moosedata2 %>%  filter(Year == 2020) %>%  filter(MooseDensity > 2.0) %>%  arrange(desc(MooseDensity)) %>% print()

#11.)SaplingStudy <- read_csv("SaplingStudy.csv"), sap_clean <- na.omit(sapling)

#12.) Moose browsing pressure varies noticeably across ecoregions. Some regions show much higher average browsing scores, indicating heavier moose impact on saplings, while others show lower browsing scores, suggesting reduced browsing pressure. The sorted dataset () clearly identifies which ecoregions experience the most and least browsing.The highest browsing region was Northern Peninsula Forest at 4.57 and the lowest was Strait of Belle Is Barrens at 1, sap_reg_browse <- sap_clean %>%  group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%  print()

#13.)sap_reg_height <- sap_clean %>% group_by(Ecoregion) %>% summarize(AverageHeight = mean(Height, na.rm = TRUE)) %>% print(),Ecoregions with average height < 20 cm: Western_Forests, Central_Lowlands, Average sapling height varies across ecoregions. Some regions show much shorter saplings (under 20 cm), indicating heavy moose browsing, while others have taller saplings, suggesting lower browsing pressure. The filtered dataset () identifies which ecoregions are most heavily impacted.

#14.)sap_spe_browse <- sap_clean %>% group_by(Species) %>% summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>% print(),avg_browse_spe <- sap_spe_browse %>% arrange(desc(AverageBrowsing)), # Highest browsing: Willow Lowest browsing: Black_Spruce,Some species experience much heavier browsing pressure than others. Species with the highest average browsing scores are likely preferred by moose, while species with the lowest scores are browsed less often or avoided.

#15.)fir_reg_browse <- sap_clean %>%,filter(Species == "Balsam_Fir") %>%, group_by(Ecoregion) %>%,summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>%,print()

#16.)barplot(fir_reg_browse$AverageBrowsing, names.arg = fir_reg_browse$Ecoregion, xlab = "Ecoregion", ylab = "Average Browsing Score", main = "Average Balsam Fir Browsing Intensity by Ecoregion", col = "forestgreen", cex.names = 0.6)

#17.)spruce_reg_browse <- sap_clean %>% filter(Species == "Black_Spruce") %>% group_by(Ecoregion) %>% summarize(AverageBrowsing = mean(BrowsingScore, na.rm = TRUE)) %>% print(),barplot(spruce_reg_browse$AverageBrowsing,names.arg = spruce_reg_browse$Ecoregion,xlab = "Ecoregion",ylab = "Average Browsing Score", main = "Average Black Spruce Browsing Intensity by Ecoregion",col = "darkolivegreen3",cex.names = 0.6), Black Spruce generally shows lower browsing intensity than Balsam Fir across most ecoregions. This suggests moose prefer Balsam Fir over Black Spruce when browsing saplings.

#18.)sap_reg_tally <- sap_clean %>% group_by(Ecoregion) %>% tally() %>% print(). No the same amount of trees weren't counted for each ecoregion.

#19.)sap_spe_tally <- sap_clean %>% group_by(Species) %>% tally() %>% print(), also no, the same amount of saplings were not counted for each

#20.a)The SaplingStudy data set is not evenly distributed.  Some ecoregions have many more saplings sampled than others, and some species are over represented while others have very few samples. The data set is uneven: Eco regions like Western_Forests have many saplings, while others have fewer. Species such as Balsam_Fir are heavily represented, while species like White_Birch have fewer samples.

#20.b) Recognizing sampling bias is important because uneven sampling can distort ecological patterns and lead to incorrect conclusions. If some species or ecoregions are over represented, it may falsely appear that moose prefer or avoid certain areas or trees.

#21.a) moose_2020b <- moose_clean %>% filter(Year == 2020) %>% mutate(MooseDensity = Estimated_Moose_Pop / Area)

#21.b)moose_sap <- left_join(moose_2020b, sap_clean,  by = "Ecoregion", relationship = "many-to-many")

#22.)sum_spe_browse <- moose_sap %>% group_by(Species, Ecoregion) %>% summarize(MeanBrowsing = mean(BrowsingScore, na.rm = TRUE),MeanMooseDensity = mean(MooseDensity, na.rm = TRUE)) %>% print()

#23.a)Yes. The figure shows that at low moose densities, browsing intensity is concentrated on a few preferred species, while at higher densities browsing becomes more evenly spread across species, which is consistent with a shift from specialist to more generalist feeding.

#23.b)Moose appear to favour Alder the most, as it has the highest average browsing scores across densities. Black Spruce is browsed the least, showing consistently low browsing intensity.

#23.c)One sapling species is not shown because it had little to no browsing recorded, so it was likely removed during data summarization or plotting (for example, species with zero or missing average browsing values).

#24.)collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6) human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300) study_sites <- c("North_Shore_Forests","Northern_Peninsula_Forests","Long_Range_Barrens","Central_Forests","Western_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens","Avalon_Forests", "StraitOfBelleIsleBarrens") moose_coll <- data.frame(collisions2020, human_pop, study_sites)

#25.)moose_coll2 <- moose_coll %>% rename_with(~ "Ecoregion", study_sites) coll_merge <- left_join(moose_2020b, moose_coll2,by = "Ecoregion", relationship = "many-to-many")

#26.)plot(coll_merge$MooseDensity,coll_merge$collisions2020,xlab = "Moose Density (moose per km²)",ylab = "Moose-Vehicle Collisions (2020)",main = "Moose Density vs Moose-Vehicle Collisions",pch = 19, col = "darkred") There is a general trend where higher moose density is associated with more moose-vehicle collisions. One ecoregion appears to be an outlier with very high collision numbers relative to its density (likely Avalon Forests).

#27.)coll_merge_per_capita <- coll_merge %>% mutate(coll_per_capita = collisions2020 / human_pop) The ecoregions with the highest moose collisions per person are: Northern_Peninsula_Forests (highest), followed by Long_Range_Barrens,North_Shore_Forests, and EasternHyperOceanicBarrens.

#28.)plot(coll_merge_per_capita$human_pop, coll_merge_per_capita$coll_per_capita,xlab = "Human Population",ylab = "Collisions per Capita",main = "Moose Collisions per Capita vs Human Population",pch = 19,col = "darkblue")

#29.) The plot shows a strong negative relationship: regions with small human populations have much higher collisions per capita.This fits what we know about Newfoundland—remote, low‑population areas tend to have more moose, more forested roads, and therefore a higher collision risk per person.