# Biology 1002 R assignment
# Kaleigh Coombs
# 11-02-2026
moose data <- read.csv("MoosePopulation.csv")
moose_clean <- na.omit(moosedata)
library(dplyr)
moose_sel <- select(moose_clean, Ecoregion, Year, Area, Estimated_Moose_Pop)
View(moose_sel)
str(moose_sel)
moose_sel$Year <- as.numeric(moose_sel$Year)
year_min <- min(moose_sel$Year)
year_min
# The oldest observation in the dataset is 1904
moose_max <- max(moose_sel$Estimated_Moose_Pop)
moose_max
# The highest estimated moose population recorded was 41250
moosedata2 <- mutate(moose_sel, MooseDensity = Estimated_Moose_Pop / Area)
plot(moosedata2$Year, moosedata2$MooseDensity,
	+ 	xlab = "year"
	+ 	ylab = "Moose per sq km",
	+	main = "Moose Density in Newfoundland ecoregion over time")
moose_west <- filter(moosedata2, Ecoregion == "Western Forests")
moose_west$Moose_Density <- moose-west$Estimated_Moose_Pop / moosewest$Area
plot(
	+	moose_west$Year,
	+	moose_west$Moose_Density,
	+	type = "l",
	+	xlab = "Year",
	+	ylab = "Moose Density (moose per unit area)",
	+	main = "Moose Density Over Time in the Western Forests Ecoregion")
moose_2020 <- filter(moose_sel, Year == 2020)
moose_2020$MooseDensity <- moose_2020$Estimated_Moose_Pop / moose_2020$Area
moose_2020_high <- filter(moose_2020, MooseDensity > 2.0)
moose_2020_high_byD <- arrange(moose_2020_high, desc(MooseDensity))
moosefinal <- moosedata2 %>%
	+	filter(Year == 2020) %>%
	+	filter(MooseDensity > 2.0) %>%
	+	arrange(desc(MooseDensity)) %>%
	+	print()
saplings <- read.csv("SaplingStudy.csv")
sap_clean <- na.omit(saplings)
library(dpyler)
>
	> sap_reg_browse <- sap_clean %>%
	+	group_by(Ecoregion) %>%
	+	summarize(AverageBrowsing = mean(BrowsingScore))
>
	> print(sap_reg_browse)
# Nothern_Peninsula_Forests has the highest moose browsingscore (4.57) while StraitOfBelleIsleBarrens has the lowest browsingscore (1.00).
> avg_brose_reg <- arrange(sap_reg_browse, desc(AverageBrowsing))
>
	> sap_reg_height <- sap_clean %>%
	+	group_by(Ecoregion) %>%
	+	summarize(AverageBrowsing = mean(Height))
>
	> print(sap_reg_height)
# Filter ecoregions with the average tree height being less than 20cm (severely browsed)
> sap_reg_height_low <- sap_reg_height %>%
	+	filter(AverageHeight < 20)
>
	> print(sap_reg_height_low)
# Northern_Peninsula_Forests and Western_Forests both have severly browsed saplings (average heights < 20 cm)
sap_spe_browse <- sap_clean %>%
	+	group_by(Species) %>%
	+	summarize(AverageBrowsing = mean(BrowsingScore))
>
	> print(sap_spe_browse)
avg_browse_spe <- arrange(sap_spe_browse, desc(AverageBrowsing))
# Black_Ash has the highest browsing score (5.0), meanwhile Black_Spruce has the lowest browsing score (2.33)
> fir_reg_browse <- sap_clean %>%
	+	filter(Species == "Bolsom_Fir") %>$
	+	group_by(Ecoregion) %>%
	+	summarize(AverageBrowsing = mean(BrowsingScore))
>
	> print(fir_reg_browse)
# North_Shore_Forests has the lowest (1.0)
> barplot(
	+	fir_reg_browse$AverageBrowsing,
	+	names.arg = fir_reg_browse$Ecoregion,
	+	xlab = "Ecoregion",
	+	ylab = "Average Browsing Intensity",
	+	main = "Average Moose Browsing Intensity on Balsam Fir by Ecoregion",
	+	col = "forestgreen",
	+	cex.names = 0.6)
> spruce_reg_browse <- sap_clean %>%
	+	filter(Species == "Black_Spruce") %>%
	+	group_by(Ecoregion) %>%
	+	summarize(AverageBrowsing = mean(BrowsingScore))
> print(spruce_reg_browse)
# Black Spruce generally has lower browsing intensity compared to Balsam Fir across ecoregions, meaning that moose show a stronger browsing preference for Balsam Fir compared to Black Spruce.
> sap_reg_tally <- sap_clean 5>%
	+	group_by(Ecoregion) %>%
	+	tally() %>%
	+	print()
> sap_spe_tally <- sap_clean %>%
	+	group_by(Species) %>%
	+	tally() %>%
	+	print()
# The SaplingStudy dataset is definitely not evenly distributed, North_Shore_Forests (8) and Northern_Peninsula_Forests (7) are overrepresented, while StraitofBelleIsleBarrens (1) and Maritime_Barrens (3) are both underrepresented. Likewise, Balsam_Fir (11) and Black_Spruce (9) are sampled more often than Black_Ash (1), meaning uneven sampling across both ecoregions and species.

# Understanding bias in ecological datasets is very important because uneven sampling can cause incorrect conclusions about ecological patterns. When some groups are sampled more than others, the results could be misleading or incomplete.
> library(dpyler)
> moose_2020b <- moose_clean %>%
	+	filter(Year == 2020) %>%
	+	mutate(MooseDensity = Estimated_Moose_Pop / Area)
moose_sap <- left_join(moose_2020b, sap_clean, by = 'Ecoregion', relationshipp = "many-to-many")
> sum_spe_browse <- sap_clean %>%
	+	group_by(Species, Ecoregion) %>%
	+	summarize(AverageBrowsing = mean(BrowsingScore))
> ggplot(sum_spe_browse,
		+	aes(x = Species, y = AverageBrowsing, color = Species)) +
	+	geom_point(size = 3) +
	+	theme_minimal() +
	+	labs(title = "Average Moose Browsing Score by Species",
		+	x = "Species",
		+	y = "Average Browsing Score" )

# Moose show a higher browsing on prefered species (e.g., Black Ash and Balsam Fir) even at low densities, and usually browse less-preferred species more often when moose density is higher, reinforcing the hypothesis that moose are selective at low desnity and more generalist at high density.

# Moose prefer Black Ash and Balsam Fir the most, with the highest average browsing scores. Black Spruce is browsed the least, with consistently lower average scores.

# Black Ash might not appear on the figure if it was sampled very few times (n = 1), because it does not have enough data to plot reliably.
> collisions2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
> human_pop <- c(18000, 12000, 4000, 75100, 24000, 3500, 32000, 270000, 2300)
> study_sites <- c("North_Shore_Forests", "Nothern_Peninsula_Forests", "Long_Range_Barrens",

"Central_Forests", "Westerb_Forests", "EasternHyperOceanicBarrens", "Maritime_Barrens", "Avalon_Forests", "StraitOfBelleIsleBarrens")
>
	> moose_coll <- data.frame(collisons2020, human_pop, study_sities)
> moose_coll2 <- moose_coll %>%
	+	rename_with(-"Ecoregion", study_sites)
> coll_merge <- moose_2020 %>%
	+	left_join(moose_coll2, by = "Ecoregion")
>
	> print(coll_merge)
> plot(coll_merge$MooseDensity, coll_merge$collisions2020,
		+	xlab = "Moose Density",
		+	ylab = "Number of Moose-Vehicle Collsions (2020)",
		+	main = "Relationship Between Moose Density and Vehicle Collisions")
# The scatter plot represents a positive relationship between moose density and the number of moose-vehicle collisions, with collisions generally increasing as moose density increases. One obvious outlier has a very high number of collisions despite only moderate moose density, recommending that other factors such as human population or traffic volume also impact collision rates.
> coll_merge_per_capita <- coll_merge %>%
	+	mutate(coll_per_capitia = collisions2020 / human_pop) %>%
	+	print()
> plot(coll_merge_per_capita$human_pop,
		+	coll_merge_per_capita$coll_per_capita,
		+	xlab = "Human Population",
		+	ylab = "Moose Collisions per Capita",
		+	main = "Moose Collisions per Capita vs Human Population"
		+	pch = 16)
# The plot represents that moose collisions per capita decrease as human population increases, with the highest per-person collision rates occuring in sparsely populated ecoregions. This makes sense in Newfoundland because rural areas have more moose habitat and fewer people, increasing the likelihood that individuals encounter moose while driving.
	

