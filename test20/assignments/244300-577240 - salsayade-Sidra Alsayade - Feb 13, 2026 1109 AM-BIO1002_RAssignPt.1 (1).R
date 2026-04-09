library(dplyr)
ShrubData <- read.csv("shrub-volume-data.csv")

#Q1
select(ShrubData, site, experiment)

#Q2
mutate(ShrubData, area=length*width)

#Q3
arrange(ShrubData, width) #Q.3 (a)

#Q4
arrange(ShrubData, length)

#Q5
filter(ShrubData, length > 5)

#Q6
filter(ShrubData, length > 4, width > 2)

#Q7
filter(ShrubData, experiment==1 | experiment==2)

#Q8
filter(ShrubData, !is.na(width))

## NEW SECTION - Maximum Monthly Temperature (Q9)
MaxTemp <- read.csv("max-temperature.csv")
Table.1 <- filter(MaxTemp, !is.na(Max.Temp.C))
by_site <- group_by(Table.1, Month)
data2 <- summarize(by_site, Max.Temp.C=mean(Max.Temp.C))
plot(data2$Month, data2$Max.Temp.C)

## NEWSECTION - Bird Banding
number_of_birds <- c(28, 32, 1, 0, 10, 22, 30, 19, 145, 27, 
                     36, 25, 9, 38, 21, 12, 122, 87, 36, 3, 0, 5, 55, 62, 98, 32, 
                     900, 33, 14, 39, 56, 81, 29, 38, 1, 0, 143, 37, 98, 77, 92, 
                     83, 34, 98, 40, 45, 51, 17, 22, 37, 48, 38, 91, 73, 54, 46,
                     102, 273, 600, 10, 11)
#Q10: 61 sites

sum(number_of_birds)
#Q11: 4366 birds

#Q12: 0, or 1 birds

#Q13: 900 birds

mean(number_of_birds)
#Q14: average of 71.6 birds

birds_last_site <- number_of_birds[length(number_of_birds)]
birds_last_site
#Q15: 11 birds

birds_at_site_42 <- number_of_birds[42]
birds_at_site_42
#Q16: 83 birds at site 42

## NEW SECTION - Portal Data

#Q17
Survey <- read.csv("surveys.csv")

#Q18
survey_new <- filter(Survey, !is.na(weight))

#Q19
survey_new_2 <- select(survey_new, year, month, day, species_id, weight)

#Q20
survey_new_3 <- mutate(survey_new_2, weight_kg=weight*0.001)

#Q21
survey_new_4 <- filter(survey_new_3, species_id == "SH")

#Q22
head(survey_new_4, 6)


## NEW SECTION - Using Pipes

#Q23
survey_new_5 <- filter(Survey, !is.na(weight)) %>%
  select(year, month, day, species_id, weight) %>%
  mutate(weight_kg=weight*0.001) %>%
  filter(species_id == "SH")
survey_new_5

#Q24
head(survey_new_5, 6)
