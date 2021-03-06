## script for the data carpentry workshop 2017
## Genevieve Mullins gmullins@ucmerced.edu
## August 17-18, 2017
## Git -- gnmullins

## Day 1 ----
## download data ----
# download.file("https://ndownloader.figshare.com/files/2292169",
#               "data/portal_data_joined.csv")

## read in data into R ----
surveys <- read.csv("data/portal_data_joined.csv")

## explore our data ----
head(surveys) #show first 6 rows of data, can specify number of rows
tail(surveys) #show last 6 rows of data, can specify number of rows
view(surveys) #open table window of data
str(surveys) #structure of data
summary(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)
names(surveys)

## the $ operator for isolating columns ----
tail(surveys$weight)
str(surveys$weight)
summary(surveys$weight)
summary(surveys$month)

## plot in base R ----
plot(surveys$year, surveys$weight)
plot(surveys$hindfoot_length, surveys$weight)
hist(surveys$month, col = "gray", breaks = 12)

## factor variable ----
summary(surveys$taxa)
levels(surveys$taxa)
nlevels(surveys$taxa)
hist(surveys$taxa) #error, need numeric
class(surveys$taxa) #what type of data is it?
table(surveys$taxa) #make table
class(table(surveys$taxa)) #now it is a table
barplot(table(surveys$taxa))

## subset in base R ----
## [rows, columns]

surveys[surveys$genus == "Ammodramus", ] #return all columns for genus Ammodramus
surveys[surveys$genus == "Ammodramus", 
        c("record_id", "month", "weight")] #return a few columns for this genus

##how many observations for months Jan and Feb ----
nrow(surveys[surveys$month == 1 | surveys$month == 2,])
table(surveys$month == 1 | surveys$month == 2)
length(which(surveys$month <3))
summary(surveys$month == 1 | surveys$month == 2)
summary(as.factor(surveys$month))
summary(as.factor(surveys$month == 1 | surveys$month == 2))



## Day 2 ----
## Random ----
rodent_hindfoot <- surveys[surveys$taxa == "Rodent",
                           "hindfoot_length"]
rodent_weight <- surveys[surveys$taxa == "Rodent",
                         "weight"]
plot(rodent_hindfoot, rodent_weight)

## Get Tidyverse ----
#install.packages("tidyverse")
library("tidyverse", lib.loc="~/R/win-library/3.4") #load tidyverse into R

##Working with dplyr ----
select(surveys, plot_id, species_id, weight) #selecting columns
filter(surveys, year == 1995) #filter by year

##Pipes %>% ----
#take survey, put into filter, take output of that, and put through select
Surveys_1995 <- surveys %>%
  filter(year == 1995) %>% 
  select(year, plot_id, species_id, weight)

## Adding columns ----
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000,
         weight_kg2 = weight_kg*2) %>% 
  head

##Challenge 1 ---- 
#Create a new data frame from the surveys data that meets the
#following criteria: contains only the species_id column and a new column called
#hindfoot_half containing values that are half the hindfoot_length values. In
#this hindfoot_half column, there are no NAs and all values are less than 30

survey_half <- surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_half = hindfoot_length/2) %>% 
  filter(hindfoot_half <30) %>% 
  select(species_id, hindfoot_half)
summary(survey_half)

survey_half2 <- surveys %>% 
  mutate(hindfoot_half = hindfoot_length/2) %>% 
  filter(!is.na(hindfoot_half) & hindfoot_half <30) %>% 
  select(species_id, hindfoot_half)
summary(survey_half2)

## group_by and summarize ----
#group_by does it in order specified
surveys %>% 
  filter(!is.na(weight),
         sex == "F" | sex == "M") %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight),
            med_weight = median(weight))
surveys %>% 
  group_by(sex) %>% 
  tally


##Challenge 2 ---- 
#How many individuals were caught in each plot_type surveyed? 

surveys %>% 
  group_by(plot_type) %>% 
  tally

#Use group_by() and summarize() to find the mean, min, and max hindfoot length for each species (using species_id). 

surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarize(mean_hindfoot_length = mean(hindfoot_length),
            min_hindfoot_length = min(hindfoot_length),
            max_hindfoot_length = max(hindfoot_length))

#What was the heaviest animal measured in each year? 
#Return the columns year, genus, species_id, and weight. Hint: does not use summarize

heavy_animals <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>% 
  select(year, genus, species_id, weight) %>% 
  arrange(year)

heavy_animals2 <- surveys %>% 
  select(year, genus, species_id, weight) %>% 
  group_by(year) %>% 
  top_n(1, weight) %>% 
  arran(year)

#You saw above how to count the number of individuals of each sex using a combination of
#group_by() and tally(). How could you get the same result using group_by() and
#summarize()? Hint: see ?n.

surveys %>% 
  group_by(sex) %>% 
  summarize(n())

## Exporting data ----
#clean
surveys_clean <- surveys %>% 
  filter(species_id != "") %>% #remove missing species
  filter(!is.na(weight)) %>% #remove weight NAs
  filter(!is.na(hindfoot_length)) %>% #remove hindfoot_length NAs
  filter(sex != "") #remove blank sex

surveys_clean <- surveys %>% 
  filter(species_id != "",
         sex != "",
         !is.na(weight),
         !is.na(hindfoot_length))

#extract common species_id
species_counts <- surveys_clean %>% 
  group_by(species_id) %>% 
  tally() %>% 
  filter(n>= 50)

#keep most common species
surveys_complete <- surveys_clean %>% 
  filter(species_id %in% species_counts$species_id)

#save data
write.csv(surveys_complete, file="data_output/surveys_complete.csv",
          row.names=FALSE)

## Data visualization ----

#load data
surveys_complete <- read.csv("data_output/surveys_complete.csv")

#ggplot2 scatter

ggplot(data=surveys_complete, aes(x=weight, y=hindfoot_length)) +
  geom_point(alpha = 0.06, aes(color= species_id))

##Challenge 3 ---- 
#Use what you just learned to create a scatter plot of weight
#over species_id with the plot types showing in different colors. 
#Is this a good way to show this type of data? NO

ggplot(data=surveys_complete, aes(x=species_id, y=weight)) +
  geom_point(alpha = 0.06, aes(color= plot_type)) 

## More plots ----

#boxplot
ggplot(data=surveys_complete, aes(x=species_id, y=weight)) +
  geom_boxplot()

ggplot(data=surveys_complete, aes(x=species_id, y=weight)) +
  geom_boxplot(aes(color=plot_type)) +
  facet_grid(sex~.) +
  labs(x="Species",
       y="Weight",
       title="Plot")

ggplot(data=surveys_complete, aes(x=species_id, y=weight)) +
  geom_boxplot(aes(color=sex)) +
  labs(x="Species",
       y="Weight",
       title="Plot")

##time series ----

yearly_counts <- surveys_complete %>% 
  group_by(year, species_id) %>% 
  tally()

ggplot(data=yearly_counts, aes(x=year, y=n,
                               group=species_id,
                               color=species_id)) +
  geom_line() +
  facet_wrap(~ species_id)
#can just use color and not group, as color will group and color

yearly_sex_counts <- surveys_complete %>% 
  group_by(year, species_id, sex) %>% 
  tally()

ggplot(data=yearly_sex_counts, aes(x=year, y=n,
                                   color=sex))+
  geom_line()+
  facet_wrap(~species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())

##Challenge 4 ----
#Use what you just learned to create a plot that depicts 
#how the average weight of each species changes through the years.

average_weight <- surveys_complete %>% 
  group_by(year, species_id) %>% 
  summarise(mean_weight = mean(weight))

ggplot(data=average_weight, aes(x=year, y=mean_weight,
                                color=species_id)) +
  geom_line() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  facet_wrap(~species_id) +
  labs(x="Year",
       y="Av. Weight(g)")

## save plot ----
my_plot <- ggplot(data=average_weight, aes(x=year, y=mean_weight,
                                color=species_id)) +
  geom_line() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  facet_wrap(~species_id) +
  labs(x="Year",
       y="Av. Weight(g)")

ggsave("my_plot.png", plot = my_plot, device="png")
