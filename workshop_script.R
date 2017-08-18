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

##Challenge ---- 
#Create a new data frame from the surveys data that meets the
#following criteria: contains only the species_id column and a new column called
#hindfoot_half containing values that are half the hindfoot_length values. In
#this hindfoot_half column, there are no NAs and all values are less than 30

survey_half <- surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_half = hindfoot_length/2) %>% 
  filter(hindfoot_half <30) %>% 
  select(species_id, hindfoot_half)

##  ----