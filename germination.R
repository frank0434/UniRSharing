# install libraries -------------------------------------------------------------------------
## Don't worry about the following code, just run it. only need to run once

install.packages("readxl")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("germinationmetrics")
install.packages("reshape2")

# load libraries ----------------------------------------------------------

library(readxl)     # Read excel files
library(tidyverse)  # Data manipulation
library(ggplot2)    # Visualisation
library(magrittr)   # Helper to chain command together
library(germinationmetrics)
# read data  --------------------------------------------------------------

## Read excel file in and save it into a data.frame called df.
## argument skip and .name_repair can determine where to start import the data
## and replace the spaces with dot in the column names, respectively.
df <- read_excel("Data/Data.xlsx", skip = 3, .name_repair = "universal")

## Use head function to view the top 5 rows
head(df)

## Use str function to quickly overview the data structure
str(df)
##notes:
#1. the column names with numeric numbers are instructions.

# data manipulation 1------------------------------------------------------

## Select the necessary columns. Tip: use TAB key on the keyboard
## The %>% (pipe) can take an object and pass it to next function.
df_selected <- df %>%
  select(Temperature, Date.Time, PetriDishN., Average.of.Seeds.Germinated,
         Average.of.Days)


# visualisation 1----------------------------------------------------------
## The actual time of experiment irrelevant, we will use the days.
df_selected %>%
  ggplot(aes(Average.of.Days, Average.of.Seeds.Germinated)) +
  geom_point()

# visualisation 2----------------------------------------------------------
## Use the color argument to visualise the data by color
df_selected %>%
  ggplot(aes(Average.of.Days, Average.of.Seeds.Germinated, color = Temperature)) +
  geom_point()

##notes:
#1. Temperature is numeric values which shows as on a continuous scale

# data manipulation 2------------------------------------------------------
# df_selected <- df_selected %>%
  ## use mutate function to change Temperature column from numeric to character
  # mutate(Temperature = as.character(Temperature))

# visualisation 3----------------------------------------------------------

df_selected %>%
  ggplot(aes(Average.of.Days, Average.of.Seeds.Germinated,
             color= Temperature)) +
  geom_point(size = 3)

##Exercises? change temperature to PetriDash number?


# data manipulation 3------------------------------------------------------
## Calculate the cumulative seed germination rate.

df_selected <- df_selected %>%
  group_by(Average.of.Days, Temperature, PetriDishN.) %>%
  summarise(Average.of.Seeds.Germinated = sum(Average.of.Seeds.Germinated))

df_cumsum <- df_selected %>%
  ## Use group_by functions to group data into smaller chunks by Temperature and Petridish number
  group_by(Temperature, PetriDishN.)  %>%
  ## Use mutate function to calculate the cumulative sum (the cumsum function)
  mutate(cummulative_germination = cumsum(Average.of.Seeds.Germinated))
# visualisation 4----------------------------------------------------------

df_cumsum %>%
  ggplot(aes(Average.of.Days, cummulative_germination, color= Temperature)) +
  geom_point(size = 3) +
  ## Use smooth function to indicate a general pattern among the data
  geom_smooth()
## But this won't give us the formula for the germination rates.


# curve fitting via germinationmetric package  ----------------------------
## To get help documentation -- super useful
?FourPHFfit.bulk()
head(gcdata)
## After reading through the documentation and having a look at the example,
## we know the following points:
## 1. The data fits into this function must have germination counts present in the columns by the time interval
## 2. Different Temperature treatments in the first column
## 3. Different Petri Dishes in the second column
## 4. Total seeds in each treatment+Petridishes in the last column.

# data manipulation 4------------------------------------------------------
gcdf <- df_cumsum %>%

  pivot_wider(id_cols = c(Temperature, PetriDishN.),
              names_from = Average.of.Days,
              values_from = Average.of.Seeds.Germinated,
              values_fill = 0) %>%
  mutate(TotalSeed = 50)

counts.per.intervals <- as.character(unique(df_cumsum$Average.of.Days))
intervales <- length(counts.per.intervals)
fitted <- FourPHFfit.bulk(data = gcdf,
                          total.seeds.col = "TotalSeed",
                          counts.intervals.cols = counts.per.intervals,
                          intervals = 1:intervales,tmax = 20)
plot(fitted,group.col = "Temperature", show.points = TRUE,  annotate = "t50.germ")

germinated50 <- fitted %>%
  select(Temperature,PetriDishN.,t50.Germinated)

germinated50 %>%
  group_by(Temperature) %>%
  summarise(t50.Germinated = mean(t50.Germinated)) %>%
  ggplot(aes(Temperature, 1/t50.Germinated)) +
  geom_point()+
  geom_smooth()


# temperature 3 have huge variations  -------------------------------------

df_selected %>%
  filter(Temperature == 3) %>%
  ggplot(aes(Average.of.Days, Average.of.Seeds.Germinated)) +
  geom_point()


