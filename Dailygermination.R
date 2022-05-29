# install libraries -------------------------------------------------------------------------
## Don't worry about the following code, just run it. only need to run once

install.packages("readxl")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("germinationmetrics")
install.packages("reshape2")
install.packages("segmented")
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
  ggplot(aes(Date.Time, Average.of.Seeds.Germinated)) +
  geom_point()


# visualisation 2----------------------------------------------------------
## Use the color argument to visualise the data by color
df_selected %>%
  ggplot(aes(Average.of.Days, Average.of.Seeds.Germinated, color = Temperature)) +
  geom_point()

##notes:
#1. Temperature is numeric values which shows as on a continuous scale and we
## won't be able to see the difference of seed germinated in different temperatures

# data manipulation 2------------------------------------------------------

df_germdaily <- df_selected %>%
  mutate(Date.Time = as.Date(Date.Time)) %>%
  group_by(Date.Time, PetriDishN.,Temperature) %>%
  summarise(Average.of.Seeds.Germinated = sum(Average.of.Seeds.Germinated))
# visualisation 3----------------------------------------------------------

df_germdaily %>%
  ggplot(aes(Date.Time, Average.of.Seeds.Germinated, color = Temperature)) +
  geom_point()
# Notes:
## 1. The experiment was conducted in two different times.
## 2. Actual date is irrelevant
## 3. So we need to do know the daily counts of germinated seeds

# data manipulation 3------------------------------------------------------

actual_date <- unique(df_germdaily$Date.Time)
Period <- ifelse(actual_date < "2021-04-01", 1, 2)
df_days <- data.frame(Date.Time = actual_date, Period = Period) %>%
  group_by(Period) %>%
  mutate(Days = 1:n())


df_germdaily <- df_germdaily %>%
  left_join(df_days, by = c("Date.Time"))

# visualisation 4----------------------------------------------------------

df_germdaily %>%
  ggplot(aes(Days, Average.of.Seeds.Germinated,
             color= Temperature)) +
  geom_point(size = 3)

##Exercises? change temperature to PetriDash number?


# data manipulation 4------------------------------------------------------
## Calculate the cumulative seed germination rate.

df_cumsum <- df_germdaily %>%
  ## Use group_by functions to group data into smaller chunks by Temperature and Petridish number
  group_by(Temperature, PetriDishN.)  %>%
  ## Use mutate function to calculate the cumulative sum (the cumsum function)
  mutate(cummulative_germination = cumsum(Average.of.Seeds.Germinated))
# visualisation 5----------------------------------------------------------

df_cumsum %>%
  ggplot(aes(Days, cummulative_germination, color= Temperature)) +
  geom_point(size = 3) +
  ## Use smooth function to indicate a general pattern among the data
  geom_smooth()

# data manipulation 5 and non-updating visualisation ----------------------
df_cumsum %>%
  ## use mutate function to change Temperature column from numeric to character
  mutate(Temperature = as.character(Temperature)) %>%
  ggplot(aes(Days, cummulative_germination, color= Temperature)) +
  geom_point(size = 3) +
  ## Use smooth function to indicate a general pattern among the data
  geom_smooth()

## But this won't give us the formula for the germination rates.
## So we need the specific function to fit the curves.
df_germdailysum <- df_germdaily %>%
  group_by(Temperature, Days) %>%
  summarise(Average.of.Seeds.Germinated = mean(Average.of.Seeds.Germinated))
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

# data manipulation 6------------------------------------------------------
gcdf <- df_germdailysum %>%
  ## Change the data to wider format to match the function input requirements
  pivot_wider(id_cols = Temperature,
              names_from = Days,
              values_from = Average.of.Seeds.Germinated,
              values_fill = 0) %>%
  ## Add a new column to indicate the total seed number for the function
  mutate(TotalSeed = 50)
## Create a new object to save the column name
counts.per.intervals <- as.character(unique(df_germdaily$Days))
## Create a new object to save the number of days
intervales <- length(counts.per.intervals)
## Call the fitting function
fitted <- FourPHFfit.bulk(data = gcdf,
                          total.seeds.col = "TotalSeed",
                          counts.intervals.cols = counts.per.intervals,
                          intervals = 1:intervales,tmax = 30, tries = 100)
# visualisation 6----------------------------------------------------------

plot(fitted,group.col = "Temperature", show.points = TRUE,  annotate = "t50.germ")

# select the 50% germination  ---------------------------------------------
germinated50 <- fitted %>%
  ## Double colons are called namespace operator more details:https://r-pkgs.org/namespace.html
  ## In short, it is for specifying a function from a package
  dplyr::select(Temperature, t50.total)


# visualise the days to 50% germination ~ temperature ---------------------

germinated50 %>%
  ggplot(aes(Temperature, t50.total)) +
  geom_point(size = 3) +
  theme_light()+
  labs(y = "Days to 50% germination")


# fit a broken stick lines ------------------------------------------------

library(segmented)
## Convert to 1/d
germinated50 <- germinated50 %>%
  mutate(t50.Germinated = 1/t50.total)
## Prepare a linear model for the broken sticks fitting
lmfit <- lm(t50.Germinated ~  Temperature, data = germinated50)
## Fit linear model to the broken stick function
seglmfit <- segmented(lmfit, seg.Z = ~Temperature, npsi = 1)
#https://stackoverflow.com/questions/33164639/how-to-use-ggplot2-to-plot-results-from-segmented-package

germinated50 %>%
  ## Drop missing values (na) before visualisation
  drop_na() %>%
  ggplot(aes(x = Temperature, y = t50.Germinated)) +
  geom_point(size = 3) +
  ## broken.line function to extract the fitted values from the broken sticks function
  geom_line(aes(x = Temperature, y =  broken.line(seglmfit)$fit), color = 'blue')+
  theme_light()+
  labs(y = "1/d")

## Details about the broken sticks fitting
summary(seglmfit)
# Extract the optimal temperature
seglmfit$psi

# Extract the coeficiencies
seglmfit$coefficients

