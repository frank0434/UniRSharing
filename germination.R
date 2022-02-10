# install libraries -------------------------------------------------------------------------

Required_pkgs <- c("readxl", "tidyverse", "ggplot2",
                   "magrittr", "germinationmetrics")

## Don't worry about the following code, just run it.

for(i in Required_pkgs){

  if(isFALSE(require(i, quietly = T))){
    cat("Required libraries are not installed. Installing", i,"\n")
    install.packages(i)
  } else {
      print("Good to go.")
    }


}

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

df_selected %>%
  ggplot(aes(Date.Time, Average.of.Seeds.Germinated)) +
  geom_point()

##notes:
#1. the experiment run twice: one in March and the other in May.

# data manipulation 2------------------------------------------------------
## We will look at the first set of data
df_selected1 <- df_selected %>%
  ## use filter function to keep only date smaller than 2021-04-01
  filter(Date.Time < "2021-04-01")

# visualisation 2----------------------------------------------------------
df_selected1 %>%
  ggplot(aes(Date.Time, Average.of.Seeds.Germinated, color = Temperature)) +
  geom_point()

##notes:
#1. Temperature is numeric values which shows as on a continuous scale
# data manipulation 3------------------------------------------------------

df_selected1 <- df_selected %>%
  filter(Date.Time < "2021-04-01") %>%
  ## use mutate function to change Temperature column from numeric to character
  mutate(Temperature = as.character(Temperature))

# visualisation 3----------------------------------------------------------

df_selected1 %>%
  ggplot(aes(Date.Time, Average.of.Seeds.Germinated,
             color= Temperature)) +
  geom_point(size = 3)

##Exercises? change temperature to PetriDash number?


# data manipulation 3------------------------------------------------------
## Calculate the cumulative seed germination rate.

df_cumsum1 <- df_selected1 %>%
  ## Use group_by functions to group data into smaller chunks by Temperature and Petridish number
  group_by(Temperature, PetriDishN.)  %>%
  ## Use mutate function to calculate the cumulative sum (the cumsum function)
  mutate(cummulative_germination = cumsum(Average.of.Seeds.Germinated))
# visualisation 4----------------------------------------------------------

df_cumsum1 %>%
  ggplot(aes(Date.Time, cummulative_germination, color= Temperature)) +
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
gcdf <- df_cumsum1 %>%
  select(Temperature, PetriDishN., Average.of.Seeds.Germinated, Date.Time) %>%
  pivot_wider(id_cols = c(Temperature, PetriDishN.),
              names_from = Date.Time,
              values_from = Average.of.Seeds.Germinated,
              values_fill = 0) %>%
  mutate(TotalSeed = 50)

counts.per.intervals <- as.character(unique(df_cumsum1$Date.Time))
fitted <- FourPHFfit.bulk(data = gcdf,
                          total.seeds.col = "TotalSeed",
                          counts.intervals.cols = counts.per.intervals,
                          intervals = 1:18,tmax = 20)
plot(fitted,group.col = "Temperature")

## Average the germinated seed across the Petri Dishes

df_summary1 <- df_selected1 %>%
  group_by(Temperature, Date.Time) %>%
  summarise(mean_seed_g = mean(Average.of.Seeds.Germinated, na.rm = TRUE),
            mean_days = mean(Average.of.Days, na.rm = TRUE),
            sd_seed_g = sd(Average.of.Seeds.Germinated, na.rm = TRUE)) %>%
  group_by(Temperature) %>%
  mutate(cummulative_germination = cumsum(mean_seed_g))

df_summary1 %>%
  ggplot(aes(Date.Time, cummulative_germination, color= Temperature)) +
  geom_point(size = 3) +
  # geom_line() +
  geom_smooth()

%>%
  group_by(Temperature) %>%
  mutate(cummulative = cumsum(Average.of.Seeds.Germinated))


# fit the curves ----------------------------------------------------------


