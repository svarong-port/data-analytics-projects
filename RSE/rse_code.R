# Code for Rosenberg Self-Esteem (RSE) Analysis

# Dataset source: https://openpsychometrics.org/_rawdata/



# --------------------------------------



# Load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(e1071)



# --------------------------------------



# Load the dataset
rse <- read.csv("rse_dataset.csv", sep = "\t")

# Preview the dataset
head(rse, 10)
dim(rse)

# Make a copy of the dataset for transformation
rse_1 <- data.frame(rse)



# --------------------------------------



# Check data types

# Glimpse the dataset
glimpse(rse_1)

## `gender` and `source` are `int` instead of `factor`

## Convert `gender` to `factor`
rse_1$gender <- factor(rse_1$gender,
                       levels = c(0, 1, 2, 3),
                       labels = c("Not indicated",
                                  "Female",
                                  "Male",
                                  "Other"))

## Convert `source` to `factor`
rse_1$source <- factor(rse_1$source,
                       levels = c(1, 2, 3),
                       labels = c("Front page",
                                  "Google search",
                                  "Other"))

## Check the results
glimpse(rse_1[c("gender", "source")])

## `gender` and `source` are now `factor`.



# --------------------------------------



# Handle missing values

## Check if there is any NA
anyNA(rse_1)

## Get the total number of NA
sum(is.na(rse_1))

## Check the number of NA per column
colSums(is.na(rse_1))

## Only `country` contains missing values.
## Since there are only 4 missing values, I will impute with mode.

## Define a function to get mode
mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

## Get mode for `country`
country_mode <- mode(rse_1$country)

## Impute `country`
rse_1$country[is.na(rse_1$country)] <- country_mode

## Check the results
colSums(is.na(rse_1))

## There is now no explicit missing value in the dataset.



## From the codebook, I found that 0 in `age` means the value cannot be converted to integre, which means 0 represents NA in `age`.

## Count the number of 0 in `age`
sum(rse_1$age == 0)

## There are 568 missing values in `age`.

## Convert 0 to NA
rse_1$age[rse_1$age == 0] <- NA

## Check the number of NA in `age`
sum(is.na(rse_1$age))

## All `0` have been converted to `NA`.

## Next, I will impute `NA` with mean or median.

## See the distribution of `age`
ggplot(rse[!is.na(rse_1$age), ], aes(x = age)) +
  geom_histogram() +
  theme_bw()

## `age` seems to be clustered in one place.

## Check the distribution of `age` with a box plot
ggplot(rse_1[!is.na(rse_1$age), ], aes(x = age)) +
  geom_boxplot() +
  theme_classic()

## `age` seems to have usual range.

## Check the min and max of `age`
min(rse_1$age, na.rm = TRUE)
max(rse_1$age, na.rm = TRUE)

## Min = 1
## Max = 2147483647
## Both are very unlikely to be true.
## Conclusion: `age` has extreme outliers.

## Handle outliers in `age`

### Calculate IQR
age_iqr <- IQR(rse_1$age, na.rm = TRUE)

### Calculate lower and upper bounds
age_lowerbound <- quantile(rse_1$age, 0.25, na.rm = TRUE) - (age_iqr * 1.5)
age_upperbound <- quantile(rse_1$age, 0.75, na.rm = TRUE) + (age_iqr * 1.5)

### Filter out outliers
rse_1 <- rse_1 |>
  filter(age >= age_lowerbound &
           age <= age_upperbound)

## Check the results
ggplot(rse_1[!is.na(rse_1$age), ], aes(x = age)) +
  geom_boxplot() +
  theme_classic()

min(rse_1$age)
max(rse_1$age)

## Min = 1
## Max = 53
## `age` still have data from someone who identified as a 1-year-old.

## Check the number of people younger than 10 (10 = a rule of thumb, the age where children should be able to read somewhat proficiently)
rse_1 |>
  filter(age < 10) |>
  summarise(n())

## There are 62 records that meet the criterion.

## Drop these records
rse_1 <- rse_1 |>
  filter(age >= 10)

## Check the results
min(rse_1$age)
max(rse_1$age)

## Min = 10
## Max = 53

## The outlier issue with `age` has been handled.

## Back to handling NA.

## Check the distribution of `age`
ggplot(rse_1[!is.na(rse_1$age), ], aes(x = age)) +
  geom_density() +
  theme_bw()

## Get skewness
skewness(rse_1$age, na.rm = TRUE)

## Skewness = 1.109776

## Both the density plot and skewness confirm that `age` is positively skewed.

## Thus, I will impute `age` with median instead of mean.

rse_1$age[is.na(rse_1$age)] <- median(rse_1$age, na.rm = TRUE)

## Check the results
sum(is.na(rse_1$age))

## `age` is now NA-free.



## From the codebook, `0` in item columns (i.e., Q1-Q10) indicates no answer. Thus, `0` in these columns also represents `NA`.

## Check the number of `0` in each item column
rse_1 |>
  summarise(across(Q1:Q10, ~ sum(. == 0)))

## Drop rows with `0` in the item columns
rse_1 <- rse_1 |>
  filter(if_all(Q1:Q10, ~ . != 0))

## Check the results
rse_1 |>
  summarise(across(Q1:Q10, ~ sum(. == 0)))

dim(rse_1)



# --------------------------------------



# Calculate total RSE score

## Make a copt of rse_1
rse_2 <- data.frame(rse_1)

## Reverse-score items Q3, 5, 8, 9, 10

### Define a function to reverse-score items
reverse_sore <- function(df, col) {
  df[[col]] = case_when(df[[col]] == 1 ~ 5,
                        df[[col]] == 2 ~ 4,
                        df[[col]] == 3 ~ 3,
                        df[[col]] == 4 ~ 2,
                        df[[col]] == 5 ~ 1)
  
  return(df)
}

### Create a list of columns to be reverse-scored
rs_cols <- c("Q3", "Q5", "Q8", "Q9", "Q10")

### Reverse-score with a for loop
for (col in rs_cols) {
  rse_2 <- reverse_sore(rse_2, col)
}

### Check the results
head(rse_1$Q3)
head(rse_2$Q3)

head(rse_1$Q10)
head(rse_2$Q10)



## Calculate the sum score
rse_2 <- rse_2 |>
  mutate(rse_total = rowSums(across(Q1:Q10)))
  
## Check the results
rse_2 |>
  select(Q1:Q10, rse_total) |>
  head()

## Successfully created sum score.
 


# --------------------------------------



# Explore the data

## Gender vs RSE
rse_2 |>
  group_by(gender) |>
  summarise(mean_rse = mean(rse_total)) |>
  ggplot(aes(x = gender, 
             y = mean_rse,
             fill = gender)) +
  geom_col() +
  theme_bw() +
  labs(title = "Mean RSE by Gender",
       x = "Gender",
       y = "Mean RSE",
       legend = "Gender")

## There seems to be little difference between groups, although "Other" seems to have lower RSE than the other groups.



## Age vs RSE
rse_2 |>
  group_by(age) |>
  summarise(mean_rse = mean(rse_total)) |>
  ggplot(aes(x = age, 
             y = mean_rse)) +
  geom_line(color = "red") +
  theme_bw() +
  labs(title = "Mean RSE Across Age",
       x = "Age",
       y = "Mean RSE")

## Based on the line plot:
## - RSE is high around 12
## - It dramatically drops to the lowest point at 14
## - Then it steadily increases through 50s



## Country vs RSE
rse_2 |>
  group_by(country) |>
  summarise(mean_rse = mean(rse_total)) |>
  ggplot(aes(x = country, 
             y = mean_rse,
             fill = country)) +
  geom_col() +
  theme_bw() +
  labs(title = "Mean RSE by Country",
       x = "Country",
       y = "Mean RSE",
       legend = "Country")

## I cannot recognise any meaninful patterns as there are too many countries in the dataset.



## Source vs RSE
rse_2 |>
  group_by(source) |>
  summarise(mean_rse = mean(rse_total)) |>
  ggplot(aes(x = source, 
             y = mean_rse,
             fill = source)) +
  geom_col() +
  theme_bw() +
  labs(title = "Mean RSE by Source",
       x = "Source",
       y = "Mean RSE",
       legend = "Source")

## There seems to be no significant difference between groups.


## I will try to group the country by continent to make it easier to identify a pattern, if any.

### Import the list of countries (from https://www.geonames.org/countries/)
country_codes <- read.csv("geolite_countries.csv")

### Check the results
head(country_codes)

### Left join rse_2 and country_codes
rse_3 <- left_join(rse_2,
                   country_codes,
                   by = "country")

### Check the results
head(rse_3)

### Continent vs RSE
rse_3 |>
  group_by(continent) |>
  summarise(mean_rse = mean(rse_total)) |>
  ggplot(aes(x = continent,
             y = mean_rse,
             fill = continent)) +
  geom_col() +
  theme_bw() +
  labs(title = "Mean RSE by Continent",
       x = "Age",
       y = "Mean RSE",
       legend = "Continent")

## Based on the plot:
## - Highest mean = Asia Pacific and Asia
## - Lowest mean = South America


## Gender x Age vs RSE
rse_2 |>
  group_by(gender, age) |>
  summarise(mean_rse = mean(rse_total)) |>
  ggplot(aes(x = age,
             y = mean_rse,
             fill = gender)) +
  geom_line() +
  facet_wrap(~gender) +
  theme_bw() +
  labs(title = "Mean RSE by Gender x Age",
       x = "Age",
       y = "Mean RSE",
       legend = "Gender")

## Based on the plot:
## - "Male" and "Female" have similar patterns
## - "Not indicate" and "Other" have similar patterns
## - The two groups are different