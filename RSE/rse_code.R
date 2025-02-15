# Code for Rosenberg Self-Esteem (RSE) Analysis

# Dataset source: https://openpsychometrics.org/_rawdata/



# --------------------------------------



# Load libraries
library(dplyr)
library(ggplot2)
library(e1071)



# --------------------------------------



# Load the dataset
rse <- read.csv("rse_dataset.csv", sep = "\t")

# Preview the dataset
head(rse, 10)



# --------------------------------------



# Check data types

## Glimpse the dataset
glimpse(rse)

## `gender` and `source` are `int` instead of `factor`

## Convert `gender` to `factor`
rse$gender <- factor(rse$gender,
                     levels = c(0, 1, 2, 3),
                     labels = c("Not indicated",
                                "Male",
                                "Female",
                                "Other"))

## Convert `source` to `factor`
rse$source <- factor(rse$source,
                     levels = c(1, 2, 3),
                     labels = c("Front page",
                                "Google search",
                                "Other"))

## Check the results
glimpse(rse[c("gender", "source")])

## `gender` and `source` are now `factor`.



# --------------------------------------



# Handle missing values

## Check if there is any NA
anyNA(rse)

## Get the total number of NA
sum(is.na(rse))

## Check the number of NA per column
colSums(is.na(rse))

## Only `country` contains missing values.
## Since there are only 4 missing values, I will impute with mode.

## Define a function to get mode
mode <- function(x) {
  x_uniq <- unique(x)
  x_uniq[which.max(tabulate(match(x, x_uniq)))]
}

## Get mode for `country`
country_mode <- mode(rse$country)

## Impute `country`
rse$country[is.na(rse$country)] <- country_mode

## Chech the results
colSums(is.na(rse))

## There is now no missing value in the dataset.


## From the codebook, I found that 0 in `age` means the value cannot be converted to integre, which means 0 represents NA in `age`.

## Count the number of 0 in `age`
sum(rse$age == 0)

## Convert 0 to NA
rse$age[rse$age == 0] <- NA

## Check the number of NA in `age`
sum(is.na(rse$age))

## See the distribution of `age`
ggplot(rse[!is.na(rse$age), ], aes(x = age)) +
  geom_histogram() +
  theme_bw()

## `age` seems to be clustered in one place.

## Check the distribution of `age` with a box plot
ggplot(rse[!is.na(rse$age), ], aes(x = age)) +
  geom_boxplot() +
  theme_classic()

## Check the min and max of `age`
min(rse$age, na.rm = TRUE)
max(rse$age, na.rm = TRUE)

## I found that max `age` = 2147483647. Thus, there is a range issue.

## Handle outliers in `age`

### Calculate IQR
age_iqr <- IQR(rse$age, na.rm = TRUE)

### Calculate lower and upper bounds
age_lowerbound <- quantile(rse$age, 0.25, na.rm = TRUE) - (age_iqr * 1.5)
age_upperbound <- quantile(rse$age, 0.75, na.rm = TRUE) + (age_iqr * 1.5)

### Filter out outliers
rse <- rse |>
        filter(age >= age_lowerbound & age <= age_upperbound)

## Check the results
ggplot(rse[!is.na(rse$age), ], aes(x = age)) +
  geom_boxplot() +
  theme_classic()

### Outliers are successfully eliminated.
### Back to handling NA.

## Check the distribution of `age`
ggplot(rse[!is.na(rse$age), ], aes(x = age)) +
  geom_density() +
  theme_bw()

## Get skewness
skewness(rse$age, na.rm = TRUE)

## Both the density plot and skewness confirm that `age` is positively skewed.

## Thus, I will impute `age` with median instead of mean.

rse$age[is.na(rse$age)] <- median(rse$age, na.rm = TRUE)

## Check the results
sum(is.na(rse$age))

## `age` is now NA-free.



# --------------------------------------





 
# --------------------------------------



