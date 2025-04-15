# Predicting Avocado Price With Machine Learning in R

## Prepared by
## Author: Shinin Varongchayakul
## Date: 15 Apr 2025
## Language: R

## Dataset
## Name: Avocado Price
## Source: https://www.kaggle.com/datasets/neuromusic/avocado-prices?hl=en-GB
## Retrieved Date: 15 Apr 2025


# ----------------------------------------------------------------


## Business Problems
## A grocery retail chain wants to optimise their avocado pricing as avocado is one of their best-selling items.
## They want to understand:
## - What factors influence avocado price
## - How the price changes over time
## - How the price changes across regions
## - Comparison of organic versus conventional pricing strategies
## They also want to be able to predict avocado price.
## Avocado Price dataset from Kaggle will be used to simulate the situation here.


## Analysis Objectives
## Given the business problems, the goals of this analysis are twofold:
## 1. Explore the pattern of avocado pricing across different factors
## 2. Build a predictive model to predict avocado pricing


# ----------------------------------------------------------------


## 1. Install and load necessary packages

## Install
install.packages("effsize") # effect size calculation
install.packages("ggcorrplot") # correlation matrix
install.packages("tidyverse") # data manipulation
install.packages("tidymodels") # machine learning
install.packages("lubridate") # date manipulation


## Load
library(effsize)
library(ggcorrplot)
library(tidyverse)
library(tidymodels)
library(lubridate)


# ----------------------------------------------------------------


## 2. Load the dataset

## Load
avocados <- read.csv("avocado_price_dataset.csv")

## Preview the first 6 row
head(avocados)

## View the structure
glimpse(avocados)

## Comments:
## - There are 18,249 records and 14 features.


# ----------------------------------------------------------------


## 3. Prepare the dataset

## Create a copy of the dataset
avd_cleaned <- avocados


## Convert `Date` to Date
avd_cleaned$Date <- as.Date(avd_cleaned$Date)

## Check the results
glimpse(avd_cleaned)


## Extract month from `Date`
avd_cleaned$month <- format(avd_cleaned$Date, "%B")

## Convert `month` to factor
avd_cleaned$month <- factor(avd_cleaned$month,
                            levels = month.name,
                            ordered = TRUE)

## Check the results
glimpse(avd_cleaned)


## Group region into more meaningful regions

## Create a data frame for region grouping
region_groups <- data.frame(
  region = c(
    "Albany", "Atlanta", "BaltimoreWashington", "Boise", "Boston", "BuffaloRochester",
    "California", "Charlotte", "Chicago", "CincinnatiDayton", "Columbus", "DallasFtWorth",
    "Denver", "Detroit", "GrandRapids", "GreatLakes", "HarrisburgScranton", "HartfordSpringfield",
    "Houston", "Indianapolis", "Jacksonville", "LasVegas", "LosAngeles", "Louisville",
    "MiamiFtLauderdale", "Midsouth", "Nashville", "NewOrleansMobile", "NewYork", "Northeast",
    "NorthernNewEngland", "Orlando", "Philadelphia", "PhoenixTucson", "Pittsburgh", "Plains",
    "Portland", "RaleighGreensboro", "RichmondNorfolk", "Roanoke", "Sacramento", "SanDiego",
    "SanFrancisco", "Seattle", "SouthCarolina", "SouthCentral", "Southeast", "Spokane",
    "StLouis", "Syracuse", "Tampa", "TotalUS", "West", "WestTexNewMexico"
  ),
  Region.Group = c(
    "Northeast", "South", "Northeast", "West", "Northeast", "Northeast",
    "West", "South", "Midwest", "Midwest", "Midwest", "South",
    "West", "Midwest", "Midwest", "Midwest", "Northeast", "Northeast",
    "South", "Midwest", "South", "West", "West", "Midwest",
    "South", "South", "South", "South", "Northeast", "Northeast",
    "Northeast", "South", "Northeast", "West", "Northeast", "Midwest",
    "West", "South", "South", "South", "West", "West",
    "West", "West", "South", "South", "South", "West",
    "Midwest", "Northeast", "South", "National", "West", "South"
  )
)

## Left join
avd_cleaned <- left_join(avd_cleaned,
                         region_groups,
                         by = "region")

## Check the results
glimpse(avd_cleaned)



## Convert `type`, `region`, and `Region.Group` to factor
avd_cleaned <- avd_cleaned |>
  
  ## Convert to factor
  mutate(across(where(is.character),
                as.factor))

## Check the results
glimpse(avd_cleaned)


## Drop `X`
avd_cleaned$X <- NULL

## Check the results
glimpse(avd_cleaned)

## Comments:
## - There are 18,249 records and 15 features.
## - No rows have been dropped.


## Check for missing values
anyNA(avd_cleaned)

## Comments: No missing values found.


# ----------------------------------------------------------------


## 3. EDA

## 3.1 Overview

## See summary stats
summary(avd_cleaned)


## Visualise the distribution of `AveragePrice`
avd_cleaned |>
  
  ## Aesthetic mapping
  ggplot(aes(x = AveragePrice)) +
  
  ## Instantiate a box plot
  geom_histogram(binwidth = 0.1) +
  
  ## Add text elements
  labs(title = "Distribution of Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal()

## Comments:
## - The distribution appears normally distributed.
## - This is in line with the `summary()` results which shows that median (1.360) and mean (1.391) are very close to one another.


## Create a correlation matrix
avd_cleaned |>
  
  ## Select numeric columns
  select(where(is.numeric)) |>
  
  ## Create a correlation matrix
  cor(use = "complete.obs") |>
  
  ## Visualise
  ggcorrplot(lab = TRUE,
             type = "lower",
             colors = c("blue", "white", "red"),
             lab_size = 2)

## Comments:
## - The correlations between `AveragePrice` and other variables range from 0.08 to 0.21.
## - `year` shows the weakest correlation at 0.08.
## - `X4046` shows the strongest correlation at -0.21.
## - The magnitude of the correlations suggest that price may stem from a combination of factors, rather than any single factor alone.
## - Additionally, many factors overlap greatly, for example, `Total.Volume` and other factor related to the number of sales such as `X4046`. 


## 3.2 Price vs time

## 3.2.1 Price vs year
avd_cleaned |>
  
  ## Group by year
  group_by(year) |>
  
  ## Compute mean price per year
  summarise(MeanPrice = mean(AveragePrice)) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Aesthetic mapping
  ggplot(aes(x = year,
             y = MeanPrice)) +
  
  ## Instantiate a line plot
  geom_line(color = "blue") +
  
  ## Add annotation
  geom_text(aes(label = paste(round(MeanPrice, 2), "USD")),
            nudge_y = 0.02,
            size = 3) +
  
  ## Add text elements
  labs(title = "Avocado Price Across the Years",
       x = "Year",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal()

## Comments
## - Avocado price was highest in 2017, at 1.52 USD, and lowest in 2016, at 1.34 USD.


## 3.2.2 Price vs month
avd_cleaned |>
  
  ## Group by month
  group_by(month) |>
  
  ## Compute mean price per month
  summarise(MeanPrice = mean(AveragePrice)) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Aesthetic mapping
  ggplot(aes(x = month,
             y = MeanPrice,
             fill = month)) +
  
  ## Instantiate a bar plot
  geom_col() +
  
  ## Add a trend line
  geom_line(aes(group = 1),
            color = "red",
            linewidth = 1) +
  
  ## Add annotation
  geom_text(aes(label = paste(round(MeanPrice, 2), "USD")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  
  ## Add text elements
  labs(title = "Avocado Price Per month",
       x = "Month",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal()

## Comments:
## - The lowest price is in February, at 1.27 USD.
## - The highest price is in October, at 1.58 USD.
## - Throughout a year, the average price rises steadily, dipping twice, in February and May, until reaching a peak in October and then drops steadily for the rest of the year.


## 3.3 Price vs region
avd_cleaned |>
  
  ## Group by region
  group_by(Region.Group) |>
  
  ## Compute mean price per region
  summarise(MeanPrice = mean(AveragePrice)) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Order by mean price
  mutate(Region.Group = fct_reorder(Region.Group,
                                    MeanPrice,
                                    .desc = TRUE)) |>
  
  ## Aesthetic mapping
  ggplot(aes(x = Region.Group,
             y = MeanPrice,
             fill = Region.Group)) +
  
  ## Instantiate a bar plot
  geom_col() +
  
  ## Add annotation
  geom_text(aes(label = paste(round(MeanPrice, 2), "USD")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  
  ## Add text elements
  labs(title = "Avocado Price Per Major Region",
       x = "Major Region",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal()

## Comments:
## - Northeast region has the highest price on average (1.57 USD).
## - The other regions have similar pricing levels (between 1.32 and 1.39 USD).


## 3.4 Price vs type
avd_cleaned |>
  
  ## Group by type
  group_by(type) |>
  
  ## Compute mean price per type
  summarise(MeanPrice = mean(AveragePrice)) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Aesthetic mapping
  ggplot(aes(x = type,
             y = MeanPrice,
             fill = type)) +
  
  ## Instantiate a line plot
  geom_col() +
  
  ## Add text elements
  labs(title = "Avocado Price by Pricing Strategy",
       x = "Pricing Strategy",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal()

## Comments:
## - On average, organic pricing has a highper price than conventional.
## - This is not surprising as organic pricing is generally associated with higher product pricing.


### 3.5 Price vs volume

## Explore the relationship between `Total.Volume` and `AveragePrice`
avd_cleaned |>
  
  ## Aesthetic mapping
  ggplot(aes(x = Total.Volume,
             y = AveragePrice)) +
  
  ## Instantiate a scatter plot
  geom_point(position = "jitter",
             alpha = 0.5,
             color = "orange") +
  
  ## Add a trend line
  geom_smooth(method = "loess",
              color = "blue",
              se = FALSE) +
  
  ## Add text elements
  labs(title = "Relationship between Total Volume and Average Price",
       x = "Total Volume",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ## Log-transform the x scale
  scale_x_log10()


## Conduct correlation test
cor.test(avd_cleaned$AveragePrice,
         avd_cleaned$Total.Volume,
         method = "pearson")

## Comments:
## - There is a weakly yet significant negative correlation between price and volume such that as volume increases, price decreases.
## - The magnitude of the correlation suggests that volume may not play a major role in avocado pricing.


## Explore potential moderators: Region
avd_cleaned |>
  
  ## Aesthetic mapping
  ggplot(aes(x = Total.Volume,
             y = AveragePrice)) +
  
  ## Instantiate a scatter plot
  geom_point(position = "jitter",
             alpha = 0.5,
             color = "orange") +
  
  ## Add a trend line
  geom_smooth(method = "loess",
              color = "blue",
              se = FALSE) +
  
  ## Add text elements
  labs(title = "Relationship between Total Volume and Average Price",
       x = "Total Volume",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ## Log-transform the x scale
  scale_x_log10() +
  
  ## Facet by Region.Group
  facet_wrap(~ Region.Group,
             scales = "free_y")

## Comments:
## - The relationship between pricing and volume is simiar across region groups, except for "National."
## - This suggests that region groups may not be a moderator of this relationship.


## Explore potential moderators: Pricing strategy
avd_cleaned |>
  
  ## Aesthetic mapping
  ggplot(aes(x = Total.Volume,
             y = AveragePrice)) +
  
  ## Instantiate a scatter plot
  geom_point(position = "jitter",
             alpha = 0.5,
             color = "orange") +
  
  ## Add a trend line
  geom_smooth(method = "loess",
              color = "blue",
              se = FALSE) +
  
  ## Add text elements
  labs(title = "Relationship between Total Volume and Average Price",
       x = "Total Volume",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ## Log-transform the x scale
  scale_x_log10() +
  
  ## Facet by type
  facet_wrap(~ type,
             scales = "free_y")

## Comments:
## - The relationship between pricing and volume is simiar across pricing strategy.
## - This suggests that pricing strategy may not be a moderator of this relationship.


## 3.6 Explore potential interaction effects

## 3.6.1 Pricing strategy vs region
avd_cleaned |>
  
  ## Group by type and major region
  group_by(type, Region.Group) |>
  
  ## Compute mean price per type
  summarise(MeanPrice = mean(AveragePrice),
            .groups = "drop") |>
  
  ## Aesthetic mapping
  ggplot(aes(x = type,
             y = MeanPrice,
             fill = type)) +
  
  ## Instantiate a line plot
  geom_col() +
  
  ## Add text elements
  labs(title = "Avocado Price by Pricing Strategy",
       x = "Pricing Strategy",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ## Facet by region
  facet_wrap(~ Region.Group)

## Comments:
## - Across all regions, organic pricing is consistently associated with higher price.
## - This suggests that there is no interaction effect between pricing strategy and region on pricing.


## 3.6.1 Pricing strategy vs month
avd_cleaned |>
  
  ## Group by type and major region
  group_by(type, month) |>
  
  ## Compute mean price per type
  summarise(MeanPrice = mean(AveragePrice),
            .groups = "drop") |>
  
  ## Aesthetic mapping
  ggplot(aes(x = type,
             y = MeanPrice,
             fill = type)) +
  
  ## Instantiate a line plot
  geom_col() +
  
  ## Add text elements
  labs(title = "Avocado Price by Pricing Strategy",
       x = "Pricing Strategy",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ## Facet by region
  facet_wrap(~ month)

## Comments:
## - Across all months, organic pricing is consistently associated with higher price.
## - This suggests that there is no interaction effect between pricing strategy and month on pricing.


# ----------------------------------------------------------------


# 4. Building a Predictive Model

## I will build two models, compare their performance, then choose the better performing one.
## The models are linear regression and random forest.
## Both are chosen due to their explainability.

## 4.1 Prepare the data

## Create splitting index
avd_split <- initial_split(avd_cleaned,
                           prop = 0.8,
                           strata = AveragePrice)

## Create a training set
avd_train <- training(avd_split)

## Create a test set
avd_test <- testing(avd_split)

## Check the results
cat("Training set:", nrow(avd_train), "\n")
cat("Test set:", nrow(avd_test))


## 4.2 Define a recipe
avd_recipe <- recipe(AveragePrice ~ .,
                     avd_train) |>
  
  ## Normalise all numeric variables
  step_normalize(all_numeric()) |>
  
  ## Handle multicollinearity
  step_corr(all_numeric_predictors(),
            threshold = 0.7) |>
  
  ## Dummy-encode nominal variables
  step_dummy(all_nominal()) |>
  
  ## Remove redundant variables
  step_rm("Date", "region")
