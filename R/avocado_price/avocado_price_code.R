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


## 2. Load and prepare the dataset

## 2.1 Load
avocados <- read_csv("avocado_price_dataset.csv")

## Preview the first 10 row
head(avocados, 10)

## View the structure
glimpse(avocados)

## Comments:
## - There are 18,249 records and 14 features.


## 2.2 Prepare the dataset

## Create a copy of the dataset
avd_cleaned <- avocados


## 2.2.1 Drop index column

## Drop
avd_cleaned$...1 <- NULL

## Check the results
glimpse(avd_cleaned)


## 2.2.2 Rename the columns

### Rename
avd_cleaned <- avd_cleaned |>
  
  ## Rename
  rename(date = Date,
         avg_price = AveragePrice,
         vol_total = `Total Volume`,
         vol_4046 = `4046`,
         vol_4225 = `4225`,
         vol_4770 = `4770`,
         bags_total = `Total Bags`,
         bags_small = `Small Bags`,
         bags_large = `Large Bags`,
         bags_xl = `XLarge Bags`,
         pricing_strategy = type,
         year = year,
         region = region)

## Check the results
glimpse(avd_cleaned)


## 2.2.3 Convert `date` to Date

## Convert 
avd_cleaned$date <- as.Date(avd_cleaned$date)

## Check the results
glimpse(avd_cleaned)


## Extract month from `date`

## Extract
avd_cleaned$month <- format(avd_cleaned$date,
                            "%B")

## Convert `month` to factor
avd_cleaned$month <- factor(avd_cleaned$month,
                            levels = month.name,
                            ordered = TRUE)

## Check the results
glimpse(avd_cleaned)


## Group `region` into more meaningful regions

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
  region_group = c(
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


## 2.2.6 Convert character columns to factor

## Convert
avd_cleaned <- avd_cleaned |>
  
  ## Convert to factor
  mutate(across(where(is.character),
                as.factor))

## Check the results
glimpse(avd_cleaned)


## 2.2.7 Re-arrange the columns

## Re-arrange
avd_cleaned <- avd_cleaned |>
  
  ## Select the columns
  select(date, year, month,
         region_group, region,
         vol_total, vol_4046, vol_4225, vol_4770,
         bags_total, bags_small, bags_large, bags_xl,
         pricing_strategy, avg_price)

## Check the results
glimpse(avd_cleaned)


## 2.2.8 Check for missing values
anyNA(avd_cleaned)

## Comments:
## - No missing values found.



# ----------------------------------------------------------------


## 3. EDA

## 3.1 Overview

## 3.1.1 See summary stats
summary(avd_cleaned)


## 3.1.2 Create a correlation matrix
avd_cleaned |>
  
  ## Select numeric columns
  select(where(is.numeric)) |>
  
  ## Create a correlation matrix
  cor(use = "complete.obs") |>
  
  ## Visualise
  ggcorrplot(lab = TRUE,
             type = "lower",
             colors = c("skyblue", "white", "darkgreen"),
             lab_size = 2)

## Comments:
## - The correlations between `avg_price` and other variables range from 0.08 to 0.21.
## - `year` shows the weakest correlation at 0.09.
## - `vol_4046` shows the strongest correlation at -0.21.
## - The magnitude of the correlations suggest that price may stem from a combination of factors, rather than any single factor alone.
## - Additionally, many factors overlap greatly, for example, `vol_total` and other factor related to the number of sales such as `vol_4046`.


## 3.1.3 Visualise the distribution of price
avd_cleaned |>
  
  ## Aesthetic mapping
  ggplot(aes(x = avg_price)) +
  
  ## Instantiate a box plot
  geom_histogram(binwidth = 0.1,
                 fill = "#7ea122") +
  
  ## Add text elements
  labs(title = "Distribution of Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal()

## Comments:
## - The distribution appears normally distributed.
## - This is in line with the `summary()` results which shows that median (1.370) and mean (1.406) are very close to one another.


## 3.2 Price vs time

## 3.2.1 Price vs year
avd_cleaned |>
  
  ## Group by year
  group_by(year) |>
  
  ## Compute mean price per year
  summarise(mean_price = mean(avg_price)) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Aesthetic mapping
  ggplot(aes(x = year,
             y = mean_price)) +
  
  ## Instantiate a line plot
  geom_line(color = "#7ea122") +
  
  ## Add annotation
  geom_text(aes(label = paste(round(mean_price, 2), "USD")),
            nudge_y = 0.02,
            size = 3) +
  
  ## Add text elements
  labs(title = "Distribution of Average Price",
       x = "Average Price (USD)",
       y = "Frequency") +
  
  ## Adjust theme to minimal
  theme_minimal()

## Comments:
## - Avocado price was highest in 2017, at 1.52 USD, and lowest in 2016, at 1.34 USD.


## 3.2.2 Price vs month
avd_cleaned |>
  
  ## Group by month
  group_by(month) |>
  
  ## Compute mean price per month
  summarise(mean_price = mean(avg_price)) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Aesthetic mapping
  ggplot(aes(x = month,
             y = mean_price,
             fill = month)) +
  
  ## Instantiate a bar plot
  geom_col() +
  
  ## Add a trend line
  geom_line(aes(group = 1),
            color = "red",
            linewidth = 1) +
  
  ## Add annotation
  geom_text(aes(label = paste(round(mean_price, 2), "USD")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  
  ## Add text elements
  labs(title = "Avocado Price Per month",
       x = "Month",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ## Adjust x ticks
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

## Comments:
## - The lowest price is in February, at 1.27 USD.
## - The highest price is in October, at 1.58 USD.
## - Throughout a year, the average price rises steadily, dipping twice, in February and May, until reaching a peak in October and then drops steadily for the rest of the year.


## 3.3 Price vs region group
avd_cleaned |>
  
  ## Group by region group
  group_by(region_group) |>
  
  ## Compute mean price per region group
  summarise(mean_price = mean(avg_price)) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Order by mean price
  mutate(region_group = fct_reorder(region_group,
                                    mean_price,
                                    .desc = TRUE)) |>
  
  ## Aesthetic mapping
  ggplot(aes(x = region_group,
             y = mean_price,
             fill = region_group)) +
  
  ## Instantiate a bar plot
  geom_col() +
  
  ## Add annotation
  geom_text(aes(label = paste(round(mean_price, 2), "USD")),
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


## 3.4 Price vs pricing strategy
avd_cleaned |>
  
  ## Group by pricing strategy
  group_by(pricing_strategy) |>
  
  ## Compute mean price per pricing strategy
  summarise(mean_price = mean(avg_price)) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Aesthetic mapping
  ggplot(aes(x = pricing_strategy,
             y = mean_price,
             fill = pricing_strategy)) +
  
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

## 3.5.1 Explore the relationship between price and total volume
avd_cleaned |>
  
  ## Aesthetic mapping
  ggplot(aes(x = vol_total,
             y = avg_price)) +
  
  ## Instantiate a scatter plot
  geom_point(position = "jitter",
             alpha = 0.5,
             color = "#7ea122") +
  
  ## Add a trend line
  geom_smooth(method = "loess",
              color = "red",
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
cor.test(avd_cleaned$avg_price,
         avd_cleaned$vol_total,
         method = "pearson")

## Comments:
## - There is a weak yet significant negative correlation between price and volume such that as volume increases, price decreases.
## - The magnitude of the correlation suggests that volume may not play a major role in avocado pricing.


## 3.5.2 Explore potential moderators: Region Group
avd_cleaned |>
  
  ## Aesthetic mapping
  ggplot(aes(x = vol_total,
             y = avg_price)) +
  
  ## Instantiate a scatter plot
  geom_point(position = "jitter",
             alpha = 0.5,
             color = "#7ea122") +
  
  ## Add a trend line
  geom_smooth(method = "loess",
              color = "red",
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
  facet_wrap(~ region_group,
             scales = "free_y")

## Comments:
## - The relationship between pricing and volume is simiar across region groups, except for "National."
## - This suggests that region groups may not be a moderator of this relationship.


## 3.5.3 Explore potential moderators: Pricing strategy
avd_cleaned |>
  
  ## Aesthetic mapping
  ggplot(aes(x = vol_total,
             y = avg_price)) +
  
  ## Instantiate a scatter plot
  geom_point(position = "jitter",
             alpha = 0.5,
             color = "#7ea122") +
  
  ## Add a trend line
  geom_smooth(method = "loess",
              color = "red",
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
  facet_wrap(~ pricing_strategy,
             scales = "free_y")

## Comments:
## - The relationship between pricing and volume is simiar across pricing strategy.
## - This suggests that pricing strategy may not be a moderator of this relationship.


## 3.6 Explore potential interaction effects

## 3.6.1 Pricing strategy vs region
avd_cleaned |>
  
  ## Group by type and major region
  group_by(pricing_strategy,
           region_group) |>
  
  ## Compute mean price per type
  summarise(mean_price = mean(avg_price),
            .groups = "drop") |>
  
  ## Aesthetic mapping
  ggplot(aes(x = pricing_strategy,
             y = mean_price,
             fill = pricing_strategy)) +
  
  ## Instantiate a line plot
  geom_col() +
  
  ## Add text elements
  labs(title = "Avocado Price by Pricing Strategy",
       x = "Pricing Strategy",
       y = "Average Price") +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ## Facet by region
  facet_wrap(~ region_group)

## Comments:
## - Across all regions, organic pricing is consistently associated with higher price.
## - This suggests that there is no interaction effect between pricing strategy and region on pricing.


## 3.6.2 Pricing strategy vs month
avd_cleaned |>
  
  ## Group by type and major region
  group_by(pricing_strategy,
           month) |>
  
  ## Compute mean price per type
  summarise(mean_price = mean(avg_price),
            .groups = "drop") |>
  
  ## Aesthetic mapping
  ggplot(aes(x = pricing_strategy,
             y = mean_price,
             fill = pricing_strategy)) +
  
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

## I will build an exXtreme Gradient Boosting (XGBoost) model due to its high performance and explanability.

## 4.1 Initial model

## 4.1.1 Split the data

## Set seed for reproducibility
set.seed(888)

## Create splitting index
avd_split <- initial_split(avd_cleaned,
                           prop = 0.8,
                           strata = avg_price)

## Create a training set
avd_train <- training(avd_split)

## Create a test set
avd_test <- testing(avd_split)

## Check the results
cat("Training set:", nrow(avd_train), "\n")
cat("Test set:", nrow(avd_test))


## 4.1.2 Define the recipe
avd_recipe <- recipe(avg_price ~ .,
                     avd_train) |>
  
  ## Remove redundant variables
  step_rm(date, year, region,
          vol_4046, vol_4225, vol_4770,
          bags_small, bags_large, bags_xl) |>
  
  ## Normalise all numeric variables
  step_normalize(all_numeric()) |>
  
  ## Dummy-encode nominal variables
  step_dummy(all_nominal())

## Check the recipe
avd_recipe


## 4.1.3 Instantiate an XGBoost model
xgb_mod <- boost_tree() |>
  
  ## Set engine
  set_engine("xgboost") |>
  
  ## Set mode
  set_mode("regression")


## 4.1.4 Bundle the recipe and the model
xgb_wfl <- workflow() |>
  
  ## Add recipe
  add_recipe(avd_recipe) |>
  
  ## Add model
  add_model(xgb_mod)


## 4.1.5 Fit the model

## Set the metrics
xgb_metrics <-metric_set(mae,
                         rmse,
                         rsq)

## Set seed for reproducibility
set.seed(888)

## Fit
xgb_fit <- last_fit(xgb_wfl,
                    split = avd_split,
                    metrics = xgb_metrics)


## 4.1.6 Evaluate the model

## Collect metrics
xgb_performance <- collect_metrics(xgb_fit)

## Print the results
xgb_performance

# # A tibble: 3 × 4
#   .metric .estimator .estimate .config             
#   <chr>   <chr>          <dbl> <chr>               
# 1 mae     standard       0.461 Preprocessor1_Model1
# 2 rmse    standard       0.606 Preprocessor1_Model1
# 3 rsq     standard       0.635 Preprocessor1_Model1

## Comments
## - The initial model demonstrated moderate performance, with MAE of 0.46 and RMSE of 0.61.
## - The model was able to account for about 64% of the avocado price variance.


## 4.2 Tune the model

## 4.2.1 Define the hyperparametres for tuning
xgb_mod_tune <- boost_tree(trees = tune(),
                           tree_depth = tune(),
                           mtry = tune(),
                           learn_rate = tune(),
                           sample_size = tune()) |>
  
  ## Set engine
  set_engine("xgboost") |>
  
  ## Set mode
  set_mode("regression")


## 4.2.2 Bundle the recipe and the model
xgb_wfl_tune <- workflow() |>
  
  ## Add recipe
  add_recipe(avd_recipe) |>
  
  ## Add model
  add_model(xgb_mod_tune)


## 4.2.3 Define resampling method

## Set seed for reproducibility
set.seed(888)

## Set k-fold cross-validation
xgb_cv <- vfold_cv(avd_train,
                   v = 10,
                   strata = avg_price)


## 4.2.4 Create a hyperparametre grid

## Define hyperparametres ranges
xgb_param_grid <- parameters(trees(range = c(100, 1000)),
                             tree_depth(range = c(2, 10)),
                             mtry(range = c(2, ncol(avd_train) - 1)),
                             learn_rate(range = c(0.01, 0.3)),
                             sample_prop(range = c(0.5, 1.0)))

## Set seed for reproducibility
set.seed(888)

## Create a random grid
xgb_hp_grid <- grid_random(xgb_param_grid,
                           size = 30)


## 4.2.5 Tune the model

## Set seed for reproducibility
set.seed(888)

## Tune
system.time({
  xgb_tune_results <- tune_grid(xgb_wfl_tune,
                                resamples = xgb_cv,
                                grid = xgb_hp_grid,
                                metrics = xgb_metrics,
                                verbose = 1)
})


## 4.2.6 Show best hyperparametres
show_best(xgb_tune_results)