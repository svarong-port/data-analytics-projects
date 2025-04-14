# Predicting Avocado Price With Machine Learning in R

## Prepared by
## Author: Shinin Varongchayakul
## Date: 15 Apr 2025
## Langauge: R

## Dataset
## Name: Avocado Price
## Source: https://www.kaggle.com/datasets/neuromusic/avocado-prices?hl=en-GB
## Retrieved Date: 15 Apr 2025


# ----------------------------------------------------------------


## Business Problems
## A grocery retail chain wants to optimise their avocado pricing as avacado is one of the ir best-selling items.
## They want to understand what factors impact avocado pricing and how the price changes over time and across regions.
## They also want to be able to predict avocado price.
## Avocado Price dataset from Kaggle will be used to simulate the situation here.


## Analysis Objectives
## Given the business problems, the goals of this analysis are twofold:
## 1. Explore the pattern of avocado pricing across different factors
## 2. Build a predictive model to predict avocado pricing


# ----------------------------------------------------------------


## 1. Install and load necessary packages

## Install
install.packages("tidyverse") # data manipulation
install.packages("tidymodels") # machine learning
install.packages("lubridate") # date manipulation

## Load
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


# ----------------------------------------------------------------


## 3. Prepare the dataset

## Create a copy of the dataset
avd_cleaned <- avocados


## Convert `Date` to Date
avd_cleaned$Date <- as.Date(avd_cleaned$Date)

## Check the results
glimpse(avd_cleaned)


## Convert `type` and `region` to factor
avd_cleaned <- avd_cleaned |>
  
  ### Convert to factor
  mutate(across(where(is.character),
                as.factor))

## Check the results
glimpse(avd_cleaned)


## Drop `X`
avd_cleaned$X <- NULL

## Check the results
glimpse(avd_cleaned)


## Check for missing values
anyNA(avd_cleaned)

## Comments: No missing values found.


# ----------------------------------------------------------------


## 3. EDA