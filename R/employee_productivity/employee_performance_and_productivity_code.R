# Exploring & Predicting Employee Productivity With Machine Learning

# Dataset
## Name: Employee Performance and Productivity Data
## Source: https://www.kaggle.com/datasets/mexwell/employee-performance-and-productivity-data
## Retrieved Date: 10 Apr 2025


# Install and load packages

## Install
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("data.table")
install.packages("dtplyr")

## Load
library(tidyverse)
library(tidymodels)
library(data.table)
library(dtplyr)


# -------------------------


# Load the dataset

## Load the data with fread()
prod <- fread("employee_performance_and_productivity_extended_dataset.csv")

## Preview the data
head(prod)

## View the structure
glimpse(prod)


# -------------------------


# Prepare the dataset

## Remove unnecessary variables
prod_cleaned <- prod[, -"Employee_ID"]

## Check the result
glimpse(prod_cleaned)


## Convert character columns to factor

### Select character columns
char_cols <- names(prod_cleaned)[sapply(prod_cleaned, is.character)]

### Convert to factor
prod_cleaned[, 
             (char_cols) := lapply(.SD, as.factor),
             .SDcols = char_cols]

## Check the result
glimpse(prod_cleaned)


## Convert `Resigned` to factor
prod_cleaned$Resigned <- factor(prod_cleaned$Resigned,
                                levels = c(FALSE, TRUE),
                                labels = c("No", "Yes"))

## Check the result
glimpse(prod_cleaned)


## Convert `Hire_Date` to Date
prod_cleaned[, Hire_Date := as.Date(Hire_Date)]

## Check the result
glimpse(prod_cleaned)


## Update `Years_At_Company`

### Store today's date
today_date <- as.Date("2025-04-10")

### Update
prod_cleaned[, Years_At_Company := as.integer(format(today_date, "%Y")) - year(Hire_Date)]

## Check the result
glimpse(prod_cleaned)


# -------------------------


# EDA, part I - explore sample characteristics

## Create a copy of the dataset for EDA
prod_eda <- copy(prod_cleaned)


## Compute the number of employees by department
prod_eda[,
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Department][order(-Percent)]


## Compute the number of employees by gender
prod_eda[,
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Gender][order(-Percent)]


## Compute the number of employees by education level

### Reorder the education level factors
prod_eda[, Education_Level := factor(Education_Level,
                                     levels = c("High School",
                                                "Bachelor",
                                                "Master",
                                                "PhD"))]

### Compute
prod_eda[, 
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Education_Level][order(Education_Level)]


## Compute the number of employees by team size
prod_eda[, 
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Team_Size][order(Team_Size)]
