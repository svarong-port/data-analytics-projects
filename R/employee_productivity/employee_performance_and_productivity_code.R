# Exploring & Predicting Employee Productivity With Machine Learning

# Dataset
## Name: Employee Performance and Productivity Data
## Source: https://www.kaggle.com/datasets/mexwell/employee-performance-and-productivity-data
## Retrieved Date: 10 Apr 2025


# Install and load packages

## Install
install.packages("tidyverse") # data manipulation
install.packages("tidymodels") # machine learning
install.packages("data.table") # data manipulation
install.packages("dtplyr") # data manipulation
install.packages("lubridate") # date manipulation

## Load
library(tidyverse)
library(tidymodels)
library(data.table)
library(dtplyr)
library(lubridate)


# -------------------------


# Load the dataset

## Store raw CSV URL from GitHub
raw_url <- "https://raw.githubusercontent.com/svarong-port/data-analytics-projects/refs/heads/main/R/employee_productivity/employee_performance_and_productivity_extended_dataset.csv"

## Load the data with fread()
prod <- fread(raw_url)

## Preview the data
head(prod)

## View the structure
glimpse(prod)


# -------------------------


# Prepare the dataset

## Remove unnecessary columns
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


## Convert `Hire_Date` to Date type
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


## Compute the number of employees by gender
prod_eda[,
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Gender][order(-Percent)]


## Compute the mean and SD of age
prod_eda[, .(Min = round(min(Age), 2),
             Mean = round(mean(Age), 2),
             Max = round(max(Age), 2),
             SD = round(sd(Age), 2))]

## Visualise age distribution
prod_eda |>
  
  ### Aesthetic mapping
  ggplot(aes(x = Age)) +
  
  ### Instantiate a histogram
  geom_histogram(binwidth = 10,
                 color = "blue",
                 fill = "steelblue") +
  
  ### Add text elements
  labs(title = "Age Distribution",
       x = "Age (Years)") +
  
  ### Apply classic theme
  theme_classic()


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




## Compute the number of employees by department
prod_eda[,
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Department][order(-Percent)]


# -------------------------


# EDA, part 2

# Compute the number of employees by department
prod_eda[,
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Department][order(-Percent)]


## Compute the number of employees by team size
prod_eda[, 
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Team_Size][order(Team_Size)]

## Compute the number of employees by job title
prod_eda[,
         .(Total = .N,
           Percent = .N / nrow(prod_eda) * 100),
         by = Job_Title][order(Job_Title)]

## Compute the number of employees by hire date

### Find hire year and hire month
prod_eda[, `:=` (Hire_Year = year(Hire_Date),
                 Hire_Month_Name = month(Hire_Date))]


### Compute the number of hires per year
hires_by_year <- prod_eda[,
                          .(Count = .N),
                          by = Hire_Year][order(Hire_Year)]

### Print the results
hires_by_year


### Plot the number of employees by hire year
hires_by_year |>
  
  #### Aesthetic mapping
  ggplot(aes(x = Hire_Year,
             y = Count)) +
  
  #### Instantiate a line plot
  geom_col(fill = "steelblue") +
  
  #### Add text elements
  labs(title = "The Number of Hires by Year",
       x = "Year",
       y = "Hires") +
  
  #### Adjust classic theme
  theme_classic() +
  
  ### Add x scale
  scale_x_continuous(breaks = 2014:2024)


### Compute the average number of hires by month
hires_by_month <- prod_eda[,
                           Hire_Month_Name := as.factor(Hire_Month_Name)][,
                                                                          .(Avg_Hires = .N / uniqueN(year(Hire_Date))),
                                                                          by = Hire_Month_Name][order(Hire_Month_Name)]

### Print the results
hires_by_month

### Plot the number of employees by month
hires_by_month |>

  #### Aesthetic mapping
  ggplot(aes(x = Hire_Month_Name,
             y = Avg_Hires)) +
  
  #### Instantiate a line plot
  geom_col(fill = "steelblue") +
  
  #### Add text elements
  labs(title = "Average Number of Employees Hired by Month",
       x = "Month",
       y = "Average Hires") +
  
  #### Adjust classic theme
  theme_classic()