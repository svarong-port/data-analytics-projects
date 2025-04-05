# HR Employee Attrition Analysis

# Author: Shinin Varongchayakul
# Date: 05 Apr 2025

# Dataset Info
# Name: IBM HR Analytics Employee Attrition & Performance
# Source: https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset


# --------------------------------------


# Busines Questions

# Q1. Attrition Risk by Department & Role
# - We’ve been noticing an increase in employee turnover.
# - Which departments and job roles have the highest attrition rates?

# Q2. Work-Life Balance & Overtime
# - Employees have expressed concerns about work-life balance.
# - How does overtime impact attrition?
# - Are employees who work overtime more likely to leave?

# Q3. Salary vs. Attrition: The Pay Gap Dilemma
# - Do employees who earn less tend to leave more frequently?
# - What’s the average monthly income of those who stay vs. those who leave?
# - Are we paying our high-performing employees enough to retain them?

# Q4. Age & Experience: Who is Most at Risk?
# - Are younger employees leaving at a higher rate than older employees?
# - How does total working experience influence attrition?

# Q5. Promotion & Career Growth Opportunities
# - We want to ensure that employees see long-term career growth in our company.
# - How does the number of promotions (YearsSinceLastPromotion) relate to attrition?
# - Are employees with fewer promotions more likely to leave?

# Q6. Job Satisfaction vs. Attrition
# - How does job satisfaction affect attrition rates?
# - Are employees with lower satisfaction scores leaving more often?

# Q7. Remote Work vs. Travel Frequency
# - With more employees requesting remote work, does business travel influence attrition?
# - Are those who travel frequently more likely to leave?

# Q8. High Performers & Attrition
# - Are we losing our top-performing employees?
# - How does Performance Rating relate to attrition?


# --------------------------------------


# Install and Load Packages

## Install
install.packages("dplyr") # data manipulation
install.packages("forcats") # data manipulation
install.packages("ggplot2") # data visualisation
install.packages("tidymodels") # predictive modeling

## Load
library(dplyr)
library(ggplot2)
library(tidymodels)
library(forcats)


# --------------------------------------


# Load the Dataset

hr <- read.csv("hr_employee_attrition_dataset.csv")


# HR Employee Attrition Analysis

# Author: Shinin Varongchayakul
# Date: 05 Apr 2025

# Dataset Info
# Name: IBM HR Analytics Employee Attrition & Performance
# Source: https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset


# --------------------------------------


# Busines Questions

# Q1. Attrition Risk by Department & Role
# - We’ve been noticing an increase in employee turnover.
# - Which departments and job roles have the highest attrition rates?

# Q2. Work-Life Balance & Overtime
# - Employees have expressed concerns about work-life balance.
# - How does overtime impact attrition?
# - Are employees who work overtime more likely to leave?

# Q3. Salary vs. Attrition: The Pay Gap Dilemma
# - Do employees who earn less tend to leave more frequently?
# - What’s the average monthly income of those who stay vs. those who leave?
# - Are we paying our high-performing employees enough to retain them?

# Q4. Age & Experience: Who is Most at Risk?
# - Are younger employees leaving at a higher rate than older employees?
# - How does total working experience influence attrition?

# Q5. Promotion & Career Growth Opportunities
# - We want to ensure that employees see long-term career growth in our company.
# - How does the number of promotions (YearsSinceLastPromotion) relate to attrition?
# - Are employees with fewer promotions more likely to leave?

# Q6. Job Satisfaction vs. Attrition
# - How does job satisfaction affect attrition rates?
# - Are employees with lower satisfaction scores leaving more often?

# Q7. Remote Work vs. Travel Frequency
# - With more employees requesting remote work, does business travel influence attrition?
# - Are those who travel frequently more likely to leave?

# Q8. High Performers & Attrition
# - Are we losing our top-performing employees?
# - How does Performance Rating relate to attrition?


# --------------------------------------


# Install and Load Packages

## Install
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidymodels")

## Load
library(dplyr)
library(ggplot2)
library(tidymodels)


# --------------------------------------


# Load the Dataset

## Import the dataset
hr <- read.csv("hr_employee_attrition_dataset.csv")

## Preview
head(hr)

## View the structure
glimpse(hr)


# --------------------------------------


# Clean the Data

## Convert colunms to factor variables

### Define variables to convert
factor_cols <- c("Attrition", "BusinessTravel","Department",
                 "Education", "EducationField", "Gender",
                 "JobLevel", "JobRole", "MaritalStatus",
                 "Over18", "OverTime", "StockOptionLevel")

### Convert to factor
hr_cleaned <- hr |>
  mutate(across(all_of(factor_cols), as.factor))

### Check the results
glimpse(hr_cleaned)


## Check for missing data
any(is.na(hr_cleaned))


# --------------------------------------


# Business Question 1

# Q1. Attrition Risk by Department & Role
# - We’ve been noticing an increase in employee turnover.
# - Which departments and job roles have the highest attrition rates?


## Calculate attribution risk by department
attrition_risk_by_dep <- hr_cleaned |>
  
  ### Group by department
  group_by(Department) |>
  
  ### Compute attrition risk
  summarise(AttritionRisk = mean(Attrition == "Yes") * 100) |>
  
  ### Ungroup
  ungroup() |>
  
  ## Arrange by attrition risk
  arrange(desc(AttritionRisk))


## Print the results
attrition_risk_by_dep


## Visualise the results
attrition_risk_by_dep |>
  
  ### Reorder departments by attrition risk
  mutate(Department = fct_reorder(Department,
                                  AttritionRisk,
                                  .desc = TRUE)) |>
  
  ### Aesthetic mapping
  ggplot(aes(x = Department,
             y = AttritionRisk,
             fill = Department)) +
  
  ### Call bar plot
  geom_col() +
  
  ### Adjust theme to light
  theme_minimal() +
  
  ### Add text elements
  labs(title = "Attrition Ratio by Department",
       x = "Departments",
       y = "Attrition Ratio (%)",
       fill = "Departments") +
  
  ### Adjust x scale
  scale_x_discrete(labels = c("Human Resources" = "HR",
                              "Research & Development" = "R&D",
                              "Sales" = "Sales"))



## Calculate attribution risk by job roles
attrition_risk_by_job <- hr_cleaned |>
  
  ### Group by job role
  group_by(JobRole) |>
  
  ### Compute attrition risk
  summarise(AttritionRisk = mean(Attrition == "Yes") * 100) |>
  
  ### Ungroup
  ungroup() |>
  
  ## Arrange by attrition risk
  arrange(desc(AttritionRisk))


## Print the results
attrition_risk_by_job


## Visualise the results
attrition_risk_by_job |>
  
  ### Reorder departments by attrition risk
  mutate(JobRole = fct_reorder(JobRole,
                                  AttritionRisk,
                                  .desc = TRUE)) |>
  
  ### Aesthetic mapping
  ggplot(aes(x = JobRole,
             y = AttritionRisk,
             fill = JobRole)) +
  
  ### Call bar plot
  geom_col() +
  
  ### Adjust theme to light
  theme_minimal() +
  
  ### Add text elements
  labs(title = "Attrition Ratio by Job Role",
       x = "Job Roles",
       y = "Attrition Ratio (%)",
       fill = "Job Role") +
  
  ### Customise text elements
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  
  ### Adjust x scale
  scale_x_discrete()





















