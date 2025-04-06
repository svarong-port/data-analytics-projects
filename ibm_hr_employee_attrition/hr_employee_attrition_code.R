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
  
  ## Arrange by attrition risk, descending
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
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ### Add text elements
  labs(title = "Attrition Risk by Department",
       x = "Departments",
       y = "Attrition Risk (%)",
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
  
  ## Arrange by attrition risk, descending
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
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ### Add text elements
  labs(title = "Attrition Risk by Job Role",
       x = "Job Roles",
       y = "Attrition Risk (%)",
       fill = "Job Role") +
  
  ### Customise text elements
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  
  ### Adjust x scale
  scale_x_discrete()


# --------------------------------------


# Q2. Work-Life Balance & Overtime
# - Employees have expressed concerns about work-life balance.
# - How does overtime impact attrition?
# - Are employees who work overtime more likely to leave?


## Create a matrix for overtime vs attrition
table(overtime = hr_cleaned$OverTime,
      attrition = hr_cleaned$Attrition)


## Count attrition by overtime
attrition_by_overtime <- hr_cleaned |>
  
  ## Group by overtime
  group_by(OverTime) |>
  
  ## Count attrition
  summarise(Count = sum(Attrition == "Yes"),
            Total = n(),
            Risk = mean(Attrition == "Yes") * 100) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Arrange by attrition count
  arrange(desc(Risk))


## Print the results
attrition_by_overtime


## Visualise the results
attrition_by_overtime |>
  
  ## Aesthetic mapping
  ggplot(aes(x = OverTime,
             y = Risk,
             fill = OverTime)) +
  
  ### Call on bar plot
  geom_col() +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ## Add title, labels, legend
  labs(title = "Attrition Risk by Overtime Type",
       x = "Overtime Type",
       y = "Attrition Risk (%)",
       fill = "Overtime Type") +
  
  ## Adjust x scale
  scale_x_discrete()


# --------------------------------------


# Q3. Salary vs. Attrition: The Pay Gap Dilemma
# - Do employees who earn less tend to leave more frequently?
# - What’s the average monthly income of those who stay vs. those who leave?
# - Are we paying our high-performing employees enough to retain them?


## Perform an independent t-test
## to see if monthly income differs by attrition
t.test(MonthlyIncome ~ Attrition,
       data = hr_cleaned)


## Perform an independent two-sample t-test
## to see if, among high-performers, those who leave
## earn less or more monthly income

### Filter for high-perfomers
high_performers <- hr_cleaned |>
  
  #### Filter for PerformanceRating >= 4
  filter(PerformanceRating >= 4)

### Filter for those who left
attrition_yes <- hr_cleaned |>
  
  #### Filter for Attrition == "Yes"
  filter(Attrition == "Yes")

### Perform the t-test
t.test(high_performers$MonthlyIncome,
       attrition_yes$MonthlyIncome)


# --------------------------------------


# Q4. Age & Experience: Who is Most at Risk?
# - Are younger employees leaving at a higher rate than older employees?
# - How does total working experience influence attrition?

## Compute attrition risk by age group
attrition_by_age <- hr_cleaned |>
  
  ## Create age groups
  mutate(AgeGroup = if_else(Age > quantile(Age, 0.5),
                            "Older",
                            "Younger")) |>
  
  ## Group by age group
  group_by(AgeGroup) |>
  
  ## Compute attrition risk
  summarise(AttritionRisk = mean(Attrition == "Yes") * 100) |>
  
  ## Ungroup
  ungroup() |>
  
  ## Arrange by attrition risk, descending
  arrange(desc(AttritionRisk))


## Print the results
attrition_by_age


## Visualise the results
attrition_by_age |>
  
  ## Reorder age group
  mutate(AgeGroup = factor(AgeGroup,
                           levels = c("Younger",
                                      "Older"))) |>
  
  ### Aesthetic mapping
  ggplot(aes(x = AgeGroup, 
             y = AttritionRisk,
             fill = AgeGroup)) +
  
  ### Call on bar plot
  geom_col() +

  ### Adjust theme to minimal
  theme_minimal() +
  
  ### Add title, labels, legend
  labs(title = "Attrition Risk by Age Group",
       x = "Age Group",
       y = "Attrition Risk (%)",
       fill = "Age Group") +
  
  ### Adjust x scale
  scale_x_discrete()



# --------------------------------------


# Q5. Promotion & Career Growth Opportunities
# - We want to ensure that employees see long-term career growth in our company.
# - How does the number of promotions (YearsSinceLastPromotion) relate to attrition?
# - Are employees with fewer promotions more likely to leave?

## Compute the years since last promotion by attrition
promotion_by_attrition <- hr_cleaned |> 
  
  ### Group by attrition
  group_by(Attrition) |>
  
  ### Compute the average years since last promotion
  summarise(Promotion = mean(YearsSinceLastPromotion)) |>
  
  ### Ungroup
  ungroup() |>
  
  ### Arrange by the years since last promotion, descending
  arrange(desc(Promotion))


## Print the results
promotion_by_attrition


## Visualise the results
promotion_by_attrition |>
  
  ### Aesthetic mapping
  ggplot(aes(x = Attrition,
             y = Promotion,
             fill = Attrition)) +
  
  ### Call bar plot
  geom_col() +
  
  ## Adjust theme to minimal
  theme_minimal() +
  
  ### Add text elements
  labs(title = "Years Since Last Promotion by Attrition",
       x = "Attrition",
       y = "Years",
       fill = "Attrition") +
  
  ### Adjust x scale
  scale_x_discrete()


## Compute an indepedent t-test
t.test(YearsSinceLastPromotion ~ Attrition,
       data = hr_cleaned)