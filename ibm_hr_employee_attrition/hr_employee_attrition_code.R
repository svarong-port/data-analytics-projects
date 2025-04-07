# HR Employee Attrition Analytics

# Author: Shinin Varongchayakul
# Date: 07 Apr 2025

# Dataset Info
# Name: IBM HR Analytics Employee Attrition & Performance
# Source: https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset


# Install and load packages

## Install
install.packages("tidyverse") # data manipulation
install.packages("tidymodels") # machine learning
install.packages("effsize") # effect size calculation
install.packages("ggcorrplot") # correlation matrix

# Load
library(tidyverse)
library(tidymodels)
library(effsize)
library(ggcorrplot)


# ------------------------------------------------------


# Load the dataset

## Load
hr <- read.csv("hr_employee_attrition_dataset.csv")

## Preview
head(hr)

## View the structure
glimpse(hr)


# ------------------------------------------------------


# Explore and clean the data

## Convert categorical variables to factor

### Define categorical variables
cat_vars <- c("Attrition", "BusinessTravel","Department",
              "Education", "EducationField", "Gender",
              "JobRole", "MaritalStatus",
              "Over18", "OverTime")

### Convert to factor
hr_cleaned <- hr |>
  
  #### Mutate across
  mutate(across(all_of(cat_vars), as.factor))
  
### Check the results
glimpse(hr_cleaned)


## Handle missing values

### Check for missing values
anyNA(hr_cleaned)

### Comment: No missing values found


## Summarise the attrition rate
hr_cleaned |>
  
  ### Group and count by attrition
  count(Attrition, name = "Count") |>
  
  ### Compute percentage
  mutate(Percent = round(Count / sum(Count) * 100, 2))

#   Attrition Count Percent
# 1        No  1233   83.88
# 2       Yes   237   16.12

## Comment: There are quite fewer people who left than people who stayed


# ------------------------------------------------------


# EDA, part 1

## Get an overview of the relationships in the data
hr_cleaned |>
  
  ### Encode attrition to numeric
  mutate(AttritionEncoded = if_else(Attrition == "Yes",
                                    1,
                                    0)) |>
  
  ### Select only numeric variables
  select(where(is.numeric)) |>
  
  ### Create a correlation matrix
  cor(use = "complete.obs") |>
  
  #### Visualise the correlation matrix
  ggcorrplot(lab = TRUE,
             type = "lower",
             colors = c("blue", "white", "red"),
             lab_size = 2.5)

## Comments:
## - All correlations between attrition and other variables are low (min = 0, max = 0.17)
## - This suggests that attrition may be a result of a combination of several factors rather than any single factor alone


# ------------------------------------------------------


# EDA, part 2

# Explore 3 factors likely to affect attrition

# The 3 factors:
# 1. Monlthy income: employees may leave because of insufficient financial incentive, with lower income associated with higher attrition rate
# 2. Overtime: employees may leave because of workload, where those with more overtime more likely to leave
# 3. EnvironmentSatisfaction: those unsatisfied with their workplace settings may be more likely to leave

## 1. Monlthy income vs attrition
hr_cleaned |>
  
  ### Group by attrition
  group_by(Attrition) |>
  
  ### Summarise
  summarise(AVGMonthlyIncome = mean(MonthlyIncome)) |>
  
  ### Ungroup
  ungroup() |>
  
  ### Aesthetic mapping
  ggplot(aes(x = Attrition,
             y = AVGMonthlyIncome,
             fill = Attrition)) +
  
  ### Call bar plot
  geom_col() +
  
  ### Add text elements
  labs(title = "Monthly Income by Attrition",
       x = "Attrition Status",
       y = "Average Monthly Income",
       fill = "Attrition Status") +
  
  ### Adjust x scale
  scale_x_discrete() +
  
  ### Change theme to classic for easy viewing
  theme_classic()

## Comment:
## - On average, those who stayed tended to earn more than those who left
## - This strongly suggests that monthly income is a contributing factor to attrition
  

## Check the distribution of monthly income
hr_cleaned |>
  
  ### Aesthetic mapping
  ggplot(aes(x = MonthlyIncome,
             fill = Attrition)) +
  
  ### Call density plot
  geom_density(alpha = 0.5) +
  
  ### Add text elements
  labs(title = "Monthly Income Distribution",
       x = "Monthly Income",
       fill = "Attrition Status") +
  
  ### Change theme to classic for easy viewing
  theme_classic()

## Comments:
## - The distributions of both attrition groups are positivelyskewed
## - There were more proportionally more people who left in the lower end of the monthly income distribution
## - This adds further support to the earlier analysis that monthly income is a contributor of attrition


## 2. Overtime vs attrition
hr_cleaned |>
  
  ### Group by overtime and attrition
  group_by(OverTime, Attrition) |>
  
  ### Count the observations in each group
  summarise(Count = n(), .groups = "drop") |>
  
  ### Group by overtime
  group_by(OverTime) |>
  
  ### Compute percentage
  mutate(Percent = Count / sum(Count) * 100) |>
  
  ### Ungroup
  ungroup() |>
  
  ### Aesthetic mapping
  ggplot(aes(x = OverTime,
             y = Percent,
             fill = Attrition)) +
  
  ### Call count plot
  geom_col(position = "dodge") +
  
  ### Add text elements
  labs(title = "Overtime vs Attrition",
       x = "Overtime",
       y = "Percentage",
       fill = "Attrition Status") +
  
  ### Adjust x scale
  scale_x_discrete() +
  
  ### Change theme to classic for easy viewing
  theme_classic()

## Comments:
## - In both overtime conditions, there were more people who stayed than who left
## - The attrition rate was significantly higher in the overtime condition than in the no-overtime condition
## - Conversely, the percentage of those who stayed was higher in no-overtime condition than in the overtime condition
## - This suggests that overtime contributes attrition


## 3. Environment satisfaction vs attrition
hr_cleaned |>
  
  ### Group by attrition
  group_by(Attrition) |>
  
  ### Compute mean environment satisfaction
  summarise(AVGEnvSat = mean(EnvironmentSatisfaction)) |>
  
  ### Ungroup
  ungroup() |>

  ### Aesthetic mapping
  ggplot(aes(x = Attrition,
             y = AVGEnvSat,
             fill = Attrition)) +
  
  ### Call bar plot
  geom_col() +
  
  ### Add text elements
  labs(title = "Enviroment Satisfaction vs Attrition",
       x = "Attrition Status",
       y = "Average Environment Satisfaction",
       fill = "Attrition Status") +
  
  ### Adjust x scale
  scale_x_discrete() +
  
  ### Change theme to classic for easy viewing
  theme_classic()


### Conduct an independent t-test to test the difference
t.test(EnvironmentSatisfaction ~ Attrition,
       data = hr_cleaned)

### Results:
### t = 3.7513, df = 316.62, p-value = 0.0002092
### sample estimates:
### mean in group No mean in group Yes 
### 2.771290          2.464135 

## Comments:
## - There is a significant difference in environment satisfaction between the 2 attrition groups
## - Those who left tended to be less satisfied with the workplace settings than those who stayed
## - This suggests that environment satisfaction is likely a contributing factor to attrition


### Check the effect size
cohen.d(EnvironmentSatisfaction ~ Attrition,
        data = hr_cleaned)

### Cohen's d
### 
### d estimate: 0.2824158 (small)
### 95 percent confidence interval:
###   lower     upper 
### 0.1429148 0.4219167 

## Comments:
## - This suggests that the difference, while statistically significant, may not be as practically important as other factors such as monthly income and overtime
## - Thus, when tackling attrition, environment satisfaction may be given lower priority


# ------------------------------------------------------


# EDA, part 3

# Explore attritions patterns by department and job role

## Attrition by department
hr_cleaned |>
  
  ### Group by department and attrition
  group_by(Department, Attrition) |>
  
  ### Count the number of observations
  summarise(Count = n(), .groups = "drop") |>
  
  ### Group by department
  group_by(Department) |>
  
  ### Compute percentage
  mutate(Percent = Count / sum(Count) * 100) |>
  
  ### Ungroup
  ungroup() |>
  
  ### Aesthetic mapping
  ggplot(aes(x = Department,
             y = Percent,
             fill = Attrition)) +
  
  ### Call on bar plot
  geom_col(position = "dodge") +
  
  ## Add percent text
  geom_text(aes(label = paste(round(Percent, 0), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  
  ### Add text elements
  labs(title = "Attrition by Department",
       x = "Departments",
       y = "Attrition Percentage",
       fill = "Attrition Status") +
  
  ### Adjust x scale
  scale_x_discrete() +
  
  ### Adjust theme to classic for easy viewing
  theme_classic()

## Comments:
## - All departments have similar percentage of people leaving and staying
## Notably, however, R&D has lower percent of people leaving and slightly higher percentage of people staying, compared to the other departments
## Additionally, Sales has the highest percentage of people leaving and lowest percetage of people staying. This suggests that the attrition may have the most impact in Sales.


## Attrition by job role
hr_cleaned |>
  
  
  ### Group by department and attrition
  group_by(JobRole) |>
  
  ### Count the number of observations
  summarise(AttritionRate = mean(Attrition == "Yes") * 100) |>
  
  ### Ungroup
  ungroup() |>
  
  ### Reorder job role levels
  mutate(JobRole = fct_reorder(JobRole,
                               AttritionRate,
                               .desc = TRUE)) |>
  
  ### Aesthetic mapping
  ggplot(aes(x = JobRole,
             y = AttritionRate,
             fill = JobRole)) +
  
  ### Call on bar plot
  geom_col() +
  
  ## Add percent text
  geom_text(aes(label = paste(round(AttritionRate, 0), "%")),
            vjust = -0.5,
            size = 3) +
  
  ### Add text elements
  labs(title = "Attrition Rate by Job Role",
       x = "Job Roles",
       y = "Attrition Rate (%)",
       fill = "Job Roles") +

  ### Adjust x scale
  scale_x_discrete() +
  
  ### Adjust theme to classic for easy viewing
  theme_classic() +
  
  ### Adjust text elements
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

## Comments:
## - Sales Rep has the highest percentage of attrition
## - Research Director has the lowest percentage of attrition
## - Given the job role titles, employees in the lower job levels appear to leave more often
  
### Confirm whether job level is associated with attrition
hr_cleaned |>
  
  #### Convert job levels to factor
  mutate(JobLevel = as.factor(JobLevel)) |>
  
  #### Group by job levels
  group_by(JobLevel) |>
  
  #### Compute attrition rate
  summarise(AttritionRate = mean(Attrition == "Yes") * 100) |>
  
  #### Ungroup
  ungroup() |>
  
  ### Reorder job levels
  mutate(JobLevel = fct_reorder(JobLevel,
                               AttritionRate,
                               .desc = TRUE)) |>
  
  ### Aesthetic mapping
  ggplot(aes(x = JobLevel,
             y = AttritionRate,
             fill = JobLevel)) +
  
  ### Call on bar plot
  geom_col() +
  
  ## Add percent text
  geom_text(aes(label = paste(round(AttritionRate, 0), "%")),
            vjust = -0.5,
            size = 3) +
  
  ### Add text elements
  labs(title = "Attrition Rate by Job Level",
       x = "Job Levels",
       y = "Attrition Rate (%)",
       fill = "Job Levels") +
  
  ### Adjust x scale
  scale_x_discrete() +
  
  ### Adjust theme to classic for easy viewing
  theme_classic()

## Comments:
## - Job level 1 has the highest attrition percentage, while job levels 4 and 5 have the lowest attrition rate
## - This supports the earlier notion that people in lower job levels are more likely to leave the company


# ------------------------------------------------------


# EDA, part 4

## Explore attrition in relation to gender and age