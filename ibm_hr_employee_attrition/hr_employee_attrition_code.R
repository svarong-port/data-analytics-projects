# HR Employee Attrition Analytics

# Author: Shinin Varongchayakul
# Date: 07 Apr 2025

# Dataset Info
# Name: IBM HR Analytics Employee Attrition & Performance
# Source: https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset


# Install and load packages

## Install
install.packages("tidyverse") # data manipulation
install.packages("effsize") # effect size calculation
install.packages("ggcorrplot") # correlation matrix
install.packages("tidymodels") # machine learning
install.packages("themis") # oversampling and undersampling of outcomes
install.packages("vip") # level of feature importance
install.packages("pdp") # plotting partial dependencies

# Load
library(tidyverse)
library(effsize)
library(ggcorrplot)
library(tidymodels)
library(themis)
library(vip)
library(pdp)


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
              "JobLevel", "JobRole", "MaritalStatus",
              "Over18", "OverTime", "StockOptionLevel")

### Convert to factor
hr_cleaned <- hr |>
  
  #### Mutate across
  mutate(across(all_of(cat_vars), as.factor))

### Set attrition factor levels
hr_cleaned$Attrition <- factor(hr_cleaned$Attrition,
                               levels = c("Yes", "No"))
  
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
  labs(title = "Environment Satisfaction vs Attrition",
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

# Explore attrition patterns by department and job role

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

# Explore the attrition pattens by gender and age

## Gender vs attrition

hr_cleaned |>
  
  ### Group by gender and attritiom
  group_by(Gender, Attrition) |>
  
  ### Count the number of observations
  summarise(Count = n(), .groups = "drop") |>
  
  ### Group by gender
  mutate(Percent = Count / sum(Count) * 100) |>
  
  ### Ungroup
  ungroup() |>
  
  ### Aesthetic mapping
  ggplot(aes(x = Attrition,
             y = Percent,
             fill = Gender)) +
  
  ### Call on bar plot
  geom_col(position = "dodge") +
  
  ## Add percent text
  geom_text(aes(label = paste(round(Percent, 0), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  
  ### Add text elements
  labs(title = "Attrition by Gender",
       x = "Attrition",
       y = "Attrition Percentage",
       fill = "Gender") +
  
  ### Adjust x scale
  scale_x_discrete() +
  
  ### Adjust theme to classic for easy viewing
  theme_classic()

## Comments:
## - In both attrition conditions, there were more males than females
## - Among both gender, there were more people leaving than staying
## - As the attrition patterns are similar across gender, this variable may contribute very weakly to attrition


## Age vs attrition

hr_cleaned |>
  
  ### Aesthetic mapping
  ggplot(aes(x = Age,
             fill = Attrition)) +
  
  ### Call on density plot
  geom_density(alpha = 0.5) +
  
  ### Add text elements
  labs(title = "Attrition by Age",
       x = "Age (Years)",
       y = "Employee Count",
       fill = "Attrition Status") +
  
  ### Adjust theme to classic for easy viewing
  theme_classic()

## Comments:
## - Age distributions are similar across attrition conditions
## - Age distribution for those who stayed is more normally distributed
## - The peaks are close together
## - As the peak for those who left leans more towards the left, people who left to be younger
## - This suggests that age may be a worthwhile predictor of attrition


# ------------------------------------------------------


# Predictive Modelling

# Questions:
# - Can we predict attrition?
# - If so, with how much accuracy?
# - What are the five most important predictors in the model?

## Prepare the dataset for modelling

### Remove non-predictive variables
hr_modelling <- hr_cleaned |>
  
  #### Deselect
  select(-EmployeeCount, -EmployeeNumber, -Over18)

### Check the results
glimpse(hr_modelling)


## Split the data

### Set seed for reproducibility
set.seed(2012)

### Define splitting index
hr_split <- initial_split(hr_modelling,
                          prop = 0.8,
                          strata = Attrition)

### Create a training set
hr_train <- training(hr_split)


## Instantiate a random forest model
## This model is selected due to the balance betweem
## accuracy and explainability

rf_tune_model <- rand_forest(mtry = tune(),
                             min_n = tune(),
                             trees = 500) |>
  
  ### Set engine
  set_engine("ranger",
             importance = "permutation") |>
  
  ### Set mode
  set_mode("classification")


## Create a model recipe
rf_rec <- recipe(Attrition ~ .,
                 data = hr_train) |>
  
  ### Remove near-zero variance predictors
  step_nzv(all_numeric_predictors()) |>
  
  ### handle multicollinearity
  step_corr(all_numeric_predictors(),
            threshold = 0.7) |>
  
  ### Dummy encode categorical predictors
  step_dummy(all_nominal_predictors()) |>
  
  ### Normalise numeric predictors
  step_normalize(all_numeric_predictors())


## Bundle model and recipe
rf_wfl <- workflow() |>
  
  ### Add model
  add_model(rf_tune_model) |>
  
  ### Add recipe
  add_recipe(rf_rec)


## Define cross-validation
rf_cv <- vfold_cv(hr_train,
                  v = 10,
                  strata = Attrition)

## Define tune grid
rf_grid <- grid_random(mtry(range = c(5, 10)),
                       min_n(range = c(1, 25)),
                       size = 30)

## Define tune metrics
rf_metrics <- metric_set(accuracy,
                         recall,
                         precision,
                         roc_auc)

## Tune the model
system.time({rf_tune <- tune_grid(rf_wfl,
                                  resamples = rf_cv,
                                  grid = rf_grid,
                                  metrics = rf_metrics)})

## Select the best hyperparametres
rf_best_hp <- select_best(rf_tune,
                          metric = "recall")

## Apply the best hyperparametres
rf_wfl_final <- finalize_workflow(rf_wfl,
                                  rf_best_hp)

## Fit the model
rf_wkl_fit <- last_fit(rf_wfl_final,
                       split = hr_split,
                       metrics = rf_metrics)


## Collect predictions
rf_predictions <- collect_predictions(rf_wkl_fit)

## Print predictions
rf_predictions


# # A tibble: 295 × 7
# .pred_class .pred_Yes .pred_No id     .row Attrition
# <fct>           <dbl>    <dbl> <chr> <int> <fct>    
# 1 No             0.0674    0.933 trai…    10 No       
# 2 No             0.127     0.873 trai…    11 No       
# 3 Yes            0.513     0.487 trai…    18 No       
# 4 No             0.149     0.851 trai…    29 No       
# 5 No             0.101     0.899 trai…    40 No       
# 6 No             0.343     0.657 trai…    42 No       
# 7 No             0.0981    0.902 trai…    45 No       
# 8 No             0.119     0.881 trai…    47 No       
# 9 No             0.165     0.835 trai…    48 No       
# 10 No             0.232     0.768 trai…    62 No       
# ℹ 285 more rows
# ℹ 1 more variable: .config <chr>
# ℹ Use `print(n = ...)` to see more rows


## Create a confusion matrix
rf_conf_mat <- conf_mat(rf_predictions,
                        truth = Attrition,
                        estimate = .pred_class)

## Print the confusion matrix
rf_conf_mat

#           Truth
# Prediction Yes  No
#       Yes   10   3
#       No    38 244


## Collect metrics
rf_perf_results <- collect_metrics(rf_wkl_fit)

## Print metrics
rf_perf_results

# A tibble: 4 × 4
# .metric   .estimator .estimate .config             
# <chr>     <chr>          <dbl> <chr>               
# 1 accuracy  binary      0.861 Preprocessor1_Model1
# 2 recall    binary      0.208 Preprocessor1_Model1
# 3 precision binary      0.769 Preprocessor1_Model1
# 4 roc_auc   binary      0.814 Preprocessor1_Model1


## Plot ROC curve
roc_curve(rf_predictions,
          truth = Attrition,
          .pred_Yes) |> 
  autoplot()


# ------------------------------------------------------


# Refit the model with upsampling step added

## Create a model recipe
rf_rec_1 <- recipe(Attrition ~ .,
                   data = hr_train) |>
  
  ### Remove near-zero variance predictors
  step_nzv(all_numeric_predictors()) |>
  
  ### handle multicollinearity
  step_corr(all_numeric_predictors(),
            threshold = 0.7) |>
  
  ### Dummy encode categorical predictors
  step_dummy(all_nominal_predictors()) |>
  
  ### Normalise numeric predictors
  step_normalize(all_numeric_predictors()) |>
  
  ### Oversample the outcome
  step_upsample(Attrition)


## Bundle model and recipe
rf_wfl_1 <- workflow() |>
  
  ### Add model
  add_model(rf_tune_model) |>
  
  ### Add recipe
  add_recipe(rf_rec_1)


## Define cross-validation
rf_cv <- vfold_cv(hr_train,
                  v = 10,
                  strata = Attrition)

## Define tune grid
rf_grid <- grid_random(mtry(range = c(5, 10)),
                       min_n(range = c(1, 25)),
                       size = 30)

## Define tune metrics
rf_metrics <- metric_set(accuracy,
                         recall,
                         precision,
                         roc_auc)

## Tune the model
system.time({rf_tune_1 <- tune_grid(rf_wfl_1,
                                    resamples = rf_cv,
                                    grid = rf_grid,
                                    metrics = rf_metrics)})

## Select the best hyperparametres
rf_best_hp_1 <- select_best(rf_tune_1,
                            metric = "recall")

## Apply the best hyperparametres
rf_wfl_final_1 <- finalize_workflow(rf_wfl,
                                  rf_best_hp_1)

## Fit the model
rf_wkl_fit_1 <- last_fit(rf_wfl_final_1,
                         split = hr_split,
                         metrics = rf_metrics)


## Collect predictions
rf_predictions_1 <- collect_predictions(rf_wkl_fit_1)

## Print predictions
rf_predictions_1


## Create a confusion matrix
rf_conf_mat_1 <- conf_mat(rf_predictions_1,
                        truth = Attrition,
                        estimate = .pred_class)

## Print the confusion matrix
rf_conf_mat_1

#           Truth
# Prediction Yes  No
#        Yes  20  14
#        No   28 233


## Collect metrics
rf_perf_results_1 <- collect_metrics(rf_wkl_fit_1)

## Print metrics
rf_perf_results_1

# # A tibble: 4 × 4
# .metric   .estimator .estimate .config             
# <chr>     <chr>          <dbl> <chr>               
# 1 accuracy  binary       0.858 Preprocessor1_Model1
# 2 recall    binary       0.417 Preprocessor1_Model1
# 3 precision binary       0.588 Preprocessor1_Model1
# 4 roc_auc   binary       0.810 Preprocessor1_Model1


## Plot ROC curve
roc_curve(rf_predictions_1,
          truth = Attrition,
          .pred_Yes) |> 
  autoplot()


## Get level of importance

### Get final fit model
rf_final_model_1 <- rf_wkl_fit_1 |>
  
  ### Extract workflow object
  extract_workflow() |>
  
  ### Extract fit object
  extract_fit_parsnip()

### Get levels of importance
vip(rf_final_model_1, num_features = 10)


## Get the directions of the relationships

### Create a vector of 5 most important predictors
important_predictors <- c("MonthlyIncome",
                          "OverTime_Yes",
                          "Age",
                          "YearsWithCurrManager",
                          "JobSatisfaction")

### For-loop through the vector
for (predictor in important_predictors) {
  
  #### Plot the relationship between predictor and attrition
  pd <- partial(rf_final_model$fit,
                pred.var = predictor,
                train = juice(prep(rf_rec)))
  
  #### Print plot
  print(autoplot(pd) + ggtitle(predictor))
}


# ------------------------------------------------------


## Comments:
## - We can predict attrition with 86% accuracy
## - The model shows a balance between true positive and false positive rates as ROC AUC is almost 81%
## - The model, however, falters with recall of just almost 44%
## - This is likely due to class imbalance in attrition as around 84% is "No" and 16% "Yes"
## - The model improvement will likely benefit from future with more positive attrition instances

## Based on the current model, the five most important predictors are:
## (1) Monthly income
## (2) Overtime (yes)
## (3) Age
## (4) Years with current manager
## (5) Job satisfaction


## Predictor 1. Monthly income
## Relationship with attrition: resembling U shape
## - This predictor is also not surprising.
## - Being employed is a transactional relationship.
## - If employees feel the financial return is not sufficient for their effort, they may leave in search of more satisfying contract.
## - Additionally, at a certain point, being paid more may lead employees to seek new opportunities where they may be able to earn even more.


## Predictor 2. Overtime (yes)
## Relationship with attrition: linear (positive)
## - This is not surprising given that overtime may be associated with workload or lead employees to perceive their work as more demanding.
## - This in turn may lead to more work-related stress, which make employees more likely to leave the company.


## Predict 3. Age
## Relationship with attrition: resembling U shape
## - The relationship between this predictor and attrition suggests that younger people are more likely to leave.
## - As they age, they become less likely to leave, up to a certain point.
## - After that, they become more likely to leave but not as likely as their younger counterparts.


## Predictor 4. Years with current managers
## Relationship with attrition: resembling L shape
## - There are two ways to interpret this.
## - First, this predictor may not be the cause but a correlate of attrition as people leave early are likely to have fewer years with their managers.
## - Second, years with current managers may reflect stagnation in the employees' career, which may in turn motivate employees to seek career growth elsewhere.


## Predict 5. Job satisfaction
## Relationship with attrition: almost linear (negative)
## - This predictor is intuitive in that people who are less satified with their jobs are more likely to leave the company.


## Recommendations based on the model

## Predictor 1. Monthly income
## Recommendation 1: Consider salary structure by balancing the incentive with workload. This may be done in conjunction with recommendation 1 from predictor 1.

## Predictor 2. Overtime (yes)
## Recommendation 1: Manage workload by reviewing the company overall workload and reallocating certain responsibilities and cutting down on non-essential tasks to relieve the employees of unnecessary workload.
## Recommendation 2: Implement new technology or work procedures which may facilitate work process, allowing employees to accomplish the same amount of work in less time. This is a win-win situation where the company enjoy the same level of productivity while the employees become happier.


## Predictor 3. Age
## Recommendation 1: The company may investigate whether the company culture is a good fit the younger hires, given that younger employees are more likely to leave the company. If the culture is a contributor, the company may implement a plan to adjust certain aspects of the company culture to be attractive to younger hires.
## Recommendation 2: Regarding older employees, the company may also explore what factors are driving these employees away. This may be due to culture or other job or workplace characteristics such as career path or promotion.


## Predictor 4. Years with current managers
## Recommendation 1: Validate whether years with current managers are predictive of attrition. This may be done by selectively interviewing employees or reviewing additional employee data.
## Recommendation 2: If years with current managers reflect , Communicate clear career path to the employees to ensure that they are aware of the opportunities for professional growth.


## Predictor 5. Job satisfaction
## Recommendation 1: Happy employees are less likely to leave. The company may consider implementing certain policy to ensure the emotional well-being of employees. If employees are not satisfied with of job-related stress, the company may, for example, consider contracting outsource psychologists to provide on-site or online counselling services to the employees.