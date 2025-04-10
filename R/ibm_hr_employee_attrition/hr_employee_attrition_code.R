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
install.packages("themis") # upsampling technique
install.packages("vip") # feature importance in ML models
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
cat_vars <- c("Attrition", "BusinessTravel", "Department",
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


### Remove non-predictive variables
hr_cleaned <- hr_cleaned |>
  
  #### Deselect
  select(-EmployeeCount,
         -EmployeeNumber,
         -Over18)

### Check the results
glimpse(hr_cleaned)


## Handle missing values

### Check for missing values
anyNA(hr_cleaned)

### Comment: No missing values found


## Summarise the attrition rate
hr_cleaned |>
  
  ### Group and count by attrition
  count(Attrition, 
        name = "Count") |>
  
  ### Compute percentage
  mutate(Percent = round(Count / sum(Count) * 100, 2))

#   Attrition Count Percent
# 1       Yes   237   16.12
# 2        No  1233   83.8

## Comments:
## - There are quite fewer people who left than people who stayed.


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
  
  ### Visualise the correlation matrix
  ggcorrplot(lab = TRUE,
             type = "lower",
             colors = c("blue", "white", "red"),
             lab_size = 2.5)

## Comments:
## - All correlations between attrition and other variables are low (min = 0; max = 0.17).
## - This suggests that attrition may be a result of a combination of several factors rather than any single factor alone.


# ------------------------------------------------------


# EDA, part 2

# Explore 3 factors likely to affect attrition

# The 3 factors:
# 1. Monlthy income: employees may leave because of insufficient financial incentive, with lower income associated with higher attrition rate.
# 2. Overtime: employees may leave because of workload, where those with more overtime more likely to leave.
# 3. Environment satisfaction: those unsatisfied with their workplace settings may be more likely to leave.


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
## - On average, those who stayed appear to earn more than those who left.
## - This suggests that monthly income is a contributing factor to attrition.


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
## - The distributions of both attrition groups are positively skewed.
## - There were more proportionally more people who left in the lower end of the monthly income distribution.
## - This adds further support to the earlier analysis that monthly income is a contributor of attrition.


## 2. Overtime vs attrition
hr_cleaned |>
  
  ### Group by overtime and attrition
  group_by(OverTime, Attrition) |>
  
  ### Count the observations in each group
  summarise(Count = n(),
            .groups = "drop") |>
  
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
## - In both overtime conditions, there were more people who stayed than who left.
## - The attrition rate was significantly higher in the overtime condition than in the no-overtime condition.
## - Conversely, the percentage of those who stayed was higher in no-overtime condition than in the overtime condition.
## - This suggests that overtime contributes attrition.


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
## - There is a significant difference in environment satisfaction between the 2 attrition groups.
## - Those who left tended to be less satisfied with the workplace settings than those who stayed.
## - This suggests that environment satisfaction is likely a contributing factor to attrition.


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
## - This suggests that the difference, while statistically significant, may not be as practically important as other factors such as monthly income and overtime.
## - Thus, when tackling attrition, environment satisfaction may be given lower priority.


# ------------------------------------------------------


# EDA, part 3

# Explore attrition patterns by department and job role

## Attrition by department
hr_cleaned |>
  
  ### Group by department and attrition
  group_by(Department, Attrition) |>
  
  ### Count the number of observations
  summarise(Count = n(),
            .groups = "drop") |>
  
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
  
  ### Add percent text
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
## - All departments have a similar percentage of people leaving and staying.
## - Notably, however, R&D has a lower percent of people leaving and a slightly higher percentage of people staying compared to the other departments.
## - Additionally, Sales has the highest percentage of people leaving and lowest percetage of people staying. This suggests that the attrition may have the most impact in Sales.


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
  
  ### Add percent text
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
## - The Sales Rep role has the highest percentage of attrition.
## - The Research Director role has the lowest percentage of attrition.
## - Given the job role titles, employees in lower job levels appear to leave more often.

### Confirm whether job level is associated with attrition
hr_cleaned |>
  
  #### Group by job levels
  group_by(JobLevel) |>
  
  #### Compute attrition rate
  summarise(AttritionRate = mean(Attrition == "Yes") * 100) |>
  
  #### Ungroup
  ungroup() |>
  
  #### Aesthetic mapping
  ggplot(aes(x = JobLevel,
             y = AttritionRate,
             fill = JobLevel)) +
  
  #### Call on bar plot
  geom_col() +
  
  #### Add percent text
  geom_text(aes(label = paste(round(AttritionRate, 0), "%")),
            vjust = -0.5,
            size = 3) +
  
  #### Add text elements
  labs(title = "Attrition Rate by Job Level",
       x = "Job Levels",
       y = "Attrition Rate (%)",
       fill = "Job Levels") +
  
  #### Adjust x scale
  scale_x_discrete() +
  
  #### Adjust theme to classic for easy viewing
  theme_classic()

## Comments:
## - Job level 1 has the highest attrition percentage, while job levels 4 and 5 have the lowest attrition percentages.
## - This supports the earlier notion that employees in lower job levels are more likely to leave the company.


# ------------------------------------------------------


# EDA, part 4

# Explore the attrition pattens by gender and age

## Gender vs attrition
hr_cleaned |>
  
  ### Group by gender and attritiom
  group_by(Gender, Attrition) |>
  
  ### Count the number of observations
  summarise(Count = n(),
            .groups = "drop") |>
  
  ### Group by gender
  mutate(Percent = Count / sum(Count) * 100) |>
  
  ### Ungroup
  ungroup() |>
  
  ### Aesthetic mapping
  ggplot(aes(x = Gender,
             y = Percent,
             fill = Attrition)) +
  
  ### Call on bar plot
  geom_col(position = "dodge") +
  
  ### Add percent text
  geom_text(aes(label = paste(round(Percent, 0), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  
  ### Add text elements
  labs(title = "Attrition by Gender",
       x = "Gender",
       y = "Attrition Percentage",
       fill = "Attrition") +
  
  ### Adjust x scale
  scale_x_discrete() +
  
  ### Adjust theme to classic for easy viewing
  theme_classic()

## Comments:
## - In both attrition conditions, there were more males than females.
## - Among both genders, more people left than stayed.
## - Since the attrition patterns are similar across genders, this variable may contribute weakly to attrition.


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
## - The age distributions are similar across attrition conditions.
## - The age distribution for those who stayed is more normally distributed.
## - The peaks are close together.
## - As the peak for those who left leans more towards the left, it suggests that people who left tend to be younger.
## - This suggests that age may be a worthwhile predictor of attrition.


# ------------------------------------------------------


# Predictive Modelling

# Questions:
# - Can we predict attrition?
# - If so, with how much accuracy?
# - What are the five most important predictors in the model?

## Split the data

### Set seed for reproducibility
set.seed(1853)

### Define splitting index
hr_split <- initial_split(hr_cleaned,
                          prop = 0.8,
                          strata = Attrition)

### Create a training set
hr_train <- training(hr_split)

### Create a training set
hr_test <- testing(hr_split)


## Instantiate a random forest model
## This model is selected due to the balance betweem
## accuracy and explainability

rf_model <- rand_forest(mtry = tune(),
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
  step_dummy(all_nominal_predictors())


## Bundle model and recipe
rf_wfl <- workflow() |>
  
  ### Add model
  add_model(rf_model) |>
  
  ### Add recipe
  add_recipe(rf_rec)


## Define cross-validation

### Set seed for reproducibility
set.seed(1853)

### Set CV
rf_cv <- vfold_cv(hr_train,
                  v = 10,
                  strata = Attrition)

## Define tune grid

### Set seed for reproducibility
set.seed(1853)

### Define the grid
rf_grid <- grid_random(mtry(range = c(5, 10)),
                       min_n(range = c(1, 25)),
                       size = 30)

## Define tune metrics
rf_metrics <- metric_set(accuracy,
                         recall,
                         precision,
                         roc_auc)

## Tune the model

### Set seed for reproducibility
set.seed(1853)

### Tune
system.time({rf_tune <- tune_grid(rf_wfl,
                                  resamples = rf_cv,
                                  grid = rf_grid,
                                  metrics = rf_metrics)})

#    user  system elapsed 
#  183.42    2.81  370.84 


## Show the best hyperparametres

### Define the metrics
metrics <- c("accuracy",
             "precision",
             "recall",
             "roc_auc")

### For-loop through the metrics
for (metric in metrics) {
  
  ## Get best hyperparametres
  best <- show_best(rf_tune,
                    metric = metric)
  
  ## Print the results
  print(paste(metric, ":"))
  print(best)
  print("----------------------------------------------")
}


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
head(rf_predictions)


## Create a confusion matrix
rf_conf_mat <- conf_mat(rf_predictions,
                        truth = Attrition,
                        estimate = .pred_class)

## Print the confusion matrix
rf_conf_mat

#            Truth
# Prediction Yes  No
#        Yes   3   2
#        No   45 245


## Collect metrics
rf_perf_results <- collect_metrics(rf_wkl_fit)

## Print metrics
rf_perf_results

# A tibble: 4 × 4
#   .metric   .estimator .estimate .config             
#   <chr>     <chr>          <dbl>  <chr>               
# 1 accuracy  binary         0.841  Preprocessor1_Model1
# 2 recall    binary         0.0625 Preprocessor1_Model1
# 3 precision binary         0.6    Preprocessor1_Model1
# 4 roc_auc   binary         0.784  Preprocessor1_Model1


## Comments:
## - While this initial model shows relatively high accuracy (84%), its recall is extremely poor (6%).
## - This suggests that the model is very adept at correctly identifying people who are staying but performs poorly when it comes to identifying people who are leaving.
## - This is clearly shown in the confusion matrix, where, when predicting with the negative class, the model correctly classified 245 out of 247 cases. In contrast, the model correctly identified only 3 out of 48 positive class instances.
## - Next, I will try to improve the model by using downsampling and upsampling methods when tuning and training the model.


# ------------------------------------------------------


# Refit the model with upsampling step added

## Instantiate a random forest model
rf_model_1 <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 500) |>
  
  ### Set engine
  set_engine("ranger",
             importance = "permutation") |>
  
  ### Set mode
  set_mode("classification")


## Create a model recipe

### Set seed for reproducibility
set.seed(1853)

### Create the recipe
rf_rec_1 <- recipe(Attrition ~ .,
                   data = hr_train) |>
  
  ### Remove near-zero variance predictors
  step_nzv(all_numeric_predictors()) |>
  
  ### handle multicollinearity
  step_corr(all_numeric_predictors(),
            threshold = 0.7) |>
  
  ### Dummy encode categorical predictors
  step_dummy(all_nominal_predictors()) |>
  
  ### Downsampling
  step_downsample(Attrition) |>
  
  ### Oversampling
  step_upsample(Attrition)


## Bundle model and recipe
rf_wfl_1 <- workflow() |>
  
  ### Add model
  add_model(rf_model_1) |>
  
  ### Add recipe
  add_recipe(rf_rec_1)


## Define cross-validation

### Set seed for reproducibility
set.seed(1853)

### Set CV
rf_cv_1 <- vfold_cv(hr_train,
                    v = 10,
                    strata = Attrition)

## Define tune grid

### Set seed for reproducibility
set.seed(1853)

### Define the grid
rf_grid_1 <- grid_random(mtry(range = c(5, 10)),
                         min_n(range = c(1, 25)),
                         size = 30)

# Define tune metrics
rf_metrics_1 <- metric_set(accuracy,
                           recall,
                           precision,
                           roc_auc)

## Tune the model

### Set seed for reproducibility
set.seed(1853)

### Tune
system.time({rf_tune_1 <- tune_grid(rf_wfl_1,
                                    resamples = rf_cv_1,
                                    grid = rf_grid_1,
                                    metrics = rf_metrics_1)})

#    user  system elapsed 
#   31.09    0.94  102.76


## Show the best hyperparametres

### Define the metrics
metrics_1 <- c("accuracy",
               "precision",
               "recall",
               "roc_auc")

### For-loop through the metrics
for (metric in metrics_1) {
  
  #### Get best hyperparametres
  best <- show_best(rf_tune_1,
                    metric = metric)
  
  #### Print the results
  print(paste(metric, ":"))
  print(best)
  print("----------------------------------------------")
}


## Select the best hyperparametres
rf_best_hp_1 <- select_best(rf_tune_1,
                            metric = "precision")

## Apply the best hyperparametres
rf_wfl_final_1 <- finalize_workflow(rf_wfl_1,
                                    rf_best_hp_1)

## Fit the model
rf_wkl_fit_1 <- last_fit(rf_wfl_final_1,
                         hr_split,
                         metrics = rf_metrics_1)


## Collect predictions

### Collect predictions
rf_predictions_1 <- collect_predictions(rf_wkl_fit_1)

### Print the results
head(rf_predictions_1)


## Create a confusion matrix

### Create a confusion matrix
rf_conf_mat_1 <- conf_mat(rf_predictions_1,
                          truth = Attrition,
                          estimate = .pred_class)

### Print the confusion matrix
rf_conf_mat_1

#            Truth
# Prediction Yes  No
#        Yes  31  55
#        No   17 192


## Get metrics

### Collect metrics
rf_perf_results_1 <- collect_metrics(rf_wkl_fit_1)

### Print metrics
rf_perf_results_1

# # A tibble: 4 × 4
#   .metric   .estimator .estimate .config             
#   <chr>     <chr>          <dbl> <chr>               
# 1 accuracy  binary         0.756 Preprocessor1_Model1
# 2 recall    binary         0.646 Preprocessor1_Model1
# 3 precision binary         0.360 Preprocessor1_Model1
# 4 roc_auc   binary         0.775 Preprocessor1_Model1


## Compute ROC AUC
roc_auc(rf_predictions_1,
        truth = Attrition,
        .pred_Yes)

# # A tibble: 1 × 3
#   .metric .estimator .estimate
#   <chr>   <chr>          <dbl>
# 1 roc_auc binary         0.775

## Comments:
##  - The downsampling and upsampling methods significantly improves the model's recall from 6% up to 64%.
## - However, this comes at a cost to the other performance aspects.
## - Specifically, accuracy decreased froom 84% to 75%, precision from 60% to 36%, and ROC AUC from 78% to 77%.
## - While this is remarkable improvement, the model may be further ehanced by adjusting the prediction threshold.


# ------------------------------------------------------


# Calibrate the model performance by adjust the threshold for predicting positive class

## Define a set of threshold
thresholds <- seq(0.1,
                  0.9,
                  by = 0.1)


## Create empty vectors to store results from a for loop

### Recall values
recalls <- numeric(length(thresholds))

### Precision values
precisions <- numeric(length(thresholds))

### The number of all cases
all <- numeric(length(thresholds))

### The number of all positive classes
all_positives <- numeric(length(thresholds))

### The number of true positives
true_positives <- numeric(length(thresholds))


## Create a for loop to loop through the thresholds
for (i in 1:length(thresholds)) {
  
  ### Make predictions with the threshold
  new_pred <- if_else(rf_predictions_1$.pred_Yes > thresholds[i],
                      "Yes",
                      "No")
  
  ### Create a tibble to store the results
  predictions <- tibble(actual = hr_test$Attrition,
                        predicted = factor(new_pred, 
                                           levels = c("Yes", "No")))
  
  ### Calculate recall
  recalls[i] <- recall(predictions,
                       truth = actual,
                       estimate = predicted,
                       event_level = "first") |>
    pull(.estimate)
  
  ### Calculate precision
  precisions[i] <- precision(predictions,
                             truth = actual,
                             estimate = predicted,
                             event_level = "first") |>
    pull(.estimate)
  
  
  ### Count all cases
  all[i] <- nrow(predictions)
  
  ### Calculate all positive cases
  all_positives[i] <- sum(predictions$predicted == "Yes")
  
  ### Calculate actual positive cases
  true_positives[i] <- sum(predictions$predicted == "Yes" & predictions$actual == "Yes")
  
}

## Store the thresholds and corresponding values
thres_results <- tibble(threshold = thresholds,
                        recall = recalls,
                        precision = precisions,
                        total = all,
                        flagged = all_positives,
                        true_pos = true_positives)

## Print the for loop results
thres_results

# A tibble: 9 × 6
#   threshold recall precision total flagged true_pos
#       <dbl>  <dbl>     <dbl> <dbl>   <dbl>    <dbl>
# 1       0.1 0.938      0.178   295     253       45
# 2       0.2 0.771      0.274   295     135       37
# 3       0.3 0.583      0.364   295      77       28
# 4       0.4 0.417      0.606   295      33       20
# 5       0.5 0.229      0.733   295      15       11
# 6       0.6 0.104      0.714   295       7        5
# 7       0.7 0.0417     0.667   295       3        2
# 8       0.8 0         NA       295       0        0
# 9       0.9 0         NA       295       0        0


## Plot threshold vs recall and precision

### Precision
plot(thres_results$threshold,
     thres_results$precision,
     type = "b",
     main = "Precision & Recall vs Threshold",
     xlab = "Threshold",
     ylab = "Score",
     col = "blue",
     pch = 16,
     ylim = c(0, 1))

### Recall
lines(thres_results$threshold,
      thres_results$recall,
      type = "b",
      col = "red",
      pch = 17)

### Add a legend
legend("topright",
       legend = c("Precision", "Recall"),
       col = c("blue", "red"),
       pch = c(16, 17),
       lty = 1)


## Comments:
## - Although we are prioritising recall to identify potential leavers, a high recall means we will also incorrectly classify employees who are staying as leavers.
## - A high number of false positives, while beneficial for retaining the company's talent, could lead to additional costs as resources are allocated to employees who are staying, instead of those who are leaving.
## - As such, while we emphasize recall, this should not come at too great a cost to precision.
## - Given this reasoning, I have chosen a threshold of 0.6, as it represents a good balance between recall and precision. Specifically, this threshold allows us to flag 37 employees out of 295, of which 20 are true positives.
## - If we adjust the threshold to 0.5, we would flag about twice as many (86 employees), but less than half of them (31) would be true positives.
## - With threshold of 0.6, the company would be able to identify a small yet concentrated number of employees who are likely to leave and will benefit from any invested intervention.


## Select the threshold
selected_thres <- 0.6


## Make poredictions with threshold = 0.6
rf_final_pred <- if_else(rf_predictions_1$.pred_Yes > selected_thres,
                         "Yes",
                         "No")

## Store the results in a tibble
rf_final_results <- tibble(actual = hr_test$Attrition,
                           predicted = factor(rf_final_pred,
                                              levels = c("Yes", "No")))


## Create a confusion matrix
rf_final_conf_mat <- conf_mat(rf_final_results,
                              truth = actual,
                              estimate = predicted)

## Get metrics
summary(rf_final_conf_mat)

# # A tibble: 13 × 3
#   .metric               .estimator .estimate
#   <chr>                 <chr>          <dbl>
# 1 accuracy              binary         0.847
# 2 kap                   binary         0.383
# 3 sens                  binary         0.417
# 4 spec                  binary         0.931
# 5 ppv                   binary         0.541
# 6 npv                   binary         0.891
# 7 mcc                   binary         0.388
# 8 j_index               binary         0.348
# 9 bal_accuracy          binary         0.674
# 10 detection_prevalence binary         0.125
# 11 precision            binary         0.541
# 12 recall               binary         0.417
# 13 f_meas               binary         0.471


# ------------------------------------------------------


# Get variable importance and interpret the results

## Get level of importance

### Get final fit model
rf_final_model_1 <- extract_fit_parsnip(rf_wkl_fit_1)

### Get levels of importance
vip(rf_final_model_1)


## Get the directions of the relationships

### Create a vector of 5 most important predictors
important_predictors <- c("MonthlyIncome",
                          "OverTime_Yes",
                          "Age",
                          "YearsWithCurrManager",
                          "EnvironmentSatisfaction")

### For-loop through the vector
for (predictor in important_predictors) {
  
  #### Plot the relationship between predictor and attrition
  pd <- partial(rf_final_model_1$fit,
                pred.var = predictor,
                train = juice(prep(rf_rec)))
  
  #### Print plot
  print(autoplot(pd) + ggtitle(predictor))
}


# ------------------------------------------------------


## Comments:
## - We can predict attrition with 86% accuracy.
## - The model shows a balance between true positive and false positive rates as ROC AUC is 76%.
## - Despite class imbalance in the outcome, The model demonstrates a good balance between recall and precision, with recall reported at around 41% and precision 60%.
## - The model may be improved by providing more balanced dataset.


## Based on the current model, the five most important predictors are:
## (1) Monthly income
## (2) Overtime (yes)
## (3) Age
## (4) Years with current manager
## (5) Environment satisfaction


## Predictor 1. Monthly income
## Relationship with attrition: resembling U shape
## - This predictor is also not surprising as employment is a transactional relationship.
## - If employees feel the financial return is not sufficient for their effort, they may leave in search of a more satisfying contract.


## Predictor 2. Overtime (yes)
## Relationship with attrition: linear (positive)
## - This is not surprising, given that overtime may be associated with workload or lead employees to perceive their work as more demanding.
## - This, in turn, may lead to more work-related stress, which makes employees more likely to leave the company.


## Predictor 3. Age
## Relationship with attrition: resembling L shape
## - The relationship between this predictor and attrition suggests that younger people are more likely to leave.
## - As they age, they become less likely to leave, up to a certain point.
## - After that, they become more likely to leave, but not as likely as their younger counterparts.


## Predictor 4. Years with current manager
## Relationship with attrition: resembling L shape
## - There are two ways to interpret this.
## - First, this predictor may not be the cause but a correlate of attrition, as people who leave early are likely to have fewer years with their managers.
## - Second, years with current managers may reflect stagnation in the employees' career, which may, in turn, motivate employees to seek career growth elsewhere.


## Predictor 5. Environment satisfaction
## Relationship with attrition: resembling L shape
## - This predictor is in line with the two-factor theory, which states that part of employees' motivation stems from the work environment.
## - The direction of the relationship suggests that the less satisfied the employees are with their environment, the more likely they are to leave.


## Recommendations based on the model

## Predictor 1. Monthly income
## Recommendation 1: Consider the salary structure by balancing the incentive with the workload. This may be done in conjunction with recommendation 1 from predictor 1.

## Predictor 2. Overtime (yes)
## Recommendation 1: Manage the workload by reviewing the company's overall workload and reallocating certain responsibilities, as well as cutting down on non-essential tasks to relieve employees of unnecessary workload.
## Recommendation 2: Implement new technology or work procedures which may facilitate work processes, allowing employees to accomplish the same amount of work in less time. This is a win-win situation where the company enjoys the same level of productivity while the employees become happier.

## Predictor 3. Age
## Recommendation 1: The company may investigate whether the company culture is a good fit for younger hires, given that younger employees are more likely to leave the company. If the culture is a contributor, the company may implement a plan to adjust certain aspects of the company culture to be more attractive to younger hires.
## Recommendation 2: Regarding older employees, the company may also explore what factors are driving these employees away. This may be due to culture or other job or workplace characteristics, such as career path or promotion opportunities.

## Predictor 4. Years with current managers
## Recommendation 1: Validate whether years with current managers are predictive of attrition. This may be done by selectively interviewing employees or reviewing additional employee data.
## Recommendation 2: If years with current managers reflect stagnation, communicate a clear career path to employees to ensure that they are aware of the opportunities for professional growth.

## Predictor 5. Environmental satisfaction
## Recommendation 1: The company may dive deeper into which environmental factors may discourage employees from staying. These may be physical (e.g., workspace, common areas), social (e.g., peers, supervisors), or a combination of both. By analysing which contextual factors contribute to attrition, the company has a greater chance of successfully tackling attrition.