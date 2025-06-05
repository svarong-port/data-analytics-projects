# Analysing a Psychometric Assessment in R


# -------------------------------


# Prepared by

# Author: Shinin Varongchayakul

# Date: 31 May 2025

# Language: R


# -------------------------------


# Dataset

# Name: SD3

# Source: http://openpsychometrics.org/_rawdata/SD3.zip

# Retrived Date: 31 May 2025


# -------------------------------


# Additional Info

# SD-3 on Open-Source Psychometrics: https://openpsychometrics.org/tests/SD3/

# SD-3 Score Summary: https://openpsychometrics.org/tests/SD3/results.php


# -------------------------------


# 1. Install & Load Packages

# Install packages
install.packages("tidyverse") # data manupulation package
install.packages("psych") # EFA
install.packages("lavaan") # CFA

# Load packages
library(tidyverse)
library(psych)
library(lavaan)


# -------------------------------


# 2. Load & Prepare the Dataset

# 2.1 Load the Dataset

# Load the dataset
sd3 <- read_tsv("sd3_dataset.csv")

# Preview the dataset
head(sd3,
     n = 10)

# View the structure
glimpse(sd3)


# 2.2 Drop `country` and `source`

# Drop `country` and `source`
sd3_cleaned <- sd3 |>
  select(-last_col(),
         -last_col(offset = 1))

# Check the results
glimpse(sd3_cleaned)


# 2.3 Handle Missing Values

# Check for missing values
anyNA(sd3_cleaned)

# Comments:
# - No missing values found.


# 2.4 Handle Out-of-Bound Scores

# Count the number of out-of-bound scores in each column
sd3_cleaned |> 
  
  # Count out-of-bound records
  map_df(~sum(.x < 1 | .x > 5)) |>
  
  # Transpose the results
  t() |>
  
  # Convert to data frame
  as.data.frame() |>
  
  # Rename the count column
  setNames("Out-of-Bound")

# Comments
# - Since there are relatively few out-of-bound scores in each column, I will drop these records.

# Drop the records with out-of-bound scores
sd3_cleaned <- sd3_cleaned |>
  filter(if_all(everything(), 
                ~ .x >= 1 & .x <= 5))

# Check the results
sd3_cleaned |> 
  
  # Count out-of-bound records
  map_df(~sum(.x < 1 | .x > 5)) |>
  
  # Transpose the results
  t() |>
  
  # Convert to data frame
  as.data.frame() |>
  
  # Rename the count column
  setNames("Out-of-Bound")


# 2.5 Reverse-Score the Items

# Define the items to reverse-score
rs_items <- c("N2", "N6", "N8", "P2", "P7")

# View the scores
head(sd3_cleaned[, rs_items])

# Reverse-score the items
sd3_cleaned <- sd3_cleaned|>
  mutate(across(all_of(rs_items),
                ~ 6 - .x))

# Check the results
head(sd3_cleaned[, rs_items])


# -------------------------------


# 3. Psychometric Analysis

# 3.1 Reliablity

# Compute Cronbach's alpha

# Mach scale
alpha(sd3_cleaned |>
        select(starts_with("M")))

# Narcissism scale
alpha(sd3_cleaned |>
        select(starts_with("N")))

# Psychopathy scale
alpha(sd3_cleaned |>
        select(starts_with("P")))


# -------------------------------


# 4. Validity Analysis

# Split the data

# 4.1 Split the dataset

# Set seed for reproducibility
set.seed(42)

# Define row index for EFA
efa_rows <- sample(nrow(sd3_cleaned),
                   nrow(sd3_cleaned) * 0.5)

# Create dataset for EFA
sd3_efa <- sd3_cleaned[efa_rows, ]

# Create dataset for CFA
sd3_cfa <- sd3_cleaned[-efa_rows, ]

# Check the results
cat("EFA dataset:", nrow(sd3_efa), "\n")
cat("CFA dataset:", nrow(sd3_cfa), "\n")


# 4.2 Exploratory Factor Analysis (EFA)

# Determine the number of factors
fa.parallel(sd3_efa,
            fa = "fa",
            n.iter = 100,
            main = "Scree Plot for EFA")

# Comments:
# - The elbow method suggests a 3-factor solution.
# - The parallel method suggests a 9-factor solution.

# Perform EFA with 3 factors
efa_3 <- fa(sd3_efa,
            nfactors = 3,
            rotate = "oblimin",
            fm = "ml")

# Print the results
print(efa_3,
      cut = 0.3,
      sort = TRUE)

# Perform EFA with 9 factors
efa_9 <- fa(sd3_efa,
            nfactors = 9,
            rotate = "oblimin",
            fm = "ml")

# Print the results
print(efa_9,
      cut = 0.3,
      sort = TRUE)


# 4.3 Confirmatory Factor Analysis (CFA)

# Define the items
sd3_3fac <- "
  Machiavellianism =~ M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9
  Narcissism       =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9
  Psychopathy      =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9
"

# Perform CFA
cfa_3fac <- cfa(sd3_3fac,
                data = sd3_cfa,
                estimator = "MLR")

# Print the results
summary(cfa_3fac,
        fit.measures = TRUE,
        standardized = TRUE)