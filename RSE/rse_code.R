# Code for Rosenberg Self-Esteem (RSE) Analysis

# Dataset source: https://openpsychometrics.org/_rawdata/


# --------------------------------------


# Load libraries
library("tidyverse")
library(data.table)


# --------------------------------------


# Load the dataset
df <- read.csv("rse_dataset.csv", sep = "\t")

# Preview the dataset
head(df, 10)
glimpse(df)

# 