# Exploring & Predicting Used Car Pricing in Python

# Prepared by
# Author: Shinin Varongchayakul
# Date: 15 Jun 2025
# Language: Python

# Dataset
# Name: Vehicle Dataset
# Source: https://www.kaggle.com/datasets/nehalbirla/vehicle-dataset-from-cardekho?resource=download
# Retrieved Date: 15 Jun 2025

# ----------------------------------------------------------------------------------------

# 1. Import Libraries

# Load libraries
import pandas as pd # data manipulation
import matplotlib.pyplot as plt # data viz
import seaborn as sns # data viz


# 2. Load Dataset

# Load dataset
vehicles = pd.read_csv("vehicle_dataset.csv")

# Preview the first 10 rows
vehicles.head(10)

# Get info
vehicles.info()

# Get summary stats
vehicles.describe(include='all')

# View the duplicates
vehicles[vehicles.duplicated()].sort_values(by='name')

# Get original dimensions
vehicles.shape

# Drop the duplicates
vehicles_cleaned = vehicles.drop_duplicates()

# Check the dimensions
vehicles_cleaned.shape
