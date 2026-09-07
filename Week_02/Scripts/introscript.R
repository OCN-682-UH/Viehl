### This is my first script!
### I am learning how to import data

# Created by: Kat Viehl
# Created on: 2026-09-06


### --------------------------------
# Load libraries

library(tidyverse)
library(here)

### --------------------------------
# Read in my data

WeightData <- read_csv(here("Week_02", "Data", "weightdata.csv"))

### --------------------------------
# Data analysis

head(WeightData)
tail(WeightData)
view(WeightData)

