
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Load data 
afcd <- load(file='data/afcd.rda') 

# for each nutrient give a boxplot of values, see if any specific studies or species keep showing up as outliers
# note studies and species that show up as outliers in this document