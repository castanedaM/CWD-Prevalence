# Title: CWD Prevalence Project
# Author: Mariana Castaneda-Guzman
# Date last updated: 9/17/2020


# Read data
cwd <- read.csv("Master_CWD_Data.csv", header = TRUE, stringsAsFactors = FALSE)

str(cwd)

# Set categorical data to factors and continous to numeric
factor_columns <- c(1:5, 8, 21, 22)
numeric_columns <- c(6, 7, 9:20, 23:44)

data[ , factor_columns] <- lapply(data[ , factor_columns], as.factor)
data[ , numeric_columns] <- lapply(data[ , numeric_columns], as.numeric)


# Generates Multivariate Imputations by Chained Equations (MICE)
library(mice) 

# Display missing-data patterns per row per column. The total of NA's shows in
# the left y-axis (pink = NA, blue = exists).
md.pattern(cwd, rotate.names = TRUE)


# Visualization of missing or imputed values in
library(VIM)

# Display histogram and pattern of NAs as well as list of frequency values. 
aggr_plot <- aggr(cwd, 
                  col=c('lightgreen','pink'),
                  numbers = TRUE, 
                  sortVars = TRUE, 
                  labels = names(cwd),
                  cex.axis = .7, 
                  gap = 3, 
                  ylab = c("Histogram of missing data","Pattern"))


# Assigned/impute values to missing data, using MICE
library(missForest)

# Use the set.seed function when running simulations to ensure all results,
# figures, etc are reproducible.

# Got an error on line below: Error in solve.default(xtx + diag(pen)) : system
# is computationally singular: reciprocal condition number = 7.72413e-19. Forced
# model to use "cart" (classification and regression trees). Solution and
# explanation found here:
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586

tempData <- mice(cwd, method = "cart", seed = 12345)

# https://cran.r-project.org/web/packages/missForest/missForest.pdf Runs random
# forest to impute missing values, and can be ran in parrallel Default 10
# iterations, return normalized root mean squared error (NRMSE) and The
# proportion of falsely classifeid (PFC)

tempData <- missForest(data)
