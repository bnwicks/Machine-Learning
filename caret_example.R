# Caret Library Examples

# Librarys
library(caret)
library(tidyverse)
library(tidyr)
library(dslabs)

# Data
data(heights)

# Vars
y <- heights$sex
x <- heights$height

# Create Test Data Sets
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

#Random Sample Estimate
y_hat <- 
  sample(c("Male", "Female"),length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

mean(y_hat ==  test_set$sex)

# Heights Distribution
heights %>% group_by(sex) %>%
  summarise(mean(height), sd(height))

# Predicting using Std Deviation
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

mean(y == y_hat)

# Investigating a Cutoff Values for training data
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
  })

plot(cutoff, accuracy)
best_cutoff <- 64

# Test Cutoff Values for test data
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)


