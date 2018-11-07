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

# Confusion Matrix
table(predicted = y_hat, actual = test_set$sex)

# % Percentages
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarise(accuracy = mean(y_hat == sex))

# Pervalence of Males in dataset
prev <- mean( y == "Male")
prev

# Sensitivity / Specificity
# High sensitivity means y equals 1 implies y hat equals 1.
# High specificity means y equals 0 implies y hat equals 0.

confusionMatrix(data = y_hat, reference = test_set$sex)

# Balanced accuracy is average of specificity and sensitivity

# F1 Measure weights sensitivity and specificity
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

plot(cutoff, F_1)

# Test Cutoff Values for F1 with test data
best_F_1_cutoff <- 66
y_hat <- ifelse(test_set$height > best_F_1_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)

confusionMatrix(data = y_hat, reference = test_set$sex)

# Sensitivity / Specificity with cutoff
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height Cutoff",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

plot(height_cutoff$FPR, height_cutoff$TPR)

