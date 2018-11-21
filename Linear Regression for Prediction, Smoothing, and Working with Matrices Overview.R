### Linear Regression for Prediction, Smoothing, and Working with Matrices Overview

# Use Galton Height Test Data
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


# Create train and test data set
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y,times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# Son's height independant of father
avg <- mean(train_set$son)
avg

mean((avg - test_set$son)^2)

# Least Squares Fitted Model
fit <- lm(son ~ father, data = train_set)
fit

# Loss Function
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)


