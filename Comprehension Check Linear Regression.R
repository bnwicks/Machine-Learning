### Comprehension Check: Linear Regression
library(caret)
library(tidyverse)

# Question 1

y_rmse <- c(1:100)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
dat

set.seed(1)
for(i in 1:100) # For Loop (Should Use Replicate)
  {    
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  # Train linear model
  fit <- lm(y ~ x, data = train_set)
  fit
  
  # Loss Function
  y_hat <- predict(fit, test_set)
  y_rmse[i] <- sqrt(mean((y_hat - test_set$y)^2))
  print(i)
}

y_rmse
mean(y_rmse)
sd(y_rmse)

# Question 2 

set.seed(1)

myRMSE <- function(size)
{
  set.seed(1)
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  

  RMSE <- replicate(n = size, {
  
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    # Train linear model
    fit <- lm(y ~ x, data = train_set)
    
    # Loss Function
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(RMSE),sd(RMSE))
}

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
f<-sapply(n, myRMSE)
f

# Question 4

myRMSE <- function(size)
{
  set.seed(1)
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  RMSE <- replicate(n = size, {
    
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    # Train linear model
    fit <- lm(y ~ x, data = train_set)
    
    # Loss Function
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(RMSE),sd(RMSE))
}

n <- c(100)
set.seed(1)
f<-sapply(n, myRMSE)
f

# Question 6
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# Train linear model
fit_1 <- lm(y ~ x_1, data = train_set)
fit_2 <- lm(y ~ x_2, data = train_set)
fit_12 <- lm(y ~ x_1 + x_2, data = train_set)

# Loss Functions
y_hat_1 <- predict(fit_1, test_set)
RMSE_1 <- sqrt(mean((y_hat_1 - test_set$y)^2))

y_hat_2 <- predict(fit_2, test_set)
RMSE_2 <- sqrt(mean((y_hat_2 - test_set$y)^2))

y_hat_12 <- predict(fit_12, test_set)
RMSE_12 <- sqrt(mean((y_hat_12 - test_set$y)^2))  




