# Distance, Knn, Cross-validation, and Generative Models

library(caret)

#GLM Fit
fit_glm <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = "binomial")

p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5,7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall["Accuracy"]

# KNN Fit
fit_knn <- knn3(y ~ ., data = mnist_27$train, k =5)

#OR
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

y_hat_knn <- predict(fit_knn, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#Overtraining and Oversmoothing
y_hat_knn <- predict(fit_knn, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(fit_knn, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

# Unique Predictors
knn_fit_1 <- knn3(y ~., data = mnist_27$train, k = 1)

y_hat_knn <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn_fit_1 <- predict(knn_fit_1, data = mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

# Example (k to large / oversmoothing)
knn_fit_401 <- knn3(y ~., data = mnist_27$train, k =401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type="class")
confusionMatrix(data = y_hat_knn_401, reference = mnist_27$test$y)$overall["Accuracy"]

# for values of k
library(purrr)
ks <- seq(3, 251, 2)

accuracy <- map_df(ks, function(k){
  
  fit <- knn3(y ~., data = mnist_27$train, k =k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  
  train_error <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type="class")
  
  test_error <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
  
  list(train = train_error, test = test_error)
  
})

accuracy %>% ggplot(aes(ks)) +
  geom_line(aes(y = test, colour = "var0")) + 
  geom_line(aes(y = train, colour = "var1"))


# Comprehension Check: Nearest Neighbors
library(dslabs)
library(tidyverse)
library(caret)
data(heights)

set.seed(1)

ks<-seq(1,251,.1)

y <-heights$sex
x <- heights$height

test_index<- createDataPartition(y, times=1, p=0.5, list=FALSE)

train_set<- heights[-test_index,]
test_set<- heights[test_index,]

accuracy <- map_df(ks, function(k) {
  
  fit<- knn3(sex~height, data=train_set, k=k)
  
  y_hat<-predict(fit, test_set, type="class") %>% 
    factor(levels = levels(train_set$sex))
  
  F_val<-F_meas(data=y_hat, reference=factor(train_set$sex))
  
  list(k=k, F_val=F_val)
  
})

accuracy

accuracy %>% ggplot(aes(k,F_val)) + geom_line()
max(accuracy[1:500,2])

##Answer
set.seed(1)
data("heights")
library(caret)
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
  test_set <- heights[test_index, ]
  train_set <- heights[-test_index, ]
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)

#Question 2
library(dslabs)
data('tissue_gene_expression')

ks <- seq(1, 11, 2)

F_1 <- sapply(ks, function(k){
  
  set.seed(1)
  
  #create training index (rather than test index) as suggested by comments
  train_index <- createDataPartition(tissue_gene_expression$y, p = 0.5, list = F)
  
  # split original data set into x and y
  x <- tissue_gene_expression$x
  y <- tissue_gene_expression$y
  
  # split x into train and test sets
  train_set_x = x[train_index,]
  test_set_x = x[-train_index,]
  
  # split y into train and test sets
  train_set_y = y[train_index]
  test_set_y = y[-train_index]
  
  # merge x and y train sets as a list (as per original data set)
  train_set = list('x' = train_set_x, 'y' = train_set_y)
  
  # merge x and y test sets
  test_set = list('x' = test_set_x, 'y' = test_set_y)
  
  fit <- knn3(y ~ ., data = (as.data.frame(train_set)), k = k)
  
  y_hat <- predict(fit, (as.data.frame(test_set)), type = "class") %>% factor(levels = levels(y))
  
  confusionMatrix(data=y_hat, reference=test_set_y)$overall["Accuracy"]
  
  }) 

plot(ks, F_1) 
max(F_1)
F_1
ks
