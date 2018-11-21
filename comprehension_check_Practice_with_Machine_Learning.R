## Comprehension Check
# Practice with Machine Learning

# Library
library(caret)

# Data
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# Even split of the data into train and test partitions using createDataPartition
set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,] 

# iris_versicolor
iris_versicolor <- iris %>% filter(Species == "versicolor")
mean(iris_versicolor[["Petal.Length"]])
mean(iris_versicolor[,"Petal.Width"])
mean(iris_versicolor[["Sepal.Length"]])
mean(iris_versicolor[,"Sepal.Width"])

# iris_virginica
iris_virginica <- iris %>% filter(Species == "virginica")
mean(iris_virginica[["Petal.Length"]])
mean(iris_virginica[,"Petal.Width"])
mean(iris_virginica[["Sepal.Length"]])
mean(iris_virginica[,"Sepal.Width"])

# Investigating a Cutoff Values for training data
cutoff <- seq(3.0, 8.0, by = 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

plot(cutoff, accuracy)
best_cutoff <- 4.8

# Investigating a Cutoff Values for test data

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
  
mean(y_hat == test$Species)

# Investigating a multiple cutoff Values for test data
cutoff_pl <- seq(3.0, 8.0, by = 0.1)
cutoff_pw <- seq(0.0, 5.0, by = 0.1)

accuracy_pl <- 
    map_dbl(cutoff_pl, function(x){
      y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
        factor(levels = levels(train$Species))
      mean(y_hat == train$Species)
      })

plot(cutoff_pl, accuracy)

accuracy_pw <- 
  map_dbl(cutoff_pw, function(x){
    y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
      factor(levels = levels(train$Species))
    mean(y_hat == train$Species)
  })

plot(cutoff_pw, accuracy)

cutoff_pl <- 4.8 # optimal value
cutoff_pw <- 1.6 # optimal value
y_hat <- ifelse(test$Petal.Length > cutoff_pl | test$Petal.Width > cutoff_pw, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == train$Species)

# Final Solution Explaination
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
petalWidthRange <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs <- expand.grid(petalLengthRange,petalWidthRange)

id <- sapply(seq(nrow(cutoffs)),function(i){
  y_hat <- ifelse(train[,3]>cutoffs[i,1] | train[,4]>cutoffs[i,2],'virginica','versicolor')
  mean(y_hat==train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
y_hat <- ifelse(test[,3]>optimalCutoff[1] & test[,4]>optimalCutoff[2],'virginica','versicolor')
mean(y_hat==test$Species)

