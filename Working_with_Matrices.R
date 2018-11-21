### Working with Matrices  


## Question 1

#x <- matrix(rnorm(1000), 100, 100)
x <- matrix(rnorm(100*10), 100, 10)
#x <- matrix(rnorm(100*10), 10, 10)
#x <- matrix(rnorm(100*10), 10, 100)

# Question 2
dim(x)
nrow(x)
ncol(x)

# Question 3
x <- matrix(0, 100, 10)
x <- x + seq(nrow(x))

x <- matrix(0, 100, 10)
x <- 1:nrow(x)
x

x <- matrix(0, 100, 10)
x <- sweep(x, 2, 1:nrow(x),"+")
x

x <- matrix(0, 100, 10)
x <- sweep(x, 1, 1:nrow(x),"+")
x

# Question 4
x <- matrix(0, 100, 10)
x <- 1:ncol(x)
x

x <- matrix(0, 100, 10)
x <- 1:col(x)
x

x <- matrix(0, 100, 10)
x <- sweep(x, 2, 1:ncol(x), FUN = "+")
x

x <- matrix(0, 100, 10)
x <- -x
x

# Question 5
x <- matrix(0, 100, 10)
x <- 1:col(x)
x

mean(x)
rowMedians(x)
sapply(x,mean)
rowSums(x)
rowMeans(x)
colMeans(x)

# Question 6
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images
y <- mnist$train$labels

ind <- (x > 50 & x < 205)
ind <- rowMeans(ind)
mean(ind)


