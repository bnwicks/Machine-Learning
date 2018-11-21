## Nearest Neighbors  

set.seed(0)

if(!exists("mnist")) mnist <- read_mnist()

ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

# Distance Function
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))


# Cross Product
sqrt(crossprod(x_1-x_2))
sqrt(crossprod(x_1-x_3))
sqrt(crossprod(x_2-x_3))

# Distance
d <- dist(x)
class(d)
as.matrix(d)[1:3, 1:3]
image(as.matrix(d))
image(as.matrix(d)[order(y), order(y)])

d <- dist(t(x))
dim(as.matrix(d))

# Comprehension Check: Distance
#Question 1
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)
d
image(as.matrix(d))

#Question 2
# Distances
#x_1 <- as.matrix(d)[1:2]
#x_2 <- as.matrix(d)[39:40]
#x_3 <- as.matrix(d)[73:74]
#dist(x_1)
#dist(x_2)
#dist(x_3)
#x_a <- c(x_1, x_2, x_3)
d <- dist(tissue_gene_expression$x)
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
image(as.matrix(d))

      
