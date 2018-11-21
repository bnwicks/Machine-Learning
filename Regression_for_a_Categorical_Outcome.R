### Regression for a Categorical Outcome
rm(list=ls())

library(dslabs)
data("heights")

y <- heights$height
set.seed(2)

test_index <- createDataPartition(y, times = 1, p =0.5, list = FALSE)

train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>%
  filter(round(height)==66) %>%
  summarise(mean(sex=="Female"))

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
  lm(y ~ height, data = .)

summary(lm_fit)
lm_fit

p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)

#Logistic Regression
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data = ., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
plot(test_set$height, p_hat)
plot(test_set$height, p_hat_logit)

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat_logit, test_set$sex)

#Case Study: 2 or 7
rm(list=ls()) # clear environment

library(dslabs)
data("mnist_27")

fit <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = "binomial")
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(data = y_hat, reference=mnist_27$test$y)

mnist_27$true_p %>% 
  ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("red", "white", "blue"))


#Comprehension Check: Logistic Regression
rm(list=ls()) # clear environment

set.seed(1)

make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# Q1 Answer
set.seed(1)

delta <- seq(0, 3, len = 25)

res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})

qplot(delta, res)

