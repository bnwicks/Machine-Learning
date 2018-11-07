#### Comprehension Check
# Confusion Matrix

# Librarys
library(dslabs)
library(dplyr)
library(lubridate)

# Data
data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# What is the propotion of females in class and online?

# Inclass
dat_inclass <- dat %>%
  filter(type == "inclass")

dat_inclass_male <- dat_inclass %>%
  filter(sex == "Male")

dat_inclass_female <- dat_inclass %>%
  filter(sex == "Female")

proportion_females_inclass <- nrow(dat_inclass_female) / nrow(dat_inclass)
proportion_females_inclass

proportion_males_inclass <- nrow(dat_inclass_male) / nrow(dat_inclass)
proportion_males_inclass

# Online
dat_online <- dat %>%
  filter(type == "online")

dat_online_male <- dat_online %>%
  filter(sex == "Male")

dat_online_female <- dat_online %>%
  filter(sex == "Female")

proportion_females_online <- nrow(dat_online_female) / nrow(dat_online)
proportion_females_online

# Prediction using type
y_hat <- ifelse(x == "inclass", "Female", "Male")
y_hat

#Check
mean(y == y_hat)

# Confusion Matrix
table(predicted = y_hat, actual = y)

# Sensitivity
confusionMatrix(data = factor(y_hat), reference = y)

