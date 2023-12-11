####################################################
#For the code polynomial regression the following site was referred:
#https://www.geeksforgeeks.org/polynomial-regression-in-r-programming/
#####################################################
setwd("C:/Users/Anshal/Desktop/stats")
data <- read.table("Question3_4.txt", header= TRUE)
library(ggplot2)
library(tidyverse)
library(caret)

set.seed(123)
training.samples <- data$realism %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]


##########################
model <- lm(acceptability ~ poly(realism,degree =  2, raw = TRUE), data = train.data)
summary(model)

predictions <- model %>% 
  predict(test.data)

data.frame(RMSE = RMSE(predictions, test.data$acceptability),
           R2 = R2(predictions, test.data$acceptability))

ggplot(train.data, aes(realism, acceptability) ) + geom_point() + 
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))
