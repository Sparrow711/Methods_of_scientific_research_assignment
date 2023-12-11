#Setting directory and reading the file
setwd("C:/Users/Anshal/Desktop/stats")
data <- read.table("Question2_2.txt", header= TRUE)

library(dplyr)
library(tidyr)
library(caret)
library(caTools)

set.seed(123)
data$Convergence <- as.numeric(data$Convergence == "TRUE")
data$Architecture <- as.factor(data$Architecture)

sample_split <- sample.split(Y = data$Architecture, SplitRatio=0.7)

train_set <- subset(x = data, sample_split == TRUE)
test_set <- subset(x = data, sample_split == FALSE)

logistic <- glm(Convergence ~ ., data = train_set, family = "binomial")
summary(logistic)

probs <- predict(logistic, newdata = test_set, type = "response")
pred <- ifelse(probs > 0.5, 1, 0)

confusionMatrix(factor(pred), factor(test_set$Convergence), positive = as.character(1))

#################################
#Using the formula for the logistic regression on the following link:
#http://sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/
#####################################
levels <- levels(data$Architecture)
b_0 <- coef(logistic)[1]
b_complex <- coef(logistic)["Complexity"]

for (arch in levels) {
  if (arch == levels[1]) {  # If it's the baseline category
    complexity <- (0 - b_0) / b_complex
  } else {  
    b_Arch <- coef(logistic)[paste("Architecture", arch, sep="")]
    complexity <- (0 - b_0 - b_Arch) / b_complex
  }
  print(paste(arch, ":", complexity))
}

