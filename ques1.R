#packages
library(bestNormalize)
library("ggplot2")
library(car)
library(tidyr)
library(dplyr)
library(rstatix)

#Setting directory and reading the file
setwd("C:/Users/Anshal/Desktop/stats")
data <- read.table("Question1_4.txt", header= TRUE)

long_data <- data %>% 
  gather(key = "Interface", value = "Time", interface1, interface2)

ggplot(long_data, aes(x = Time, fill = Interface)) +
    geom_histogram(aes(color = Interface), position = "dodge") +
    labs(x = "Time", y = "Frequency") +
    scale_fill_manual(values = c("#00AFBB", "#E7B800"))

ggplot(long_data, aes(x=Interface, y=Time)) + 
  geom_boxplot()

# Shapiro-Wilk normality test
shapiro.test(data$interface1)
shapiro.test(data$interface2)


#Normailization
bestNormalize(data$interface1, loo = TRUE)
w <- orderNorm(data$interface1)
data$interface1 <- predict(w)

bestNormalize(data$interface2, loo = TRUE)
v <- orderNorm(data$interface2)
data$interface2 <- predict(v)

#Testing normality again
shapiro.test(data$interface1)
shapiro.test(data$interface2)

#Homogeneity of Variance
long_data2 <- data %>% 
  gather(key = "Interface", value = "Time", interface1, interface2)
long_data2$Interface <- as.factor(long_data2$Interface)

leveneTest(Time ~ Interface, data = long_data2)

#t-test
t.test(Time ~ Interface, data = long_data, var.equal = TRUE)

