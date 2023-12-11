setwd("C:/Users/Anshal/Desktop/stats")
data <- read.table("Question3_4.txt", header= TRUE)
library(ggplot2)

#Plot
ggplot(data, aes(x=realism, y=acceptability)) + geom_point()

#Correlation tests
cor(data$realism, data$acceptability, method = c("spearman"))
test_result2 <- cor.test(data$realism, data$acceptability, method = "spearman")
print(test_result2)
cor(data$realism, data$acceptability, method = c("kendall"))
test_result3 <- cor.test(data$realism, data$acceptability, method = "kendall")
print(test_result3)





