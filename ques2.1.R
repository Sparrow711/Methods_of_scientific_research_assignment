#Setting directory and reading the file
setwd("C:/Users/Anshal/Desktop/stats")
data <- read.table("Question2_2.txt", header= TRUE)

#Part 1
library(dplyr)
library(tidyr)

data$Convergence <- as.numeric(data$Convergence == "TRUE")
data$Architecture <- as.factor(data$Architecture)

converged <- data %>% 
  filter(Convergence == TRUE) %>%
  group_by(Architecture) %>%
  summarise(median = median(Complexity))

converged

