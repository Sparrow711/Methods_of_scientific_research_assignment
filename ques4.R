#Part 1
# Load the necessary library
library(pwr)
set.seed(123)
effect_size <- 0.5  # Cohen's d
sig <- 0.05       # Significance level
power <- 0.8        # Desired power

sample_size <- pwr.t.test(d = effect_size, sig.level = sig, power = power)$n
print(sample_size)

##############################################################################
#Part2
n_samples <- 1000  
mean <- 0  
std_dev <- 1  
count <- 0
set.seed(451)

for (i in 1:n_samples) {
  # Generate Uniform distribution
  sample1 <- runif(sample_size, mean - std_dev, mean + std_dev)
  sample2 <- runif(sample_size, mean - std_dev, mean + std_dev)
  
  # Perform a t-test
  test_result <- t.test(sample1, sample2)
  
  # Check if the result is significant
  if (test_result$p.value < 0.05) {
    count <- count + 1
  }
}

type_1_error_rate <- count/1000

print(type_1_error_rate)

##############################################################################
#Part3
n_samples <- 1000  
mean <- 0  
std_dev <- 1  
#Formula for difference was obtained from the following link
#https://www.datanovia.com/en/lessons/t-test-effect-size-using-cohens-d-measure/
delta <- std_dev * 0.5  
sig_count <- 0

set.seed(324)

for (i in 1:n_samples) {
 
  sample3 <- runif(sample_size, mean - std_dev, mean + std_dev)
  sample4 <- runif(sample_size, mean + delta - std_dev, mean + delta + std_dev)
  
  test_result <- t.test(sample3, sample4)
  
  # Significance check
  if (test_result$p.value < 0.05) {
    sig_count <- sig_count + 1
  }
}

power <- sig_count
print(power)

################################################################################
#Part4
iterations <- 1000
mean <- 0  
std_dev <- 1 
significant_differences <- 0
set.seed(123)
for (i in 1:iterations) {
  # Generate two Cauchy distributed samples
  c1 <- tan(pi * (runif(sample_size, mean - std_dev, mean + std_dev) - 0.5))
  c2 <- tan(pi * runif(sample_size, mean - std_dev, mean + std_dev) - 0.5) + 1 
  
  test_result <- t.test(c1, c2)
  
  # Check if the result is significant
  if (test_result$p.value < 0.05) {
    significant_differences <- significant_differences + 1
  }
}

# Calculate Type I error rate
type_i_error_rate_cauchy <- significant_differences/1000
type_i_error_rate_cauchy

#######################################
num_simulations <- 1000
mean <- 0  
std_dev <- 1 
delta <- std_dev * 0.5 
significant_count <- 0
set.seed(678)


for (i in 1:num_simulations) {
  
  c3 <- tan(pi * ( runif(sample_size, mean - std_dev, mean + std_dev) - 0.5))
  c4 <- tan(pi * (runif(sample_size, mean + delta - std_dev, mean + delta + std_dev) - 0.5)) + 1

  test_result <- t.test(c3, c4, var.equal=FALSE)
  
  # Count significant result
  if (test_result$p.value < 0.05) {
    significant_count <- significant_count + 1
  }
}

power_cauchy <- significant_count
power_cauchy

##########################################################
set.seed(123)
mean <- 0  
std_dev <- 1 
num_simulations <- 1000
significant_count <- 0
delta <- std_dev * 0.5 

for (i in 1:num_simulations) {
  c5 <- tan(pi * ( runif(sample_size, mean - std_dev, mean + std_dev) - 0.5))
  c6 <- tan(pi * (runif(sample_size, mean + delta - std_dev, mean + delta + std_dev) - 0.5)) + 1

  # Perform the Wilcoxon rank sum test
  test_result <- wilcox.test(c5, c6)
  
  # Count significant result
  if (test_result$p.value < 0.05) {
    significant_count <- significant_count + 1
  }
}

power_wilcox <- significant_count
power_wilcox


