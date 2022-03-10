# Question 2
# Read the data and get some basic statistics
library(tidyverse)
ver_time <- read_csv('verizon.csv')$Time # read file
ver_mean <- mean(ver_time) # mean of sample
ver_sd <- sd(ver_time)
ver_size <- length(ver_time)
hyp <- 7.6 # Null hypothesis

# Question 2 (a)
# (i) Visualize the distribution of Verizon¡¦s repair times
png(filename = "2a.png")
plot(density(ver_time), col="blue", lwd=2, main = "Time") # plot pdf
abline(v=mean(ver_time)) # add vertical lines
dev.off()

# (iii) Estimate the population mean, and the 99% CI
CI <- ver_mean + c(-2.58, 2.58)*ver_sd # CI

# (iv) t-statistic and p-value
se <- ver_sd/sqrt(ver_size) # standard error
t <- (ver_mean - hyp) / se # t-statistic
df <- ver_size - 1 # degree of freedom
p <- 1-pt(t,df) # p-value

# Question 2 (b)
# bootstrap settings
num_boots <- 2000 # Let's do 2000 times
set.seed(48763)

# (i) Estimate the bootstrapped 99% CI of the mean
sample_statistic <- function(stat_function, sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  stat_function(resample)
}

boot_means <- replicate(num_boots, sample_statistic(mean, ver_time))
boot_means_ci_99 <- quantile(boot_means, probs = c(0.005, 0.995)) # 99%CI

png(filename = "2b_1.png")
plot(density(boot_means), col="blue", lwd=2, main = "Bootstraped Means")
abline(v=mean(boot_means)) # add vertical lines on 99% CI
dev.off()

# (ii) Bootstrapped Difference of Means
boot_mean_diffs <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  return( mean(resample) - mean_hyp )
}

mean_diffs <- replicate(num_boots, boot_mean_diffs(ver_time, hyp))
diff_ci_99 <- quantile(mean_diffs, probs=c(0.005, 0.995)) # 99% CI

png(filename = "2b_2.png")
plot(density(mean_diffs), xlim=c(-1,3), main = "Mean Difference") # plot pdf
abline(v=diff_ci_99, lty="dashed") # add vertical lines on 99% CI
dev.off()

# (iii)
boot_t_stat <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  diff <- mean(resample) - mean_hyp
  se <- sd(resample)/sqrt(length(resample))
  return( diff / se )
}

t_boots <- replicate(num_boots, boot_t_stat(ver_time, hyp))
t_ci_99 <- quantile(t_boots, probs=c(0.005, 0.995)) # 99% CI

png(filename = "2b_3.png")
plot(density(t_boots), xlim=c(-2,7), main="bootstrapped t-statistic") # plot pdf
abline(v=t_ci_99, lty="dashed") # add vertical lines on 99% CI
dev.off()
