# Question 2
# Read the data and get some basic statistics
library(tidyverse)
ver_time <- read_csv('verizon.csv')$Time # read file
ver_mean <- mean(ver_time) # mean of sample
ver_sd <- sd(ver_time)
ver_size <- length(ver_time)
hyp <- 7.6 # Null hypothesis

# 2(a) Recreate the traditional hypothesis test
t.test(ver_time, mu = hyp, alternative="greater", conf.level=0.99) # (i)
power.t.test(n=ver_size, 
             delta=ver_mean-hyp, 
             type="one.sample",
             sd=ver_sd, 
             sig.level=0.01,
             alternative="one.sided") # (ii)

# 2(b) bootstrap
bootstrap_null_alt <- function(sample0, hyp_mean) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  resample_se <- sd(resample) / sqrt(length(resample))
  t_stat_alt <- (mean(resample) - hyp_mean) / resample_se
  t_stat_null <- (mean(resample) - mean(sample0)) / resample_se
  c(t_stat_alt, t_stat_null)
}

# (i) original t_value
t_value <- 2.5608

# (ii) Bootstrap the null and alternative t-distributions
boot_t_stats <- replicate(2000, bootstrap_null_alt(ver_time, hyp))

# (iii) Find the 99% cutoff value
t_alt <- boot_t_stats[1,]
t_null <- boot_t_stats[2,]
ci_99 <- quantile(t_null, probs=c(0, 0.99)) # one tailed

# (iv) Compute the p-value and power of our bootstrapped test
null_probs <- ecdf(t_null)
one_tailed_pvalue <- 1 - null_probs(t_value)
alt_probs <- ecdf(t_alt)
one_tailed_power <- 1 - alt_probs(ci_99[2]) # one tailed
