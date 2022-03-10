# Question 1 (a)
rnormal <- rnorm(n=1000, mean=940, sd=190)
rnorm_std <- (rnormal - mean(rnormal))/sd(rnormal)
cat("mean of rnorm_std=", mean(rnorm_std), " sd=", sd(rnorm_std), "\n")

png(filename = "1a.png")
plot(density(rnorm_std), col="blue", lwd=2, main = "rnorm_std") # plot pdf
dev.off()

# Question 1 (b), Also partial code of Q3
bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]
hours <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
plot(density(minday), main="Minute (of the day) of first ever booking",
     col="blue", lwd=2)

minday_std <- (minday - mean(minday))/sd(minday)
cat("mean of minday_std=", mean(minday_std), " sd=", sd(minday_std), "\n")

png(filename = "1b_1.png")
plot(density(minday), col="blue", lwd=2, main = "minday") # plot pdf
dev.off()

png(filename = "1b_2.png")
plot(density(minday_std), col="blue", lwd=2, main = "minday_std") # plot pdf
dev.off()

# Question 2: Some functions we need

visualize_sample_ci <- function(num_samples = 100, sample_size = 100, 
                                pop_size=10000, distr_func=rnorm, ...) {
  # Simulate a large population
  population_data <- distr_func(pop_size, ...)
  pop_mean <- mean(population_data)
  pop_sd <- sd(population_data)
  
  # Simulate samples
  samples <- replicate(num_samples, 
                       sample(population_data, sample_size, replace=FALSE))
  
  # Calculate descriptives of samples
  sample_means = apply(samples, 2, FUN=mean)
  sample_stdevs = apply(samples, 2, FUN=sd)
  sample_stderrs <- sample_stdevs/sqrt(sample_size)
  ci95_low  <- sample_means - sample_stderrs*1.96
  ci95_high <- sample_means + sample_stderrs*1.96 
  ci99_low  <- sample_means - sample_stderrs*2.58
  ci99_high <- sample_means + sample_stderrs*2.58
  
  # Visualize confidence intervals of all samples
  plot(NULL, xlim=c(pop_mean-(pop_sd/2), pop_mean+(pop_sd/2)), 
       ylim=c(1,num_samples), ylab="Samples", xlab="Confidence Intervals")
  add_ci_segment(ci95_low, ci95_high, ci99_low, ci99_high,
                 sample_means, 1:num_samples, good=TRUE)
  
  # Visualize samples with CIs that don't include population mean
  bad = which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) |
                ((ci99_low > pop_mean) | (ci99_high < pop_mean)))
  add_ci_segment(ci95_low[bad], ci95_high[bad], ci99_low[bad], ci99_high[bad],
                 sample_means[bad], bad, good=FALSE)
  
  # Draw true population mean
  abline(v=mean(population_data))
}

add_ci_segment <- function(ci95_low, ci95_high, ci99_low, ci99_high, 
                           sample_means, indices, good=TRUE) {
  segment_colors <- list(c("lightcoral", "coral3", "coral4"),
                         c("lightskyblue", "skyblue3", "skyblue4"))
  color <- segment_colors[[as.integer(good)+1]]
  
  segments(ci99_low, indices, ci99_high, indices, lwd=3, col=color[1])
  segments(ci95_low, indices, ci95_high, indices, lwd=3, col=color[2])
  points(sample_means, indices, pch=18, cex=0.6, col=color[3])
}

# Question2 (a)
#png(filename = "2a.png")
#visualize_sample_ci(sample_size = 100, pop_size=10000,
#                    distr_func=rnorm, mean=20, sd=3)
#dev.off()

# Question2 (b)
#png(filename = "2b.png")
#visualize_sample_ci(sample_size = 300, pop_size=10000,
#                    distr_func=rnorm, mean=20, sd=3)
#dev.off()

# Question 2 (c)
#png(filename = "2c_1.png")
#visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000,
#                    distr_func=runif, min = -1, max = 1)
#dev.off()
#png(filename = "2c_2.png")
#visualize_sample_ci(num_samples = 100, sample_size = 300, pop_size=10000,
#                    distr_func=runif, min = -1, max = 1)
#dev.off()

# Question 3

compute_sample_mean <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  mean(resample)
}

plot_resample_density <- function(sample_i) {
  lines(density(sample_i), col=rgb(0.0, 0.4, 0.0, 0.01))
  return(mean(sample_i))
}

# Question 3 (a)
#(i)
ci95_trad <- mean(minday) + c(-1.96, 1.96)*sd(minday)
cat("mean of minday=", mean(minday), " sd of minday=", sd(minday), 
    " ci95=", ci95_trad, "\n")

#(ii)
resamples <- replicate(2000, sample(minday, length(minday), replace=TRUE))
sample_means <- apply(resamples, 2, FUN=plot_resample_density)

#(iii)
png(filename = "3a.png")
plot(density(sample_means), lwd=0, main="bootstrapped samples")
dev.off()

#(iv)
ci95_boot <- mean(resamples) + c(-1.96, 1.96)*sd(resamples)
cat("mean of resamples=", mean(resamples), " sd of resamples=", sd(resamples), 
    " ci95_boot=", ci95_boot, "\n")

# Question 3 (b)
# (i) (ii)
cat(" median of minday =", median(minday), "\n")
png(filename = "3b.png")
## Distribution of sampling
plot(density(resamples), lwd=2, xlim=c(0, 400))
## Confidence intervals of the sampling means
abline(v=median(resamples), lwd=2)
dev.off()

# (iii)
quantile(median(resamples), probs=c(0.025, 0.975))
