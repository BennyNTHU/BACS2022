# Question 1 (a)

# Three normally distributed data sets
d1 <- rnorm(n=100, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=500, mean=45, sd=5)

d123 <- c(d1, d2, d3) # Distribution 2

png(filename = "1a.png")
plot(density(d123), col="blue", lwd=2, main = "Distribution 2") # plot pdf
abline(v=mean(d123)) # add vertical lines
abline(v=median(d123), lty="dashed")
dev.off()
cat("mean=", mean(d123), " median=", median(d123), "\n")

# Question 1 (b)

d4 <- rnorm(n=800, mean=15, sd=5)
png(filename = "1b.png")
plot(density(d4), col="blue", lwd=2, main = "Distribution 3") # plot pdf
abline(v=mean(d4)) # add vertical lines
abline(v=median(d4), lty="dashed")
dev.off()
cat("mean=", mean(d4), " median=", median(d4), "\n")

# Question 2 (a)

rdata <- rnorm(n=2000, mean=0, sd=1)
png(filename = "2a.png")
plot(density(rdata), col="blue", lwd=2, main = "rdata") # plot pdf
abline(v=mean(rdata)) # add vertical lines
for(i in c(-3:3))
{
  abline(v=i*sd(rdata), lty="dashed")
}
dev.off()

# Question 2 (b)
q <- quantile(rdata)
q <- (q - mean(rdata))/sd(rdata)
print(q)

# Question 2 (c)
rdata <- rnorm(n=2000, mean=35, sd=3.5)
q <- quantile(rdata)
q <- (q - mean(rdata))/sd(rdata)
print(q)

# Question 2 (d)
q <- quantile(d123)
q <- (q - mean(d123))/sd(d123)
print(q)

# Question 3 (b)
rand_data <- rnorm(800, mean=20, sd = 5)

# Sturges' formula
k <- ceiling(log2(length(rand_data)))+1 
# Scott's normal reference rule
h1 <- 3.49 * sd(rand_data) / length(rand_data)^(1/3) 
# Freedman-Diaconis' choice
h2 <- 2 * IQR(rand_data) / length(rand_data)^(1/3)

cat("k=", k, ", h1=", h1, ", h2=", h2, "\n")

# Question 3 (c)
out_data <- c(rand_data, runif(10, min=40, max=60))

# Sturges' formula
k <- ceiling(log2(length(out_data)))+1 
# Scott's normal reference rule
h1 <- 3.49 * sd(out_data) / length(out_data)^(1/3) 
# Freedman-Diaconis' choice
h2 <- 2 * IQR(out_data) / length(out_data)^(1/3)

cat("k=", k, ", h1=", h1, ", h2=", h2, "\n")
