# Question 1 (b)
library("reshape")
page_loads <- read.csv(file="verizon_wide.csv") # read the file
verizon <-  melt(page_loads, 
                 na.rm = TRUE,
                 variable.name = "Type", 
                 value.name ="Time")
hosts <- split(x = verizon, f = verizon$variable)

# Question 1 (d)
png(filename = "1d.png")
plot(density(hosts$ILEC$value), col="cornflowerblue", lwd=2, xlim=c(0, 500))
lines(density(hosts$CLEC$value), col="coral3", lwd=2)
legend(300, 0.5, lty=1, c("ILEC", "CLEC"), col=c("coral3", "cornflowerblue"))
dev.off()

# Question 2 (b)
t.test(hosts$CLEC$value, hosts$ILEC$value, alt="greater", 
       var.equal=FALSE, conf.level=0.99) #(ii)
t.test(hosts$CLEC$value, hosts$ILEC$value, alt="greater", 
       var.equal=TRUE, conf.level=0.99) #(i)

# Question 2 (c)
observed_diff <- mean(hosts$CLEC$value) - mean(hosts$ILEC$value)

permute_diff <- function(values, groups) 
{
  permuted <- sample(values, replace = FALSE)
  grouped <- split(permuted, groups)
  permuted_diff <- mean(grouped[[1]]) - mean(grouped[[2]])
}
nperms <- 1000
permuted_diffs <- replicate(nperms, 
                            permute_diff(verizon$value, verizon$variable))

png(filename = "2c.png")
hist(permuted_diffs, breaks = "fd", probability = TRUE)
lines(density(permuted_diffs), lwd=2)
abline(v=observed_diff, lty="dashed")
dev.off()

p_1tailed <- sum(permuted_diffs > observed_diff) / nperms
p_2tailed <- sum(abs(permuted_diffs) > observed_diff) / nperms

# Question 3 (a)
gt_eq <- function(a, b) 
{
  ifelse(a > b, 1, 0) + ifelse(a == b, 0.5, 0)
}
W <- sum(outer(hosts$CLEC$value, hosts$ILEC$value, FUN = gt_eq))

# Question 3 (b)
n1 <- length(hosts$CLEC$value) # 23
n2 <- length(hosts$ILEC$value) # 1664
wilcox_p_1tail <- 1 - pwilcox(W, n1, n2)

# Question 3 (c)
wilcox.test(hosts$CLEC$value, 
            hosts$ILEC$value, 
            alternative = "greater",
            conf.level=0.01)

# Question 4 (a)
norm_qq_plot <- function(values)
{
  probs1000 <- seq(0, 1, 0.001)
  q_vals <- quantile(values, probs=probs1000)
  q_norm <- qnorm(probs1000, mean=mean(values), sd=sd(values))
  plot(q_norm, q_vals, xlab="normal quantiles", ylab="values quantiles")
  abline(a=0, b=1, col="red", lwd=2)
}

# Question 4 (b)
set.seed(48763)
d1 <- rnorm(n=500, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=45, sd=5)
d123 <- c(d1, d2, d3)

png(filename = "4b-1.png")
plot(density(d123))
dev.off()
png(filename = "4b-2.png")
norm_qq_plot(d123)
dev.off()

# Question 4 (c)
png(filename = "4c-CLEC-density.png")
plot(density(hosts$CLEC$value))
dev.off()

png(filename = "4c-CLEC-qq.png")
norm_qq_plot(hosts$CLEC$value)
dev.off()

png(filename = "4c-ILEC-density.png")
plot(density(hosts$ILEC$value))
dev.off()

png(filename = "4c-ILEC-qq.png")
norm_qq_plot(hosts$ILEC$value)
dev.off()
