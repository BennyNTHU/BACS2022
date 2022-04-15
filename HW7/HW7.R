library(reshape2)
library(tidyverse)
library(FSA)

# Question 1
data1 <- read_csv('pls-media1.csv')$INTEND.0 # read the data
data2 <- read_csv('pls-media2.csv')$INTEND.0
data3 <- read_csv('pls-media3.csv')$INTEND.0
data4 <- read_csv('pls-media4.csv')$INTEND.0

# Question 1 (a)
data1_mean <- mean(data1)
data2_mean <- mean(data2)
data3_mean <- mean(data3)
data4_mean <- mean(data4)

# Question 1 (b)
png(filename = "1b.png")
plot(density(data1), col="blue", lwd=2, xlim=c(0, 10), ylim=c(0,0.3))
lines(density(data2), col="red", lwd=2)
lines(density(data3), col="green", lwd=2)
lines(density(data4), col="purple", lwd=2)
abline(v=data1_mean, lty="dashed", col="blue")
abline(v=data2_mean, lty="dashed", col="red")
abline(v=data3_mean, lty="dashed", col="green")
abline(v=data4_mean, lty="dashed", col="purple")
legend(0, 0.25, 
       legend = c("pls-media1", "pls-media2", "pls-media3", "pls-media4"),
       col = c("blue","red","green","purple"),
       lty = 1:1, cex = 1)
dev.off()

# Question 2 (b)
# Compute MSTR
data <- list(data1, data2, data3, data4)
sstr <- sum(sapply(data, length)*(sapply(data, mean) - mean(sapply(data, mean)))^2)
df_mstr <- 4-1
mstr <- sstr/df_mstr 
# Compute MSE
sse <- sum((sapply(data, length)-1)*sapply(data, var))
df_mse <- sum(sapply(data, length)) - 4
mse <- sse/df_mse 
# Compute F-value and p-value
f_value <- mstr/mse # F-value
qf(p=0.95, df1=df_mstr, df2=df_mse) # The cutoff value of F-value
p_value <- pf(f_value, df_mstr, df_mse, lower.tail=FALSE) # The p-value

# Question 2 (c)
names(data) <- c("pls-media1", "pls-media2", "pls-media3", "pls-media4")
data_aov <- melt(data,
                 id.vars = NULL,
                 variable.name = "type",
                 value.name = "intend")
anova_model <- aov(data_aov$intend ~ factor(data_aov$L1))
summary(anova_model)

# Question 2 (d)
TukeyHSD(anova_model, conf.level = 0.05)

# Question 2 (e)
data1_sd <- sd(data1)
data2_sd <- sd(data2)
data3_sd <- sd(data3)
data4_sd <- sd(data4)
data_sd <- sd(data_aov$intend)

norm_qq_plot <- function(values)
{
  probs1000 <- seq(0, 1, 0.001)
  q_vals <- quantile(values, probs=probs1000)
  q_norm <- qnorm(probs1000, mean=mean(values), sd=sd(values))
  plot(q_norm, q_vals, xlab="normal quantiles", ylab="values quantiles")
  abline(a=0, b=1, col="red", lwd=2)
}

png(filename = "2e-type1.png")
norm_qq_plot(data1)
dev.off()

png(filename = "2e-type2.png")
norm_qq_plot(data2)
dev.off()

png(filename = "2e-type3.png")
norm_qq_plot(data3)
dev.off()

png(filename = "2e-type4.png")
norm_qq_plot(data4)
dev.off()

# Question 3 (b)
intend_rank <- rank(data_aov$intend)
data_aov$rank <- intend_rank # add as a new column to data_aov
group_ranks <- split(data_aov, data_aov$L1)
rank_sum_1 <- sum(group_ranks$`pls-media1`$rank)
rank_sum_2 <- sum(group_ranks$`pls-media2`$rank)
rank_sum_3 <- sum(group_ranks$`pls-media3`$rank)
rank_sum_4 <- sum(group_ranks$`pls-media4`$rank)
R <- c(rank_sum_1^2/length(data1), 
       rank_sum_2^2/length(data2),
       rank_sum_3^2/length(data3),
       rank_sum_4^2/length(data4))
H = (12/(length(data)*(length(data)+1))) * sum(R) - 3*(length(data)+1)
kw_p <- 1 - pchisq(H, df=4-1)

# Question 3 (c)
kruskal.test(intend~L1, data=data_aov)

# Question 3 (d)
dunnTest(intend~L1, data=data_aov, method = "bonferroni")
