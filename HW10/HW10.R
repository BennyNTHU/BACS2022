# Question 2 (a)
salary <- read.csv("programmer_salaries.txt", sep="\t")
salary_regression <- lm(salary$Salary ~ 
                          salary$Experience +
                          salary$Score +
                          salary$Degree) # do linear regression
summary(salary_regression, data=salary)

# View the top 6 values
head(salary_regression$fitted.values)
head(salary_regression$residuals)

# Question 2 (b)
ones <- replicate(length(salary$Salary), 1) # create ones column vector
# Combine column vectors to a matrix
X <- cbind(ones, salary$Experience, salary$Score, salary$Degree)
y <- salary$Salary
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y # some linear algebra
y_hat <- X %*% beta_hat # predicted values
res <- y - y_hat # residuals
SSR <- sum((y_hat-mean(y))^2)
SSE <- sum((y - y_hat)^2)
SST <- SSR + SSE

# Question 2 (c)
source("demo_simple_regression_rsq.R")
points <- interactive_regression_rsq()
ones_c <- replicate(length(points[,1]), 1) # create ones
X_c <- cbind(ones_c, points[,1])
y_c <- points[,2]
beta_hat_c <- solve(t(X_c) %*% X_c) %*% t(X_c) %*% y_c
y_hat_c <- X_c %*% beta_hat_c
res_c <- y_c - y_hat_c
SSR_c <- sum((y_hat_c-mean(y_c))^2)
SSE_c <- sum((y_c - y_hat_c)^2)
SST_c <- SSR_c + SSE_c
R2_c_i <- SSR_c/SST_c
R2_c_ii <- cor(y_c, y_hat_c)^2

# Question 3
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
                 "acceleration", "model_year", "origin", "car_name")

# Question 3 (a-i)
png(filename = "3a.png", width = 600, height = 1200) # Subplots
par(mfrow=c(4,2))
cy <- plot(auto$cylinders, auto$mpg, # scatter
           main="cylinders v.s. mpg", 
           xlab="cylinders ", 
           ylab="mpg", 
           pch=19)
di <- plot(auto$displacement, auto$mpg, 
           main="displacement v.s. mpg", 
           xlab="displacement", 
           ylab="mpg", 
           pch=19)
ho <- plot(auto$horsepower, auto$mpg, 
           main="horsepower v.s. mpg", 
           xlab="horsepower", 
           ylab="mpg", 
           pch=19)
we <- plot(auto$weight, auto$mpg, 
           main="weight v.s. mpg", 
           xlab="weight", 
           ylab="mpg", 
           pch=19)
ac <- plot(auto$acceleration, auto$mpg, 
           main="acceleration v.s. mpg", 
           xlab="acceleration", 
           ylab="mpg", 
           pch=19)
my <- plot(auto$model_year, auto$mpg, 
           main="model_year v.s. mpg", 
           xlab="model_year", 
           ylab="mpg", 
           pch=19)
or <- plot(auto$origin, auto$mpg, 
           main="origin v.s. mpg", 
           xlab="origin", 
           ylab="mpg", 
           pch=19)
dev.off()

# Question 3 (a-ii)
cor_matrix <- cor(auto[,colnames(auto)!="car_name"], # drop column car_name
                  use="pairwise.complete.obs") # omit NA's
cor_matrix <- round(cor_matrix, digits=2)
write.table(cor_matrix, file="3a.csv")

# Question 3 (b)
auto_lr_model <- lm(mpg ~ cylinders+displacement+horsepower+
                 weight+acceleration+model_year+factor(origin), auto)
summary(auto_lr_model)

# Question 3 (c-i,ii)
auto_std <- data.frame(scale(auto[,colnames(auto)!="car_name"])) # Standardize
auto_lr_model_std <- lm(mpg ~ cylinders+displacement+horsepower+
                          weight+acceleration+
                          model_year, data=auto_std)
summary(auto_lr_model_std)

# Question 3 (c-iii)
png(filename = "3c-1.png")
plot(density(auto_lr_model$residuals), 
     main="Residuals of Regression",
     col="blue", lwd=2)
dev.off()

png(filename = "3c-2.png")
plot(density(auto_lr_model_std$residuals), 
     main="Residuals of Standardized Regression",
     col="red", lwd=2)
dev.off()

