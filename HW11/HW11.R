# Question 1
cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
                     "acceleration", "model_year", "origin", "car_name")
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement),
                                  log(horsepower), log(weight), 
                                  log(acceleration), model_year, origin))

# Question 1 (a)
regr <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. + 
                      log.weight. + log.acceleration. + model_year + 
                      factor(origin),
                      data=cars_log, 
                      na.action=na.exclude)

# Question 1 (b)
regr_wt = lm(mpg ~ weight, data=cars, na.action=na.exclude)
regr_wt_log = lm(log.mpg. ~ log.weight., data=cars_log, na.action=na.exclude)

png(filename = "1b-1.png")
plot(density(regr_wt$residuals), 
     main="Residuals of weight",
     col="blue", lwd=2)
dev.off()

png(filename = "1b-2.png")
plot(density(regr_wt_log$residuals), 
     main="Residuals of log weight",
     col="red", lwd=2)
dev.off()

png(filename = "1b-3.png")
plot(cars$mpg, resid(regr_wt), 
     col="blue", main="Residuals vs weight", lwd=2)
abline(h=0)
dev.off()

png(filename = "1b-4.png")
plot(cars_log$log.mpg., resid(regr_wt_log), 
     col="red", main="Residuals of log weight", lwd=2)
abline(h=0)
dev.off()

# Question 1 (c)
# Function for single resampled regression line
boot_regr <- function(model, dataset) {
  boot_index <- sample(1:nrow(dataset), replace=TRUE)
  data_boot <- dataset[boot_index,]
  regr_boot <- lm(model, data=data_boot)
  regr_boot$coefficients
}
# Bootstrapping for confidence interval
coeffs <- replicate(300, boot_regr(log.mpg. ~ log.weight., cars_log))

# Confidence interval values
ci_m_weight <- quantile(coeffs["log.weight.",], c(0.025, 0.975))

# estimate of coefficient and its standard error
ci_m_weight_estimate <- confint(regr_wt_log)

# Question 2 (a)
regr_weight <- lm(log.weight. ~ log.cylinders. + log.displacement. + 
                  log.horsepower. + log.acceleration. + model_year +
                  factor(origin), data=cars_log)
r2_weight <- summary(regr_weight)$r.squared
vif_weight <- 1 / (1 - r2_weight)

# Question 2 (b)
library("car")
regr_log <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. +
                 log.weight. + log.acceleration. + model_year +
                 factor(origin), data=cars_log)
vif(regr_log)
regr_log <- lm(log.mpg. ~ log.cylinders. + log.horsepower. +
                 log.weight. + log.acceleration. + model_year +
                 factor(origin), data=cars_log)
vif(regr_log)
regr_log <- lm(log.mpg. ~ log.cylinders. + log.weight. + log.acceleration. + 
                 model_year + factor(origin), 
                 data=cars_log)
vif(regr_log)
regr_log <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year +
                 factor(origin), data=cars_log)
vif(regr_log)

# Question 3
png(filename = "3.png", width = 600, height = 600) # Subplots
origin_colors = c("blue", "darkgreen", "red")
with(cars_log, plot(log.weight., 
                    log.mpg., 
                    pch=origin, 
                    main = "mpg v.s. weight: different origins",
                    col=origin_colors[origin]))

cars_us <- subset(cars_log, origin==1)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us)
abline(wt_regr_us, col=origin_colors[1], lwd=2)
cars_us_2 <- subset(cars_log, origin==2)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us_2)
abline(wt_regr_us, col=origin_colors[2], lwd=2)
cars_us_3 <- subset(cars_log, origin==3)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us_3)
abline(wt_regr_us, col=origin_colors[3], lwd=2)
dev.off()
