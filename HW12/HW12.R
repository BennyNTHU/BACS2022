cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
                 "acceleration", "model_year", "origin", "car_name")
keeps <- c("mpg", "weight", "acceleration", "model_year", "origin", "cylinders")
cars <- cars[keeps]
cars_log <- with(cars, data.frame(log(mpg), log(weight), log(acceleration), 
                                  model_year, origin, log(cylinders)))

# Question 1 (a)
weight_mean_log <- log(mean(cars$weight))
cars_light_log <- subset(cars_log, log.weight.<weight_mean_log) # light cars
cars_heavy_log <- subset(cars_log, log.weight.>=weight_mean_log) # heavy cars

regr_light <- with(cars_light_log, lm(log.mpg. ~ log.acceleration.))
regr_heavy <- with(cars_heavy_log, lm(log.mpg. ~ log.acceleration.))

png(filename = "1a.png", width = 600, height = 600) # Subplots
with(cars_light_log, plot(log.acceleration., log.mpg., pch=1, col="blue"))
with(cars_heavy_log, points(log.acceleration., log.mpg., pch=19, col="red"))
abline(regr_light, col="blue", lwd=2)
abline(regr_heavy, col="red", lwd=2)
legend(x = "topleft", legend = c("light", "heavy"), 
       col = c("blue","red"), lwd = 2)
dev.off()

# Question 1 (b)
regr_light_full <- lm(log.mpg.~ log.weight. + log.acceleration. + model_year + 
                      factor(origin), data=cars_light_log)
regr_heavy_full <- lm(log.mpg.~ log.weight. + log.acceleration. + model_year + 
                      factor(origin), data=cars_heavy_log)

# Question 2 (b-i)
regr_log_i <- lm(log.mpg.~ log.weight. + log.acceleration. + 
                   model_year + factor(origin), data=cars_log)

# Question 2 (b-ii)
regr_log_ii <- lm(log.mpg.~ log.weight. + log.acceleration. + 
                    model_year + factor(origin) + 
                    log.weight.*log.acceleration., data=cars_log)

# Question 2 (b-iii)
log_weight_mc <- scale(cars_log$log.mpg., 
                       center=TRUE, 
                       scale=FALSE)
log_acceleration_mc <- scale(cars_log$log.acceleration., 
                             center=TRUE, 
                             scale=FALSE)
regr_log_iii <- lm(log.mpg.~ log_weight_mc + log_acceleration_mc +
                     model_year + factor(origin) + 
                     log_weight_mc*log_acceleration_mc, data=cars_log)

# Question 2 (b-iv)
weight_x_acceleration <- cars_log$log.weight. * cars_log$log.acceleration.
interaction_regr <- lm(weight_x_acceleration ~ 
                         cars_log$log.weight. + cars_log$log.acceleration.)
interaction_ortho <- interaction_regr$residuals
regr_log_iv <- lm(log.mpg. ~ log.weight. + log.acceleration. + 
                    model_year + factor(origin) + interaction_ortho, 
                  data=cars_log)

# Question 2 (c)
cor_ii_w <- round(cor(cars_log$log.mpg., 
                      cars_log$log.mpg. * cars_log$log.acceleration.), 3)
cor_ii_a <- round(cor(cars_log$log.acceleration.,
                      cars_log$log.mpg. * cars_log$log.acceleration.), 3)
cor_iii_w <- round(cor(log_weight_mc, 
                       log_weight_mc*log_acceleration_mc), 3)
cor_iii_a <- round(cor(log_acceleration_mc, 
                       log_weight_mc*log_acceleration_mc), 3)
cor_iv_w <- round(cor(cars_log$log.weight., interaction_ortho), 3)
cor_iv_a <- round(cor(cars_log$log.acceleration., interaction_ortho), 3)

# Question 3 (a)
regr_3ai <- lm(log.weight.~ log.cylinders., data=cars_log)
regr_3aii <- lm(log.mpg.~ log.weight., data=cars_log)

# Question 3 (c)
set.seed(42) # Set random seed
boot_mediation <- function(model1, model2, dataset) { # bootstrap
  boot_index <- sample(1:nrow(dataset), replace=TRUE)
  data_boot <- dataset[boot_index, ]
  regr1 <- lm(model1, data_boot)
  regr2 <- lm(model2, data_boot)
  return(regr1$coefficients[2] * regr2$coefficients[2])
}

indirect <- replicate(2000, boot_mediation(regr_3ai, regr_3aii, cars_log))
boot_ci <- quantile(indirect, probs=c(0.025, 0.975))

png(filename = "3c.png", width = 600, height = 600) # Subplots
plot(density(indirect))
abline(v=quantile(indirect, probs=c(0.025, 0.975)))
dev.off()
