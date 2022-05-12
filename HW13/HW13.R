# Question 1
cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
                 "acceleration", "model_year", "origin", "car_name")
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), 
                                  log(horsepower), log(weight), 
                                  log(acceleration), model_year, origin))
cars_log <- na.omit(cars_log)

# Question 1 (a-i)
mpg_all <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. + 
                log.weight. + log.acceleration. + model_year + factor(origin), 
              data=cars_log)
summary(mpg_all)
car::vif(mpg_all)

keeps <- c("log.cylinders.", "log.displacement.", "log.horsepower.", 
           "log.weight.")
cars_mc <- cars_log[keeps] # mc for multicollinearity

# Question 1 (a-ii)
cars_mc_eigen <- eigen(cor(cars_mc)) # eigenvalues
cars_mc_eigenvalues <- cars_mc_eigen$values
# The 1st eigenvalue is the proportion of variance of first PC
cars_mc_eigenvalues[1]

# Question 1 (a-iii)
cars_mc_eigenvectors <- cars_mc_eigen$vectors # PCA
cars_mc_eigenvectors[,1] # PC1

# Question 1 (b-i)
cars_mc_pca <- prcomp(cars_mc)
scores = cars_mc_pca$x
cars_log$composite_score <- scores[,"PC1"]

# Question 1 (b-ii)
pca_regr <- lm(log.mpg.~ composite_score + log.acceleration. + model_year + 
                        factor(origin), data=cars_log)

# Question 1 (b-iii)
pca_regr_std <- lm(log.mpg.~ scale(composite_score) + scale(log.acceleration.) + 
                     scale(model_year) + factor(origin), data=cars_log)

# Question 2 
security <- read.csv("security_questions.csv")
sec_eigen <- eigen(cor(security))
sec_pca <- prcomp(security, scale. = TRUE)
summary(sec_pca)

png(filename = "2.png")
screeplot(sec_pca, type="lines")
dev.off()
