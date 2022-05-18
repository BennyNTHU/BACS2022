library(psych)

# Question 1 (a)
security <- read.csv("security_questions.csv") # read the dataset
sim_noise_ev <- function(n, p) # generate random noise
{ 
  noise <- data.frame(replicate(p, rnorm(n)))
  eigen(cor(noise))$values
}
evalues_noise <- replicate(100, sim_noise_ev(405, 18)) # The same size 
evalues_mean <- apply(evalues_noise, 1, mean)
sec_pca <- prcomp(security, scale. = TRUE) # Apply PCA on dataset

png(filename = "1a.png")
screeplot(sec_pca, type="lines")
lines(evalues_mean, type="b")
abline(h=1, lty="dotted")
dev.off()

# Question 2 (a)
sec_principal <- principal(security, nfactor=10, rotate="none", scores=TRUE)
sec_pc1 <- sec_principal$loadings[,"PC1"]
sec_pc2 <- sec_principal$loadings[,"PC2"]
sec_pc3 <- sec_principal$loadings[,"PC3"]
first_3_pc <- round(cbind(sec_pc1, sec_pc2, sec_pc3), digits=3)
names(first_3_pc) <- names(sec_principal)
write.table(first_3_pc, file="2a.csv", sep = ",", col.names=NA)

# Question 2 (b)
var1 <- sum(sec_principal$loadings[,"PC1"]^2)
var2 <- sum(sec_principal$loadings[,"PC2"]^2)
var3 <- sum(sec_principal$loadings[,"PC3"]^2)
total_var <- var1 + var2 + var3

# Question 3 (a)
sec_pca_org <- principal(security, # The original PCs
                         nfactor=3, 
                         rotate="none", 
                         scores=TRUE)
sec_pca_rot <- principal(security, # rotate the pcs
                         nfactor=3, 
                         rotate="varimax", 
                         scores=TRUE) # just call these two model

var1_org <- sum(sec_pca_org$loadings[,"PC1"]^2)
var2_org <- sum(sec_pca_org$loadings[,"PC2"]^2)
var3_org <- sum(sec_pca_org$loadings[,"PC3"]^2)
total_var_org <- var1_org + var2_org + var3_org

var1_rot <- sum(sec_pca_rot$loadings[,"RC1"]^2)
var2_rot <- sum(sec_pca_rot$loadings[,"RC2"]^2)
var3_rot <- sum(sec_pca_rot$loadings[,"RC3"]^2)
total_var_rot <- var1_rot + var2_rot + var3_rot

# Question 3 (d)
rot_pc1 <- sec_pca_rot$loadings[,"RC1"] # Rotated component's loading
rot_pc2 <- sec_pca_rot$loadings[,"RC2"]
rot_pc3 <- sec_pca_rot$loadings[,"RC3"]
loading_rot <- round(cbind(rot_pc1, rot_pc2, rot_pc3), digits=3)
names(loading_rot) <- c("RC1", "RC2", "RC3")
write.table(loading_rot, file="3d.csv", sep = ",", col.names=NA)

# Question 3 (e)
sec_pca_rot2 <- principal(security, # rotate the pcs
                          nfactor=2, 
                          rotate="varimax", 
                          scores=TRUE) # just call these two model

rot2_pc1 <- sec_pca_rot2$loadings[,"RC1"] # Rotated component's loading
rot2_pc2 <- sec_pca_rot2$loadings[,"RC2"]
loading_compare_2comp <- round(cbind(rot2_pc1, rot2_pc2), digits=3)
names(loading_compare_2comp) <- c("RC1", "RC2")
write.table(loading_compare_2comp, file="3e.csv", sep = ",", col.names=NA)
