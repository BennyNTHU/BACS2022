library(seminr)

# Question 1 (a)
sec =read.csv("security_data_sem.csv")
sec_intxn_mm <- constructs(
  composite("TRUST", multi_items("TRST", 1:4)),
  composite("SEC", multi_items("PSEC", 1:4)),
  composite("REP", multi_items("PREP", 1:4)),
  composite("INV", multi_items("PINV", 1:3)),
  composite("POL", multi_items("PPSS", 1:3)),
  composite("FAML", multi_items("FAML", 1:1)),
  interaction_term(iv="REP",moderator="POL",method=orthogonal),
)

# Structural Model
sec_intxn_sm <- relationships(
    paths(from = c("REP", "INV", "POL", "FAML", "REP*POL"), to = "SEC"),
    paths(from = "SEC", to = "TRUST")
)

# estimate the models using PLS
sec_pls <- estimate_pls(
  data = sec,
  measurement_model = sec_mm,
  structural_model = sec_sm
)

# Question 1 (b)
# (i) Plot a figure of the estimated model
png(filename = "1b.png") 
plot(sec_pls)
dev.off()
# (ii) Weights and loadings of composites
write.table(sec_pls$weights, file="1b_weights.csv", sep = ",", col.names=NA)
write.table(sec_pls$loadings, file="1b_loadings.csv", sep = ",", col.names=NA)
# (iii) Regression coefficients of paths between factors
sec_pls$paths 
# (iv) Bootstrapped path coefficients
boot_pls <- bootstrap_model(sec_pls , nboot = 1000) 
summary(boot_pls)














