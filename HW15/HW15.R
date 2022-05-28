library("seminr")

# Question 1 (a)
sec <- read.csv("security_data_sem.csv")
sec_intxn_mm <- constructs(
  composite("TRUST", multi_items("TRST", 1:4)),
  composite("SEC", multi_items("PSEC", 1:4)),
  composite("REP", multi_items("PREP", 1:4)),
  composite("INV", multi_items("PINV", 1:3)),
  composite("POL", multi_items("PPSS", 1:3)),
  composite("FAML", single_item("FAML1")),
  interaction_term(iv="REP",moderator="POL",method=orthogonal)
)

# Structural Model
sec_intxn_sm <- relationships(
    paths(from = c("REP", "INV", "POL", "FAML", "REP*POL"), to = "SEC"),
    paths(from = "SEC", to = "TRUST")
)

# estimate the models using PLS
sec_pls <- estimate_pls(
  data = sec,
  measurement_model = sec_intxn_mm,
  structural_model = sec_intxn_sm
)

# Question 1 (b)
# (i) Plot a figure of the estimated model
save_plot(filename = "1b_plot.png",
          plot = plot(sec_pls), 
          width = 1060,
          height = 960)

# (ii) Weights and loadings of composites
sec_report <- summary(sec_pls)
write.table(round(sec_report$weights, digits=3), 
            file="1b_weights.csv", 
            sep = ",", 
            col.names=NA)
write.table(round(sec_report$loadings, digits=3), 
            file="1b_loadings.csv", 
            sep = ",", 
            col.names=NA)

# (iii) Regression coefficients of paths between factors
write.table(round(sec_report$paths, digits=3), 
            file="1b_paths.csv", 
            sep = ",", 
            col.names=NA)

# (iv) Bootstrapped path coefficients
boot_pls <- bootstrap_model(sec_pls , nboot = 1000) 
boot_report <- summary(boot_pls)
write.table(round(boot_report$bootstrapped_paths, digits=3), 
            file="1b_boot_paths.csv", 
            sep = ",", 
            col.names=NA)

# Question 2 (a)
sec_cf_mm <- constructs(
  reflective("TRUST", multi_items("TRST", 1:4)),
  reflective("SEC", multi_items("PSEC", 1:4)),
  reflective("REP", multi_items("PREP", 1:4)),
  reflective("INV", multi_items("PINV", 1:3)),
  reflective("POL", multi_items("PPSS", 1:3)),
  reflective("FAML", multi_items("FAML", 1:1)),
  interaction_term(iv="REP",moderator="POL",method=orthogonal)
)

sec_cf_pls <- estimate_pls(
  data = sec,
  measurement_model = sec_cf_mm,
  structural_model = sec_intxn_sm
)

# Question 2 (b)
# (i) Plot a figure of the estimated model
save_plot(filename = "2b_plot.png",
          plot = plot(sec_cf_pls), 
          width = 1060,
          height = 960)

# (ii) Loadings of composites
sec_cf_report <- summary(sec_cf_pls)
write.table(round(sec_cf_report$loadings, digits=3), 
            file="2b_loadings.csv", 
            sep = ",", 
            col.names=NA)

# (iii) Regression coefficients of paths between factors, and their p-values
write.table(round(sec_cf_report$paths, digits=3), 
            file="2b_paths.csv", 
            sep = ",", 
            col.names=NA)
