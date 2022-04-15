library(tidyverse)
library(lsa) # cosine()

# Question 1 
ac_bundles_dt <- read_csv('piccollage_accounts_bundles.csv')
ac_bundles_matrix <- as.matrix(ac_bundles_dt[, -1, with=FALSE])
rm(ac_bundles_dt)

# Question 1 (b-i)
top_5_recommend_cos <- function(ac_bundles_matrix){
  cos_matrix <- cosine(ac_bundles_matrix) # Obtain cosine similarity
  sorted_names_matrix <- c() # construct a empty matrix
  
  for (i in colnames(cos_matrix)){ # extract every column names
    temp_vector <- cos_matrix[,i] # extract a column of cos matrix
    # sort the similarities decreasingly
    temp_vector_sorted <- data.frame(sort(temp_vector, decreasing=TRUE)) 
    # the rownames are sorted according to the cosine similarity, too
    names_vector <- rownames(temp_vector_sorted) 
    # combine the result to get a full recommendation matrix 
    sorted_names_matrix <- cbind(sorted_names_matrix, names_vector) 
  }
  
  # assign the column names to the sorted names matrix
  colnames(sorted_names_matrix) <- colnames(cos_matrix)
  # We only want top 5 (omit each bundle itself)
  recommand_matrix <- sorted_names_matrix[2:6,]
  
  return(recommand_matrix)
}

recommand_matrix_cos <- top_5_recommend_cos(ac_bundles_matrix)

# Question 1 (b-ii)
mean_centering_col <- function(ac_bundles) {
  bundle_means <- apply(ac_bundles, 2, mean)
  bundle_means_matrix <- t(replicate(nrow(ac_bundles), bundle_means))
  # Subtract each row with its mean
  ac_bundles_mc_b <- ac_bundles - bundle_means_matrix 
  
  return(ac_bundles_mc_b)
}

ac_bundles_matrix_centered <- mean_centering_col(ac_bundles_matrix)
recommand_matrix_cor <- top_5_recommend_cos(ac_bundles_matrix_centered)
rm(ac_bundles_matrix_centered)

# Question 1 (b-iii)
mean_centering_row <- function(ac_bundles) {
  bundle_means <- apply(ac_bundles, 1, mean)
  bundle_means_matrix <- t(replicate(ncol(ac_bundles), bundle_means))
  # Subtract each row with its mean
  ac_bundles_mc_b <- ac_bundles - t(bundle_means_matrix) 
  
  return(ac_bundles_mc_b)
}

# for adjust cosine
ac_bundles_matrix_ad <- mean_centering_row(ac_bundles_matrix)
recommand_matrix_ad <- top_5_recommend_cos(ac_bundles_matrix_ad)
rm(ac_bundles_matrix_ad)

# Question 2 (g)
source("demo_simple_regression.R")
pts <- interactive_regression() # run the simulation and record the points
slope <- summary(lm(pts$y~pts$x)) # estimate the regression intercept and slope
cor_pts <- cor(pts) # estimate the correlation
pts_std <- scale(pts) # standardize
slope_std <- summary(lm(pts_std[,"y"]~pts_std[,"x"])) # regression slope
cor_pts_std <- cor(pts_std) # correlation
