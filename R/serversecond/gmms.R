# R/serversecond/gmms.R (Complete Script)

# Ensure you have 'mclust' and 'moments' packages installed
# install.packages("mclust")
# install.packages("moments")
library(mclust)
library(moments) # Used for calculate_skewness

# Function to calculate skewness (requires 'moments' package) - now in utils.R, but still good to have here if standalone
# Moved this definition to utils.R
# calculate_skewness <- function(x) { ... }

# Function to run Gaussian Mixture Model (GMM)
# Expects data_for_gmm to already have 'HGB' and 'Age' columns (which should be z-standardized)
run_gmm <- function(data_for_gmm, G_range = 2:5) { # Default G_range
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' needed for GMM. Please install it: install.packages('mclust')")
  }

  # Ensure data_for_gmm is a data frame with columns named 'HGB' and 'Age'
  if (!("HGB" %in% colnames(data_for_gmm) && "Age" %in% colnames(data_for_gmm))) {
    stop("Input data for GMM must contain 'HGB' and 'Age' columns (likely z-transformed).")
  }

  # Select only numeric columns for clustering
  gmm_input_data <- data_for_gmm[, c("HGB", "Age")]

  # Run Mclust
  # Using various modelNames to allow Mclust to find the best fit based on BIC.
  multivariate_model_names <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "VVE", "VEV", "EVV", "VVV") # Common robust choices for 2D

  print(paste("DEBUG (gmms.R): Attempting Mclust fit with G range =", paste(G_range, collapse = ":"), " and modelNames =", paste(multivariate_model_names, collapse = ", ")))
  gmm_model <- tryCatch({
    mclust::Mclust(gmm_input_data, G = G_range, modelNames = multivariate_model_names)
  }, error = function(e) {
    stop(paste("GMM model fitting failed:", e$message))
  })

  if (is.null(gmm_model)) {
    stop("GMM model could not be fitted. Check data for issues (e.g., too few observations, constant values).")
  }

  print(paste("DEBUG (gmms.R): Mclust fit completed. Optimal G chosen:", gmm_model$G))
  print(paste("DEBUG (gmms.R): Optimal model chosen:", gmm_model$modelName))

  return(gmm_model)
}

# Function to assign clusters based on the model's classification
assign_clusters <- function(data_df, gmm_model) {
  print("DEBUG (gmms.R): assign_clusters function called.")
  if (is.null(gmm_model) || is.null(gmm_model$classification)) {
    warning("GMM model or classification is NULL. Cannot assign clusters.")
    return(data_df)
  }
  # It's crucial that 'data_df' (which is 'data_for_mclust' in server.R now)
  # aligns with the data used for Mclust. If it has NAs removed already, sizes should match.
  if (nrow(data_df) != length(gmm_model$classification)) {
    warning("Dataframe row count does not match GMM classification length. This might indicate a mismatch if data was filtered after Mclust or passed incorrectly.")
  }
  data_df$cluster <- gmm_model$classification
  print("DEBUG (gmms.R): Clusters assigned.")
  return(data_df)
}