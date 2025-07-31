# R/server/refiner.R

library(refineR) # Ensure refineR package is loaded in app.R

run_refiner_analysis <- function(df, value_col, nbootstrap_speed) {
  # Select the value column
  values <- df[[value_col]]
  
  # Remove NAs from values as RefineR does not handle them
  values <- na.omit(values)

  if (length(values) < 50) { # RefineR typically needs a decent number of observations
    stop("Not enough non-missing values for RefineR analysis (minimum 50 recommended).")
  }

  # Set nBootstraps based on speed choice
  nBootstraps_val <- switch(nbootstrap_speed,
                            "Fast" = 100, # Reduced for faster testing
                            "Medium" = 500,
                            "Slow" = 1000,
                            100) # Default to Fast

  print(paste("DEBUG: Running RefineR with nBootstraps =", nBootstraps_val))

  model <- tryCatch({
    RefineR(values, modelSelection = TRUE, nBootstraps = nBootstraps_val)
  }, error = function(e) {
    stop(paste("RefineR analysis failed:", e$message))
  })
  
  return(model)
}

extract_intervals <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }
  # Assuming 2.5 and 97.5 percentiles for RI
  ri <- tryCatch({
    getPercentileCI(model, percentiles = c(0.025, 0.975))
  }, error = function(e) {
    warning(paste("Could not extract intervals from RefineR model:", e$message))
    return(NULL)
  })
  return(ri)
}