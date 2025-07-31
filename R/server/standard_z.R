# R/server/standard_z.R

# Function to perform Z-transformation (standardization)
# x: A numeric vector
z_transform_data <- function(df, value_col, ref_low, ref_high) {
  if (!value_col %in% colnames(df)) {
    warning("Value column not found for Z-transformation.")
    return(NULL)
  }
  values <- as.numeric(df[[value_col]])
  
  if (is.na(ref_low) || is.na(ref_high) || !is.numeric(ref_low) || !is.numeric(ref_high) || ref_low >= ref_high) {
    warning("Invalid reference limits provided for Z-transformation. Returning NULL.")
    return(NULL)
  }

  mean_ri <- (ref_low + ref_high) / 2
  sd_ri <- (ref_high - ref_low) / (2 * 1.96) # Assuming 95% CI (2 standard deviations)

  if (sd_ri == 0) {
    warning("Calculated standard deviation for RI is zero; cannot perform Z-transformation.")
    return(NULL)
  }

  z_scores <- (values - mean_ri) / sd_ri
  
  df$z_score <- z_scores
  return(df)
}