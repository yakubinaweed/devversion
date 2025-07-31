# R/serversecond/data_prep.R

# Function to apply universal plausibility limits
# This is a placeholder. You'll need to define your specific rules here.
# For example, filtering HGB values outside a physiologically plausible range.
apply_universal_plausibility_limits <- function(data_df) {
  # Example: Filter HGB to be between 3 and 15 g/dL (adjust as needed)
  # Example: Filter Age to be non-negative and within a reasonable max
  filtered_data <- data_df %>%
    dplyr::filter(HGB_raw >= 3 & HGB_raw <= 15) %>% # Using HGB_raw column name
    dplyr::filter(Age_raw >= 0 & Age_raw <= 120) # Using Age_raw column name

  if (nrow(filtered_data) < nrow(data_df)) {
    warning(paste(nrow(data_df) - nrow(filtered_data), "rows removed due to plausibility limits."))
  }
  return(filtered_data)
}