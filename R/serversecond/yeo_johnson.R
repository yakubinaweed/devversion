# R/serversecond/yeo_johnson.R

# Requires the 'car' package for powerTransform
# install.packages("car")
library(car) # Used for powerTransform, loaded in app.R

# Function to apply conditional Yeo-Johnson transformation
# It calculates skewness and applies Yeo-Johnson if abs(skewness) > threshold.
apply_conditional_yeo_johnson <- function(data_vector, skewness_threshold = 0.5) {
  # Assuming moments::skewness is available (loaded in app.R)
  current_skewness <- moments::skewness(data_vector, na.rm = TRUE) 

  if (is.na(current_skewness) || length(data_vector[!is.na(data_vector)]) < 2) {
    warning("Skewness could not be calculated (e.g., all NAs or not enough data). No transformation applied.")
    return(list(transformed_data = data_vector, transformation_applied = FALSE))
  }

  if (abs(current_skewness) > skewness_threshold) {
    print(paste0("DEBUG: Applying Yeo-Johnson transformation. Skewness: ", round(current_skewness, 2)))
    transformed_data <- tryCatch({
      # Yeo-Johnson can handle zeros and negative values
      # powerTransform returns an object, we need the transformed data (its $y.t component)
      yj_transform_result <- car::powerTransform(data_vector, family = "yj")
      yj_transform_result$y.t
    }, error = function(e) {
      warning(paste("Error applying Yeo-Johnson transformation:", e$message, "No transformation applied."))
      return(data_vector) # Return original data if transformation fails
    })
    return(list(transformed_data = transformed_data, transformation_applied = TRUE))
  } else {
    return(list(transformed_data = data_vector, transformation_applied = FALSE))
  }
}