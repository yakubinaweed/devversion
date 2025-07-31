# R/serversecond/standard_z.R

# Function to perform Z-transformation (standardization)
# x: A numeric vector
z_transform <- function(x) {
  if (!is.numeric(x)) {
    stop("Input to z_transform must be a numeric vector.")
  }
  if (sd(x, na.rm = TRUE) == 0) {
    warning("Standard deviation is zero for z_transform; returning zeros for constant vector.")
    return(rep(0, length(x)))
  }
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}