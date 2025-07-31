# R/app.R

# Load necessary libraries (ensure these are installed: install.packages(c("shiny", "bslib", "refineR", "readxl", "moments", "shinyjs", "shinyWidgets", "shinyFiles", "dplyr", "tidyr", "mclust", "car", "ggplot2")))
library(shiny)
library(bslib)
library(refineR) # Ensure this package is installed
library(readxl)
library(moments) # For skewness calculation, used in GMM
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(dplyr) # For data manipulation (e.g., %>% and filter)
library(tidyr) # For data reshaping (e.g., pivot_wider)
library(mclust) # For GMM analysis
library(car) # Required for powerTransform (Yeo-Johnson)
library(ggplot2) # For plotting

# Source UI and Server logic from separate files
source("ui.R")
source("server.R")
source("utils.R") # General utility functions

# Run the app
shinyApp(ui = ui, server = server)