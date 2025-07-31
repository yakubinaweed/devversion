# R/server/output_renderers.R

# --- Centralized Message Display ---
render_app_message <- function(output, message_rv) {
  output$app_message <- renderUI({
    msg <- message_rv()
    if (msg$text == "") {
      return(NULL) # Don't display anything if no message
    }

    # Determine class based on message type for styling
    class_name <- switch(msg$type,
                         "error" = "alert alert-danger",
                         "success" = "alert alert-success",
                         "warning" = "alert alert-warning",
                         "info" = "alert alert-info",
                         "alert alert-secondary") # Default

    div(class = class_name, msg$text)
  })
}

# Helper to clear messages
clear_messages <- function(message_rv) {
  message_rv(list(type = "", text = ""))
}

# Helper to display analysis-related errors consistently
display_analysis_error <- function(output, message_rv, error_text) {
  message_rv(list(type = "error", text = error_text))
  output$result_text <- renderPrint({
    cat(error_text)
  })
  output$result_plot <- renderPlot(plot.new()) # Clear the plot
}
# --- End Centralized Message Display ---


# Function to render the text output of the refineR result
render_results_text <- function(output, result) {
  output$result_text <- renderPrint({
    print(result, RIperc = c(0.025, 0.975))
  })
  print("Text results rendered.")
}

# Function to render the plot output (for RefineR)
# Note: This function's name was previously in plotting.R, but is specific to RefineR.
# It is now moved here, to the 'server' folder.
plot_refiner_output <- function(df, value_col_name, age_col_name, gender_col_name, refiner_output, unit = "", input_low = NA, input_high = NA, z_data = NULL) {
  # This function expects 'refiner_output' to be the result from RefineR()
  # and will plot the density with estimated/user limits.

  if (is.null(df) || nrow(df) == 0 || is.null(refiner_output)) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data or RefineR output available for plot.", size = 6, color = "grey50"))
  }

  # Extract estimated intervals from refiner_output
  estimated_lower <- refiner_output$percentiles[1]
  estimated_upper <- refiner_output$percentiles[2]

  plot_title <- paste0("RefineR Estimated Reference Interval (", value_col_name, ")")

  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = value_col_name)) +
    ggplot2::geom_density(fill = "lightblue", alpha = 0.5) +
    ggplot2::labs(title = plot_title, x = paste0(value_col_name, " (", unit, ")"), y = "Density") +
    ggplot2::theme_minimal()

  # Add estimated reference limits
  p <- p +
    ggplot2::geom_vline(xintercept = estimated_lower, linetype = "dashed", color = "blue", size = 1) +
    ggplot2::geom_vline(xintercept = estimated_upper, linetype = "dashed", color = "blue", size = 1) +
    ggplot2::annotate("text", x = estimated_lower, y = Inf, label = paste0("L: ", round(estimated_lower, 2)), vjust = 1.5, hjust = -0.1, color = "blue", size = 4) +
    ggplot2::annotate("text", x = estimated_upper, y = Inf, label = paste0("U: ", round(estimated_upper, 2)), vjust = 1.5, hjust = 1.1, color = "blue", size = 4)

  # Add user-defined limits if provided and valid
  if (!is.na(input_low) && is.numeric(input_low)) {
    p <- p + ggplot2::geom_vline(xintercept = input_low, linetype = "dotted", color = "red", size = 1) +
             ggplot2::annotate("text", x = input_low, y = Inf, label = paste0("User L: ", round(input_low, 2)), vjust = 2.5, hjust = -0.1, color = "red", size = 4)
  }
  if (!is.na(input_high) && is.numeric(input_high)) {
    p <- p + ggplot2::geom_vline(xintercept = input_high, linetype = "dotted", color = "red", size = 1) +
             ggplot2::annotate("text", x = input_high, y = Inf, label = paste0("User U: ", round(input_high, 2)), vjust = 2.5, hjust = 1.1, color = "red", size = 4)
  }

  # Optionally, plot Z-scores if available
  if (!is.null(z_data) && "z_score" %in% colnames(z_data)) {
    # You might want a separate plot or a dual-axis plot for Z-scores
    # For simplicity, here's an example of how you might add them as points to the existing plot's X-axis
    # (This assumes z_score is on the same scale, which it isn't, so this is just illustrative)
    # A more common approach is a separate density plot for Z-scores.
    # For now, let's just make sure it's clear this is where z_data could be utilized for plotting.
    # print("DEBUG: Z-transformed data available for plotting, but not directly integrated into this density plot.")
  }

  p
}

# Function to save the plot to a file
save_plot_to_directory <- function(plot_data, output_dir, file_prefix) {
  if (is.null(output_dir) || !dir.exists(output_dir)) {
    warning("Output directory not selected or does not exist. Plot not saved.")
    return()
  }

  filename <- generate_safe_filename(file_prefix, output_dir, "png") # Assuming generate_safe_filename exists in utils.R
  
  plot_obj <- plot_refiner_output(
    df = plot_data$df,
    value_col_name = plot_data$value_col_name,
    age_col_name = plot_data$age_col_name,
    gender_col_name = plot_data$gender_col_name,
    refiner_output = plot_data$refiner_output,
    unit = plot_data$unit,
    input_low = plot_data$input_low,
    input_high = plot_data$input_high,
    z_data = plot_data$z_data
  )

  ggplot2::ggsave(filepath, plot = plot_obj, width = 10, height = 6, units = "in")
  print(paste("Plot saved as:", filepath))
}