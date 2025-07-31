# R/serversecond/plotting.R (Complete Script)

library(ggplot2) # Ensure ggplot2 is loaded

# Function to plot HGB vs. Age with GMM clusters
# data_df: A data frame containing HGB, Age, Gender, and cluster columns.
# male_hgb_transformed: Logical, TRUE if male HGB was transformed for GMM.
# female_hgb_transformed: Logical, TRUE if female HGB was transformed for GMM.
plot_age_hgb <- function(data_df, male_hgb_transformed = FALSE, female_hgb_transformed = FALSE) {
  if (is.null(data_df) || nrow(data_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No GMM data available for plotting.", size = 6, color = "grey50"))
  }

  # Ensure cluster is a factor for discrete colors
  data_df$cluster <- as.factor(data_df$cluster)

  # Construct a dynamic plot title based on transformation status
  plot_title <- "HGB vs. Age with Detected Subpopulations"
  transformation_notes <- c()
  if (male_hgb_transformed) {
    transformation_notes <- c(transformation_notes, "Male HGB Yeo-Johnson transformed")
  }
  if (female_hgb_transformed) {
    transformation_notes <- c(transformation_notes, "Female HGB Yeo-Johnson transformed")
  }

  if (length(transformation_notes) > 0) {
    plot_title <- paste0(plot_title, "\n(Note: ", paste(transformation_notes, collapse = "; "), ")")
  }

  # Ensure 'Gender' column exists and is used if present, otherwise omit shape aesthetic
  if ("Gender" %in% colnames(data_df)) {
    p <- ggplot2::ggplot(data_df, ggplot2::aes(x = Age, y = HGB, color = cluster, shape = Gender))
  } else {
    p <- ggplot2::ggplot(data_df, ggplot2::aes(x = Age, y = HGB, color = cluster))
  }

  p <- p +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::labs(
      title = plot_title,
      x = "Age",
      y = "HGB (g/dL)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_brewer(palette = "Set1") + # Use a color palette for clusters
    ggplot2::theme(legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0.5)) # Center plot title

  p
}