# R/server.R (Main Server Logic)

# Load necessary libraries (already done in app.R, but useful for clarity if debugging this file alone)
library(shiny)
library(readxl)
library(shinyjs)
library(dplyr)
library(tidyr)
library(mclust) # For GMM analysis
library(moments) # For skewness calculation
library(car) # For Yeo-Johnson transform (powerTransform)
library(ggplot2) # For plotting

server <- function(input, output, session) {
  # Reactive values for overall app state
  analysis_running <- reactiveVal(FALSE) # Tracks if any analysis is running (Main or GMM)
  current_tab <- reactiveVal("Main Analysis") # Tracks the currently selected tab

  # Reactive values for Main Analysis tab
  data_reactive <- reactiveVal(NULL) # Stores uploaded raw data for Main Analysis
  selected_dir_reactive <- reactiveVal(NULL) # Stores selected directory for plot saving
  
  # Centralized message display reactive value
  message_rv <- reactiveVal(list(type = "", text = "")) # Stores list(type, text)


  # Reactive values for Subpopulation Detection (GMM) tab
  gmm_uploaded_data_rv <- reactiveVal(NULL) # Raw uploaded data for GMM tab
  gmm_processed_data_rv <- reactiveVal(NULL) # Transformed and clustered data for GMM tab (for plotting/summary)
  # Stores flags if Yeo-Johnson transformation was applied for plotting notes, and GMM models for summary
  gmm_transformation_details_rv <- reactiveVal(list(
    male_hgb_transformed = FALSE,
    female_hgb_transformed = FALSE,
    male_model = NULL, # Store male GMM model result here
    female_model = NULL # Store female GMM model result here
  ))


  # --- Source Modular Backend Files ---
  # Source files for the Main Analysis tab from 'server/' subfolder
  # These files contain logic specific to the Main Analysis tab.
  source("server/reactive_values.R", local = TRUE)
  source("server/data_observers.R", local = TRUE)
  source("server/file_observers.R", local = TRUE)
  source("server/analysis_observers.R", local = TRUE) # Contains the main 'analyze' button logic
  source("server/output_renderers.R", local = TRUE) # Contains functions to render main tab outputs
  # NOTE: R/server/refiner.R and R/server/standard_z.R are sourced directly by analysis_observers.R now.

  # Source files for the Subpopulation Detection (GMM) tab from 'serversecond/' subfolder
  # These files contain logic specific to the GMM tab.
  source("serversecond/gmms.R", local = TRUE) # GMM model fitting logic
  source("serversecond/plotting.R", local = TRUE) # GMM plotting functions
  source("serversecond/data_prep.R", local = TRUE) # Data preparation for GMM (e.g., plausibility limits)
  source("serversecond/standard_z.R", local = TRUE) # Generic Z-transform function (used by GMM)
  source("serversecond/yeo_johnson.R", local = TRUE) # Yeo-Johnson transform function

  source("utils.R", local = TRUE) # General utility functions


  # --- Shared/Centralized Logic ---
  # Centralized message display UI rendering
  render_app_message(output, message_rv)

  # Observer to prevent tab switching while any analysis is running
  observeEvent(input$tabs, {
    print(paste("DEBUG: Tab switch observed to:", input$tabs))
    if (analysis_running()) {
      message_rv(list(text = "Please wait, an analysis is currently running. Tab switching is disabled.", type = "warning"))
      print("DEBUG: Tab switch blocked by running analysis.")
    } else {
      current_tab(input$tabs) # Update current tab only if no analysis is running
      clear_messages(message_rv) # Clear messages on normal tab switch

      # Reset inputs/outputs of other tabs if switching away
      # Main Analysis tab reset logic (when switching AWAY from it)
      if (input$tabs != "Main Analysis") {
        # This prevents reactive expressions from trying to use stale data from Main Tab
        data_reactive(NULL)
        selected_dir_reactive(NULL)
        output$result_text <- renderPrint({ cat("") })
        output$result_plot <- renderPlot(plot.new())
        # Also reset main tab input selections visually
        updateSelectInput(session, "col_value", choices = c("None" = ""), selected = "")
        updateSelectInput(session, "col_age", choices = c("None" = ""), selected = "")
        updateSelectInput(session, "col_gender", choices = c("None" = ""), selected = "")
        updateRadioButtons(session, "gender_choice", selected = "Both")
        updateSliderInput(session, "age_range", value = c(0, 100))
        updateTextInput(session, "unit_input", value = "")
        updateNumericInput(session, "ref_low", value = NA)
        updateNumericInput(session, "ref_high", value = NA)
        updateCheckboxInput(session, "enable_directory", value = FALSE)
        shinyjs::reset("data_file") # Resets the file input visually
      }
      # GMM Tab reset logic (when switching AWAY from it)
      if (input$tabs != "Subpopulation Detection (GMM)") {
        # This prevents reactive expressions from trying to use stale data from GMM Tab
        gmm_uploaded_data_rv(NULL)
        gmm_processed_data_rv(NULL)
        gmm_transformation_details_rv(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE, male_model = NULL, female_model = NULL))
        # Clear GMM outputs (render empty content)
        output$gmm_plot <- renderPlot(plot.new())
        output$gmm_summary <- renderPrint({ cat("") })
        output$gmm_age_group_summary_output <- renderTable(NULL)
        # Also reset GMM input selections visually
        updateSelectInput(session, "gmm_col_value", choices = c("None" = ""), selected = "")
        updateSelectInput(session, "gmm_col_age", choices = c("None" = ""), selected = "")
        # Note: gmm_col_gender input is not present in UI, so no updateSelectInput for it.
        shinyjs::reset("gmm_data_file") # Resets the file input visually
      }
    }
  }, ignoreInit = TRUE)


  # --- Main Analysis Tab (RefineR) Logic ---
  # The UI elements for this tab are now directly defined in R/ui.R.
  # The observers for this tab are in `server/data_observers.R`, `server/file_observers.R`,
  # and the main logic is in `server/analysis_observers.R`.
  
  # The main analysis logic is driven by `call_analysis_observer`
  call_analysis_observer(input, output, session, data_reactive, selected_dir_reactive, analysis_running, message_rv)


  # Render Plot for Main Analysis Tab (Placeholder - actual rendering is done by analysis_observers.R)
  output$result_plot <- renderPlot({
    plot.new() # Just draw an empty plot initially
    text(0.5, 0.5, "Upload data and run analysis to see plot.", cex = 1.2, col = "grey50")
  })

  # Render Summary for Main Analysis Tab (Placeholder - actual rendering is done by analysis_observers.R)
  output$result_text <- renderPrint({
    "No analysis results to display yet."
  })

  # =========================================================================
  # Window 2: Subpopulation Detection (GMM) Logic - REVISED
  # =========================================================================

  # Observers for file upload and column selection for GMM tab (defined directly in server.R)
  observeEvent(input$gmm_data_file, {
    print("DEBUG: gmm_data_file observer triggered.")
    req(input$gmm_data_file)
    if (!analysis_running()) {
      tryCatch({
        print("DEBUG: Attempting to read GMM data.")
        data <- readxl::read_excel(input$gmm_data_file$datapath)
        gmm_uploaded_data_rv(data) # Stores raw data
        col_names <- colnames(data)
        updateSelectInput(session, "gmm_col_value", choices = c("None" = "", col_names), selected = "")
        updateSelectInput(session, "gmm_col_age", choices = c("None" = "", col_names), selected = "")
        # No gmm_col_gender input in UI, so no updateSelectInput for it.
        message_rv(list(text = "Data loaded for Subpopulation Detection! Please select HGB and Age columns.", type = "success"))
        print("DEBUG: GMM data loaded and selectors updated.")
      }, error = function(e) {
        message_rv(list(text = paste("Error loading data for GMM:", e$message), type = "danger"))
        print(paste("DEBUG ERROR: Error loading GMM data:", e$message))
      })
    } else {
      message_rv(list(text = "Cannot load data while Main Analysis is running. Please wait or reset.", type = "warning"))
      print("DEBUG: GMM data load blocked by main analysis.")
    }
  })

  output$gmm_col_value_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_col_value", "Select HGB Column:", choices = names(data),
                selected = c("HGB", "hgb", "HB", "hb", "Value", "value")[c("HGB", "hgb", "HB", "hb", "Value", "value") %in% names(data)][1])
  })

  output$gmm_col_age_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.is.null(data)) return(NULL)
    selectInput("gmm_col_age", "Select Age Column:", choices = names(data),
                selected = c("Age", "age", "leeftijd")[c("Age", "age", "leeftijd") %in% names(data)][1])
  })

  # Note: gmm_col_gender_selector is NOT present in UI per new requirement.
  # Gender column will be assumed as "Gender" or "Sex" in the internal processing.


  # Observer for GMM analysis button - REVISED LOGIC for flowchart implementation
  observeEvent(input$run_gmm_analysis, {
    print("DEBUG: run_gmm_analysis observer triggered.")
    if (input$tabs == "Subpopulation Detection (GMM)" && !analysis_running()) {
      req(gmm_uploaded_data_rv(), input$gmm_col_value, input$gmm_col_age)
      # No req for gmm_col_gender here, as it's not a direct input anymore.
      if (is.null(input$gmm_col_value) || input$gmm_col_value == "" || input$gmm_col_value == "None" ||
          is.null(input$gmm_col_age) || input$gmm_col_age == "" || input$gmm_col_age == "None") {
        message_rv(list(text = "Please select 'HGB Values' and 'Age' columns for subpopulation detection.", type = "warning"))
        print("DEBUG: GMM analysis: Missing HGB/Age column selection.")
        return()
      }

      analysis_running(TRUE)
      message_rv(list(text = "Detecting subpopulations...", type = "info"))
      print("DEBUG: Subpopulation detection started.")

      tryCatch({
        # --- Data Preparation as per Flowchart ---
        # Get raw uploaded data
        initial_gmm_data_raw <- gmm_uploaded_data_rv()

        # Try to identify gender column internally
        # Attempt to find common gender column names, default to NULL if not found
        gender_col_name_internal <- c("Gender", "gender", "Sex", "sex")[c("Gender", "gender", "Sex", "sex") %in% colnames(initial_gmm_data_raw)][1]
        
        # 1. Select relevant columns (HGB, Age, and potentially Gender for internal processing)
        # Ensure selected columns exist
        cols_to_select <- c(input$gmm_col_value, input$gmm_col_age)
        if (!is.na(gender_col_name_internal)) {
            cols_to_select <- c(cols_to_select, gender_col_name_internal)
        }
        
        # Select and rename for consistency in functions
        temp_initial_gmm_data <- initial_gmm_data_raw %>%
          dplyr::select(
            HGB_raw = !!sym(input$gmm_col_value),
            Age_raw = !!sym(input$gmm_col_age),
            # Conditionally select Gender_orig if column found
            Gender_orig = if (!is.na(gender_col_name_internal)) !!sym(gender_col_name_internal) else NULL
          ) %>%
          mutate(original_row_index = row_number()) %>% # Keep original index for rejoining if needed
          na.omit() # Remove rows with any NA in selected columns (HGB, Age, and Gender if present)


        if (nrow(temp_initial_gmm_data) == 0) {
          stop("No complete rows for GMM after initial NA removal. Check data or column selections.")
        }

        # 2. Apply Universal Plausibility Limits (from data_prep.R)
        cleaned_gmm_data <- apply_universal_plausibility_limits(temp_initial_gmm_data)
        if (nrow(cleaned_gmm_data) == 0) {
          stop("No data remains after applying universal plausibility limits.")
        }
        
        # 3. Normalize Gender column (only if Gender_orig column exists)
        if ("Gender_orig" %in% colnames(cleaned_gmm_data)) {
          cleaned_gmm_data <- cleaned_gmm_data %>%
            mutate(Gender = case_when( # Renamed Gender_norm to Gender for consistency with plot_age_hgb
              str_detect(Gender_orig, regex("male|m", ignore_case = TRUE)) ~ "Male",
              str_detect(Gender_orig, regex("female|f", ignore_case = TRUE)) ~ "Female",
              TRUE ~ "Other" # Categorize non-male/female as 'Other'
            )) %>%
            filter(Gender %in% c("Male", "Female")) # Only proceed with Male/Female for this analysis
        } else {
          # If no gender column was found/selected, treat all as one group (e.g., "Unknown")
          cleaned_gmm_data$Gender <- "Unknown"
        }

        if (nrow(cleaned_gmm_data) == 0) {
          stop("No Male or Female data remains after gender normalization and filtering (if gender column was present).")
        }

        # Store a copy of this pre-processed data with original values and labels for final display
        data_for_final_display_hgb_age <- cleaned_gmm_data %>%
            dplyr::select(HGB = HGB_raw, Age = Age_raw, Gender, original_row_index) %>%
            # Add age_group_label based on the raw age values for the summary table
            mutate(age_group_label = cut(Age,
                                       breaks = c(-Inf, 10, 30, 40, 100, Inf),
                                       labels = c("0-10 years", "10-30 years", "30-40 years", "40-100 years", "100+ years"),
                                       right = FALSE, include.lowest = TRUE))


        incProgress(0.1, detail = "Splitting data by gender and transforming...")

        male_data_for_gmm <- data_for_final_display_hgb_age %>% filter(Gender == "Male")
        female_data_for_gmm <- data_for_final_display_hgb_age %>% filter(Gender == "Female")
        
        # --- Process Male Data ---
        clustered_male_data <- tibble()
        male_hgb_transformed_flag <- FALSE
        male_gmm_model_result <- NULL # Initialize
        
        if (nrow(male_data_for_gmm) > 0) {
          # Apply conditional Yeo-Johnson to HGB for males (from yeo_johnson.R)
          yj_result_male <- apply_conditional_yeo_johnson(male_data_for_gmm$HGB)
          male_data_for_gmm$HGB_transformed <- yj_result_male$transformed_data
          male_hgb_transformed_flag <- yj_result_male$transformation_applied

          # Z-standardize HGB (transformed) and Age for males (from serversecond/standard_z.R)
          male_data_for_gmm$HGB_z <- z_transform(male_data_for_gmm$HGB_transformed)
          male_data_for_gmm$Age_z <- z_transform(male_data_for_gmm$Age)

          incProgress(0.2, detail = "Running GMM for Male data...")
          tryCatch({
            male_gmm_model_result <- run_gmm(male_data_for_gmm %>% dplyr::select(HGB = HGB_z, Age = Age_z))
            clustered_male_data <- assign_clusters(male_data_for_gmm, male_gmm_model_result)
            clustered_male_data$cluster <- as.factor(clustered_male_data$cluster)
            message_rv(list(text = "GMM for male data complete.", type = "success"))
          }, error = function(e) {
            warning(paste("Error running GMM for male data:", e$message))
            message_rv(list(text = paste("Error running GMM for male data:", e$message), type = "error"))
          })
        } else {
          message_rv(list(text = "No male data to process for GMM.", type = "info"))
        }

        # --- Process Female Data ---
        clustered_female_data <- tibble()
        female_hgb_transformed_flag <- FALSE
        female_gmm_model_result <- NULL # Initialize
        
        if (nrow(female_data_for_gmm) > 0) {
          # Apply conditional Yeo-Johnson to HGB for females (from yeo_johnson.R)
          yj_result_female <- apply_conditional_yeo_johnson(female_data_for_gmm$HGB)
          female_data_for_gmm$HGB_transformed <- yj_result_female$transformed_data
          female_hgb_transformed_flag <- yj_result_female$transformation_applied

          # Z-standardize HGB (transformed) and Age for females (from serversecond/standard_z.R)
          female_data_for_gmm$HGB_z <- z_transform(female_data_for_gmm$HGB_transformed)
          female_data_for_gmm$Age_z <- z_transform(female_data_for_gmm$Age)

          incProgress(0.2, detail = "Running GMM for Female data...")
          tryCatch({
            female_gmm_model_result <- run_gmm(female_data_for_gmm %>% dplyr::select(HGB = HGB_z, Age = Age_z))
            clustered_female_data <- assign_clusters(female_data_for_gmm, female_gmm_model_result)
            clustered_female_data$cluster <- as.factor(clustered_female_data$cluster)
            message_rv(list(text = "GMM for female data complete.", type = "success"))
          }, error = function(e) {
            warning(paste("Error running GMM for female data:", e$message))
            message_rv(list(text = paste("Error running GMM for female data:", e$message), type = "error"))
          })
        } else {
          message_rv(list(text = "No female data to process for GMM.", type = "info"))
        }

        # Combine results for plotting and summary
        combined_clustered_data_for_display <- bind_rows(clustered_male_data, clustered_female_data)
        
        if (nrow(combined_clustered_data_for_display) == 0) {
          stop("No data available for GMM plotting/summary after gender processing.")
        }
        
        gmm_processed_data_rv(combined_clustered_data_for_display)
        # Store models for summary calculation on original scale later
        gmm_transformation_details_rv(list(
          male_hgb_transformed = male_hgb_transformed_flag,
          female_hgb_transformed = female_hgb_transformed_flag,
          male_model = male_gmm_model_result,
          female_model = female_gmm_model_result
        ))

        incProgress(0.1, detail = "Generating plots and summaries...")
        message_rv(list(text = "GMM analysis complete!", type = "success"))

      }, error = function(e) {
        message_rv(list(text = paste("Error during GMM analysis:", e$message), type = "danger"))
        print(paste("DEBUG ERROR: Error during GMM analysis:", e$message))
        gmm_processed_data_rv(NULL) # Clear processed data on error
      }, finally = {
        analysis_running(FALSE)
        shinyjs::enable("gmm_results_tabs") # Re-enable tab switching
        print("DEBUG: GMM analysis finally block executed.")
      })
    } else if (input$tabs != "Subpopulation Detection (GMM)") {
      message_rv(list(text = "Please switch to the 'Subpopulation Detection (GMM)' tab to run this analysis.", type = "warning"))
      print("DEBUG: GMM analysis blocked: Not on correct Tab.")
    }
  })


  # Observer for resetting GMM tab
  observeEvent(input$reset_gmm_btn, {
    gmm_uploaded_data_rv(NULL)
    gmm_processed_data_rv(NULL)
    gmm_transformation_details_rv(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE, male_model = NULL, female_model = NULL))
    # Reset UI elements visually
    shinyjs::reset("gmm_data_file")
    updateSelectInput(session, "gmm_col_value", selected = "None")
    updateSelectInput(session, "gmm_col_age", selected = "None")
    # No gmm_col_gender input to reset
    message_rv(list(text = "GMM data and results reset.", type = "info"))
  })


  # Render GMM Plot (shared for male/female)
  output$gmm_plot <- renderPlot({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No GMM data available for plotting. Run analysis.", size = 6, color = "grey50"))
    }

    plot_age_hgb(plot_data,
                 male_hgb_transformed = gmm_transformation_details_rv()$male_hgb_transformed,
                 female_hgb_transformed = gmm_transformation_details_rv()$female_hgb_transformed
                 )
  })


  # Render GMM Summary (updated to handle gender-split summary)
  output$gmm_summary <- renderPrint({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return("No GMM analysis results to display.")
    }

    # Summarize for Male data
    male_summary_data <- plot_data %>% filter(Gender == "Male")
    male_model <- gmm_transformation_details_rv()$male_model
    
    if (nrow(male_summary_data) > 0 && !is.null(male_model)) {
      cat("--- GMM Analysis Summary (Male Subpopulations) ---\n")
      print(summary(male_model)) # Mclust's default summary (on transformed data)
      
      cat("\n--- Detailed Male Subpopulation Characteristics (Original Scale) ---\n")
      num_clusters_male <- male_model$G
      for (i in 1:num_clusters_male) {
        cat(paste0("Cluster ", i, ":\n"))
        cat(paste0("  Proportion (Size): ", round(male_model$parameters$pro[i], 3), "\n"))
        
        cluster_data_male <- male_summary_data %>% filter(cluster == i) # Data for this specific cluster on original scale
        
        mean_HGB_orig <- mean(cluster_data_male$HGB, na.rm = TRUE)
        sd_HGB_orig <- sd(cluster_data_male$HGB, na.rm = TRUE)
        mean_Age_orig <- mean(cluster_data_male$Age, na.rm = TRUE)
        sd_Age_orig <- sd(cluster_data_male$Age, na.rm = TRUE)

        cat(paste0("  Mean HGB: ", round(mean_HGB_orig, 3), "\n"))
        cat(paste0("  Mean Age: ", round(mean_Age_orig, 3), "\n"))
        
        cat(paste0("  Std Dev HGB: ", round(sd_HGB_orig, 3), "\n"))
        cat(paste0("  Std Dev Age: ", round(sd_Age_orig, 3), "\n"))
        
        if (!is.na(sd_Age_orig) && sd_Age_orig >= 0) {
          lower_age <- round(mean_Age_orig - 2 * sd_Age_orig, 1)
          upper_age <- round(mean_Age_orig + 2 * sd_Age_orig, 1)
          cat(paste0("  Estimated Age Range (Mean +/- 2SD): [", max(0, lower_age), " to ", upper_age, "] years\n"))
        } else {
          cat("  Estimated Age Range: N/A (Std Dev Age problematic)\n")
        }
        cat("\n")
      }
    } else {
      cat("No male subpopulations detected or GMM failed for male data.\n")
    }

    # Summarize for Female data
    female_summary_data <- plot_data %>% filter(Gender == "Female")
    female_model <- gmm_transformation_details_rv()$female_model
    
    if (nrow(female_summary_data) > 0 && !is.null(female_model)) {
      cat("\n--- GMM Analysis Summary (Female Subpopulations) ---\n")
      print(summary(female_model)) # Mclust's default summary (on transformed data)
      
      cat("\n--- Detailed Female Subpopulation Characteristics (Original Scale) ---\n")
      num_clusters_female <- female_model$G
      for (i in 1:num_clusters_female) {
        cat(paste0("Cluster ", i, ":\n"))
        cat(paste0("  Proportion (Size): ", round(female_model$parameters$pro[i], 3), "\n"))
        
        cluster_data_female <- female_summary_data %>% filter(cluster == i) # Data for this specific cluster on original scale

        mean_HGB_orig <- mean(cluster_data_female$HGB, na.rm = TRUE)
        sd_HGB_orig <- sd(cluster_data_female$HGB, na.rm = TRUE)
        mean_Age_orig <- mean(cluster_data_female$Age, na.rm = TRUE)
        sd_Age_orig <- sd(cluster_data_female$Age, na.rm = TRUE)

        cat(paste0("  Mean HGB: ", round(mean_HGB_orig, 3), "\n"))
        cat(paste0("  Mean Age: ", round(mean_Age_orig, 3), "\n"))
        
        cat(paste0("  Std Dev HGB: ", round(sd_HGB_orig, 3), "\n"))
        cat(paste0("  Std Dev Age: ", round(sd_Age_orig, 3), "\n"))
        
        if (!is.na(sd_Age_orig) && sd_Age_orig >= 0) {
          lower_age <- round(mean_Age_orig - 2 * sd_Age_orig, 1)
          upper_age <- round(mean_Age_orig + 2 * sd_Age_orig, 1)
          cat(paste0("  Estimated Age Range (Mean +/- 2SD): [", max(0, lower_age), " to ", upper_age, "] years\n"))
        } else {
          cat("  Estimated Age Range: N/A (Std Dev Age problematic)\n")
        }
        cat("\n")
      }
    } else {
      cat("No female subpopulations detected or GMM failed for female data.\n")
    }

    # Add a note about transformation if applied
    if (gmm_transformation_details_rv()$male_hgb_transformed || gmm_transformation_details_rv()$female_hgb_transformed) {
      cat("\nNote: HGB values were transformed (Yeo-Johnson) for GMM input due to skewness. Reported HGB values are original.\n")
    }
  })


  # Render GMM Cluster Age Group Summary (updated for gender)
  output$gmm_age_group_summary_output <- renderTable({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    # Predefined age bins for reporting (consistent with the ones in server.R's age_group_labeling)
    age_bins_cut <- c(-Inf, 10, 30, 40, 100, Inf)
    age_labels_cut <- c("0-10 years", "10-30 years", "30-40 years", "40-100 years", "100+ years")

    plot_data %>%
      mutate(age_group_label = cut(Age, breaks = age_bins_cut, labels = age_labels_cut, right = FALSE, include.lowest = TRUE)) %>%
      group_by(Gender, age_group_label, cluster) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = cluster, values_from = Count, values_fill = 0)
  }, rownames = FALSE)

} # End of server function