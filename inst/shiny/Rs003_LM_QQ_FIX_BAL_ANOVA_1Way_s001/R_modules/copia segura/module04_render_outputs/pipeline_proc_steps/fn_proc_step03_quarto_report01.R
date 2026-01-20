

fn_proc_step03_quarto_report01 <- function(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger) {

  # ------------------------------------------------------------------
  # Step 03: Rendering Quarto document.
  # ------------------------------------------------------------------

  # 1. CAPTURE the Observer object
  obs <- observeEvent(state$current_step, {

    # 1. Requirements
    req(state$current_step == 3)
    req(!my_pipeline_steps$steps$step03$check)
    print("Step 03 - Init")

    # 2. State info - Update UI immediately
    state$message <- my_pipeline_steps$steps$step03$info_step
    state$progress <- state$current_step / state$max_step

    # 3. Setup paths in the main thread
    str_work_dir_temporal <- list_bag_steps$temp_work_folder_path
    str_temp_output_folder_path <- list_bag_steps$temp_output_folder_path

    # Subfolder
    str_subfolder <- "report01_RQuarto_HERE"
    str_my_subfolder_path <- file.path(str_work_dir_temporal, str_subfolder)

    # Input files names
    str_input_file_name_original <- "report01_RQuarto_00_original.qmd"
    str_input_file_name_copy     <- stringr::str_replace(str_input_file_name_original, "00_original", "01_copy")
    str_input_file_name_anexo01  <- "report01_RQuarto_file01_sec13_Descriptive_RV.qmd"
    str_input_file_name_anexo02  <- "report01_RQuarto_file02_sec14_Descriptive_Residuals.qmd"

    # Input file paths
    str_input_file_path_original <- file.path(str_my_subfolder_path, str_input_file_name_original)
    str_input_file_path_copy     <- file.path(str_my_subfolder_path, str_input_file_name_copy)
    str_input_file_path_anexo01  <- file.path(str_my_subfolder_path, str_input_file_name_anexo01)
    str_input_file_path_anexo02  <- file.path(str_my_subfolder_path, str_input_file_name_anexo02)

    # The copy
    if (file.exists(str_input_file_path_copy)) file.remove(str_input_file_path_copy)
    file.copy(from = str_input_file_path_original, to = str_input_file_path_copy, overwrite = TRUE)

    # Modification on input file copy
    qmd_text <- readLines(str_input_file_path_copy, warn = FALSE)
    qmd_text <- gsub('_import_my_dataset_', 'get("mtcars")', qmd_text)
    qmd_text <- gsub('_var_name_rv_', '"mpg"', qmd_text)
    qmd_text <- gsub('_var_name_factor_', '"cyl"', qmd_text)
    qmd_text <- gsub('_alpha_value_', '0.05', qmd_text)
    qmd_text <- gsub('_vector_ordered_levels_', 'c("4", "6", "8")', qmd_text)
    qmd_text <- gsub('_vector_ordered_colors_', 'c("#FF0000", "#00FF00", "#0000FF")', qmd_text)
    writeLines(qmd_text, str_input_file_path_copy)

    # Output
    # library("yaml")
    # config <- yaml::read_yaml("_quarto.yml")
    # str_output_file_name <- config$format$html$"output-file"
    # str_output_file_path <- file.path(str_my_subfolder_path, str_output_file_name)


    str_execute_dir <- dirname(str_input_file_path_copy)

    shinyjs::delay(1000, {

    # 4. Start the asynchronous Quarto rendering (NON-BLOCKING)
    future_promise({

      # --- CODE BLOCK EXECUTED IN A SEPARATE THREAD ---

      quarto::quarto_render(
        input = str_input_file_path_copy,
        execute = TRUE,
        quiet = set_default_quiet,
        execute_dir = str_execute_dir
      )

      # [!!! KEY CHANGE: VERIFICATION BY PATTERN !!!]

      # Pattern to search for (files or directories starting with "zzz_output_report01")
      pattern_to_check <- "^zzz_output_report01"

      # List files/folders in the output directory that match the pattern
      # The str_temp_output_folder_path path is accessible here because it was captured
      # in the main thread before future_promise.
      matching_files <- list.files(
        path = str_temp_output_folder_path,
        pattern = pattern_to_check,
        full.names = TRUE # Include the full path
      )

      # Success is defined if AT LEAST one matching file/folder was found.
      is_file_generated <- length(matching_files) > 0

      list(
        output_file_names = matching_files, # Return what was actually found
        success = is_file_generated,
        output_path = if (is_file_generated) matching_files[1] else NULL # Use the first found file as the main path
      )

    }) %...>% {
      # --- CALLBACK EXECUTED BACK IN THE MAIN SHINY THREAD ---
      result <- .

      # 5. Step conclusion
      if (result$success) {
        # [SUCCESS] The report was generated and found.
        list_bag_steps$final_report_path <- result$output_path

        my_pipeline_steps$steps$step03$check <- TRUE
        my_pipeline_steps$steps$step03$status <- "Done! (Report Generated)"
        print("TODO OK!")

      } else {
        # [WARNING] Quarto ran, but verification failed.
        my_pipeline_steps$steps$step03$check <- TRUE # Mark as completed to advance
        my_pipeline_steps$steps$step03$status <- "Warning! (Output files missing)"
        state$message <- "WARNING: Quarto ran, but expected output files were not found."
        print("ERROR! (Files Missing, but continuing)")
      }

      # [!!! UNCONDITIONAL ADVANCE !!!]
      current_step_value <- state$current_step
      advance_step_trigger(current_step_value + 1)

      # 6. AUTODESTRUCTION: Eliminate the current observer after advancing the pipeline.
      obs$destroy()

      print("Step 03 - End")

    } %...!% {
      # --- ERROR HANDLING (CRASH) ---
      error_message <- paste("Fatal Error in Step 03 (Quarto Rendering):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step03$status <- "Quarto Crash!"
      # NOTE: Obs remains active for potential re-try.
    }

    }) # 🎯 FIN DEL shinyjs::delay(50, ...)

  }, ignoreInit = TRUE)

  # Return the Observer object
  return(obs)
}


