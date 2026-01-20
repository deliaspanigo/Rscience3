

fn_proc_step04_quarto_report02 <- function(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger) {

  # ------------------------------------------------------------------
  # Step 04: Rendering the second Quarto document.
  # ------------------------------------------------------------------

  # 1. CAPTURE the Observer object
  obs <- observeEvent(state$current_step, {

    # 1. Requirements
    req(state$current_step == 4)
    req(!my_pipeline_steps$steps$step04$check)
    print("Step 04 - Init")

    # 2. State info - Update UI immediately
    state$message   <- my_pipeline_steps$steps$step04$info_step
    state$progress  <- state$current_step / state$max_step

    # 3. Setup paths in the main thread (reading reactive values safely)
    str_work_dir_temporal <- list_bag_steps$temp_work_folder_path
    # Capture the TEMPORARY output path for verification
    str_temp_output_folder_path <- list_bag_steps$temp_output_folder_path

    str_subfolder <- "report02_R_script_HERE"
    str_input_file_name <- "report02_R.qmd"

    str_work_dir_new <- file.path(str_work_dir_temporal, str_subfolder)
    str_input_file_path <- file.path(str_work_dir_new, str_input_file_name)
    str_execute_dir <- dirname(str_input_file_path)


    shinyjs::delay(1000, {


    # 4. Start the asynchronous Quarto rendering (NON-BLOCKING)
    future_promise({

      # --- CODE BLOCK EXECUTED IN A SEPARATE THREAD ---

      # Execute Quarto Rendering. Use execute_dir.
      quarto::quarto_render(
        input = str_input_file_path,
        execute = TRUE,
        quiet = set_default_quiet,
        execute_dir = str_execute_dir
      )

      # [!!! KEY CHANGE: VERIFICATION BY PATTERN !!!]

      # Pattern to search for Step 04 (files or directories starting with "zzz_output_report02")
      pattern_to_check <- "^zzz_output_report02"

      # List files/folders in the output directory that match the pattern
      matching_files <- list.files(
        path = str_temp_output_folder_path,
        pattern = pattern_to_check,
        full.names = TRUE # Include the full path
      )

      # Success is defined if AT LEAST one matching file/folder was found.
      is_file_generated <- length(matching_files) > 0

      # Return the result
      list(
        output_file_names = matching_files,
        success = is_file_generated,
        output_path = if (is_file_generated) matching_files[1] else NULL
      )

    }) %...>% {
      # --- CALLBACK EXECUTED BACK IN THE MAIN SHINY THREAD ---
      result <- .

      # 5. Step conclusion
      if (result$success) {
        # [SUCCESS] The report was generated and found.
        list_bag_steps$final_report_02_path <- result$output_path

        my_pipeline_steps$steps$step04$check <- TRUE
        my_pipeline_steps$steps$step04$status <- "Done! (Report 02 Generated)"
        print("Step 04: TODO OK!")

      } else {
        # [WARNING] Quarto ran, but verification failed.
        my_pipeline_steps$steps$step04$check <- TRUE # Mark as completed to advance
        my_pipeline_steps$steps$step04$status <- "Warning! (Output files missing)"
        state$message <- "WARNING: Quarto ran, but expected output files (zzz_output_report02*) were not found."
        print("Step 04: ERROR! (Files Missing, but continuing)")
      }

      # [!!! UNCONDITIONAL ADVANCE !!!]
      # If we reach here, Quarto did not crash. Therefore, advance to Step 05.
      current_step_value <- state$current_step
      advance_step_trigger(current_step_value + 1) # Sets 5

      # 6. AUTODESTRUCTION: Eliminate the current observer after advancing the pipeline.
      obs$destroy()

      print("Step 04 - End")

    } %...!% {
      # --- ERROR HANDLING (CRASH) ---
      error_message <- paste("Fatal Error in Step 04 (Quarto Rendering):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step04$status <- "Quarto Crash!"
      # The pipeline stops here. Obs remains active for potential re-try.
    }

  }) # 🎯 FIN DEL shinyjs::delay(50, ...)


  }, ignoreInit = TRUE)

  # Return the Observer object
  return(obs)
}

