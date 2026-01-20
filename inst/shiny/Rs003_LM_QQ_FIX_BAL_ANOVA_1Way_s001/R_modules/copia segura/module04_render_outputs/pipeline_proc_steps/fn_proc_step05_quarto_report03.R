

fn_proc_step05_quarto_report03 <- function(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger) {

  # ------------------------------------------------------------------
  # Step 05: Rendering the PDF Quarto document.
  # ------------------------------------------------------------------

  # 1. CAPTURE the Observer object
  obs <- observeEvent(state$current_step, {

    # 1. Requirements
    req(state$current_step == 5)
    req(!my_pipeline_steps$steps$step05$check)
    print("Step 05 - Init (Async)")

    # 2. State info - Update UI immediately
    state$message <- my_pipeline_steps$steps$step05$info_step
    state$progress <- state$current_step / state$max_step

    # 3. Setup paths in the main thread (Value capture)
    str_work_dir_temporal <- list_bag_steps$temp_work_folder_path
    str_temp_output_folder_path <- list_bag_steps$temp_output_folder_path

    str_subfolder <- "report03_pdf_HERE"
    str_input_file_name <- "report03_pdf.qmd"

    str_work_dir_new <- file.path(str_work_dir_temporal, str_subfolder)
    str_input_file_path <- file.path(str_work_dir_new, str_input_file_name)
    str_execute_dir <- dirname(str_input_file_path) # Directory where Quarto should execute


    shinyjs::delay(1000, {


    # 4. Start the asynchronous Quarto rendering (NON-BLOCKING)
    future_promise({

      # --- CODE EXECUTED IN A SEPARATE THREAD ---

      # The heavy operation that blocked the UI:
      quarto::quarto_render(
        input = str_input_file_path,
        execute = TRUE,
        quiet = set_default_quiet,
        execute_dir = str_execute_dir
      )

      # [VERIFICATION BY PATTERN]
      # Assume the generated report is named something like 'zzz_output_report03...'
      pattern_to_check <- "^zzz_output_report03"

      # List files in the output directory
      matching_files <- list.files(
        path = str_temp_output_folder_path,
        pattern = pattern_to_check,
        full.names = TRUE
      )

      is_file_generated <- length(matching_files) > 0

      # Return the result
      list(
        success = is_file_generated,
        output_path = if (is_file_generated) matching_files[1] else NULL
      )

    }) %...>% {
      # --- CALLBACK ON THE MAIN SHINY THREAD ---
      result <- .

      # 5. Step conclusion
      if (result$success) {
        # (Optional: save path in list_bag_steps if needed later)
        list_bag_steps$final_report_03_path <- result$output_path

        my_pipeline_steps$steps$step05$check <- TRUE
        my_pipeline_steps$steps$step05$status <- "Done! (Report 03 PDF Generated)"
      } else {
        # Quarto may have finished but without the expected file.
        my_pipeline_steps$steps$step05$check <- TRUE # Mark as completed (advance)
        my_pipeline_steps$steps$step05$status <- "Warning! (Output file missing)"
        state$message <- "WARNING: Quarto ran, but the PDF output file was not found."
      }

      # [UNCOUPLED ADVANCE] Advance to Step 06
      current_step_value <- state$current_step
      advance_step_trigger(current_step_value + 1)

      # 6. AUTODESTRUCTION: Eliminate the current observer after advancing the pipeline.
      obs$destroy()

      print("Step 05 - End (Async)")

    } %...!% {
      # --- ERROR HANDLING (CRASH) ---
      error_message <- paste("Fatal Error in Step 05 (Quarto PDF Rendering):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step05$status <- "Quarto Crash!"
      # NOTE: Obs remains active for potential re-try.
    }

    }) # 🎯 FIN DEL shinyjs::delay(50, ...)


  }, ignoreInit = TRUE)

  # Return the Observer object
  return(obs)
}

