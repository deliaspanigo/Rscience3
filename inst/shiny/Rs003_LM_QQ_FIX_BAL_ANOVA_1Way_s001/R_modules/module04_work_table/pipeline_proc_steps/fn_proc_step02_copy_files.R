fn_proc_step02_copy_files <- function(state, my_pipeline_steps, list_bag_steps, session, advance_step_trigger) {

  # ------------------------------------------------------------------
  # Step 02: Copying files from package to work temporal folder.
  # ------------------------------------------------------------------

  # 1. CAPTURE the Observer object
  obs <- observeEvent(state$current_step, {

    # 1. Requirements
    req(state$current_step == 2)
    req(!my_pipeline_steps$steps$step02$check)
    print("Step 02 - Init")

    # 2. State info - Update UI immediately
    state$message <- my_pipeline_steps$steps$step02$info_step
    state$progress <- state$current_step / state$max_step

    # Get necessary paths from the reactive storage
    # str_render_folder_path <- here::here("..", "RQ_02_render")
    str_render_folder_path <- file.path("RQ_02_render")
    print(str_render_folder_path)

    # NOTE: We use reactiveValuesToList to safely read the value in the main thread
    # before the future_promise starts.
    str_work_temporal_folder_path <- list_bag_steps$temp_work_folder_path

    # 🎯 NUEVO: DELAY DE 50ms ANTES DE EJECUTAR EL FUTURE_PROMISE
    shinyjs::delay(1000, {

      # 3. Start the asynchronous task (NON-BLOCKING)
      future_promise({

        str_init_time <- Sys.time()

        # --- BLOCK OF CODE EXECUTED IN A SEPARATE THREAD/PROCESS ---

        # Copy ALL content from source folder to destination
        fs::dir_copy(
          path = str_render_folder_path,
          new_path = str_work_temporal_folder_path,
          overwrite = TRUE
        )

        # Verification: Check how many files are in the destination (simple check)
        files_copied_count <- length(dir(str_work_temporal_folder_path, recursive = TRUE))

        str_end_time <- Sys.time()
        str_dif_time <- difftime(str_end_time, str_init_time, units = "secs")

        # Return the result and verification count
        list(
          success = TRUE, # fs::dir_copy usually doesn't fail unless path is bad
          count = files_copied_count,
          str_init_time = str_init_time,
          str_end_time = str_end_time,
          str_dif_time = str_dif_time
        )

      }) %...>% {
        # --- CALLBACK EXECUTED BACK IN THE MAIN SHINY THREAD ---
        result <- .

        # 4. Step conclusion
        if (result$success && result$count > 0) {
          # Update status and advance
          my_pipeline_steps$steps$step02$check <- TRUE

          execution_time <- round(as.numeric(result$str_dif_time), 2)
          execution_time <- as.character(execution_time)
          execution_time <- paste0(execution_time, " s")

          # my_pipeline_steps$steps$step02$status <- paste0("Done! (", result$count, " files copied)")
          my_pipeline_steps$steps$step02$status <- paste0("Done! (", execution_time, ")")

          # [KEY CHANGE] Use the trigger to advance to Step 03
          current_step_value <- state$current_step
          advance_step_trigger(current_step_value + 1)

          # 5. AUTODESTRUCTION: Eliminate the current observer
          obs$destroy()

        } else {
          # Handle failure (e.g., if the folder was empty or copy failed)
          my_pipeline_steps$steps$step02$status <- "Failed! (Files not copied or count is zero)."
          state$message <- "ERROR: File copy failed or source folder was empty."
          # NOTE: Obs remains active for potential re-try.
        }

        print("Step 02 - End")

      } %...!% {
        # --- ERROR HANDLING ---
        error_message <- paste("Fatal Error in Step 02 (File Copy Future):", .$message)
        warning(error_message)
        state$message <- error_message
        my_pipeline_steps$steps$step02$status <- "Fatal Error!"
        # NOTE: Obs remains active for potential re-try.
      }

    }) # 🎯 FIN DEL shinyjs::delay(50, ...)

  }, ignoreInit = TRUE)

  # Return the Observer object
  return(obs)
}
