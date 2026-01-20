fn_proc_step01_temp_work_folder_and_time <- function(state, my_pipeline_steps, list_bag_steps, the_timer, advance_step_trigger) {

  # Step 01 ------------------------------------------------------------------
  # Preparing work output temporal folder and sys time.
  # 1. CAPTURE the Observer object
  obs <- observeEvent(state$current_step, {

    # 1. Requirements
    req(state$current_step == 1)
    req(!my_pipeline_steps$steps$step01$check)
    print("Step 01 - Init")

    # 2. State info - Update the UI IMMEDIATELY to 'Running...' status
    state$message <- my_pipeline_steps$steps$step01$info_step
    state$progress <- state$current_step / state$max_step

    # 🎯 NUEVO: DELAY DE 50ms ANTES DE EJECUTAR EL FUTURE_PROMISE
    # Esto da tiempo a Shiny para actualizar la UI completamente
    shinyjs::delay(1000, {

      # 3. Start the asynchronous task (NON-BLOCKING UI)
      future_promise({

        # --- CODE BLOCK EXECUTED IN A SEPARATE THREAD/PROCESS ---

        # 4. Step actions...
        the_sys_time <- Sys.time()
        str_init_time <- the_sys_time
        # 4.2. Special format
        timestamp_format <- format(the_sys_time, "%Y%m%d_%H%M%S")

        # 4.3 Folder path
        # NOTE: fn_app_str_new_temporal_output_folder_path must be accessible/loaded in the global environment
        str_new_temp_folder_path <- fn_app_str_new_temporal_output_folder_path(timestamp_format = timestamp_format)

        # 4.4. Create new folder
        # Assuming str_output_folder_const is a constant loaded previously.
        str_output_folder_const <- "zzz_zzz_USER_OUPUT_FOLDER" # Replace with the actual value if constant

        dir.create(str_new_temp_folder_path, recursive = TRUE)

        # 4.5. Check folder existence
        check_new_temp_folder_path <- dir.exists(str_new_temp_folder_path)

        str_end_time <- Sys.time()
        str_dif_time <- difftime(str_end_time, str_init_time, units = "secs")

        # Return an object with all calculated data to be used in step 6.
        list(
          check = check_new_temp_folder_path,
          sys_time = the_sys_time,
          timestamp_format = timestamp_format,
          temp_work_folder_path = str_new_temp_folder_path,
          temp_output_folder_path = file.path(str_new_temp_folder_path, str_output_folder_const),
          str_init_time = str_init_time,
          str_end_time = str_end_time,
          str_dif_time = str_dif_time
        )

      }) %...>% {
        # --- CODE BLOCK EXECUTED BACK ON SHINY'S MAIN THREAD (Callback) ---
        result <- .

        # 5. Step conclusion (using the 'result' from the separate thread)
        if(result$check){
          # 1. Update reactiveValues
          list_bag_steps$sys_time <- result$sys_time
          list_bag_steps$timestamp_format <- result$timestamp_format
          list_bag_steps$temp_work_folder_path <- result$temp_work_folder_path
          list_bag_steps$temp_output_folder_path <- result$temp_output_folder_path

          # 2. Update the timer
          the_timer$init_time <- result$str_init_time
          the_timer$end_time  <- result$str_end_time
          the_timer$time_seg  <- result$str_dif_time # Correcto

          # 3. Mark the step as completed
          my_pipeline_steps$steps$step01$check <- TRUE

          # CORRECCIÓN AQUÍ: Cambia result$str_time_seg por result$str_dif_time
          # Usamos round() para que no salgan demasiados decimales
          execution_time <- round(as.numeric(result$str_dif_time), 2)
          execution_time <- as.character(execution_time)
          execution_time <- paste0(execution_time, " s")
          my_pipeline_steps$steps$step01$status <- paste0("Done! (", execution_time, ")")

          # [KEY CHANGE] Use the trigger to advance to Step 02
          current_step_value <- state$current_step
          advance_step_trigger(current_step_value + 1)

          # 6. AUTODESTRUCTION
          obs$destroy()
        } else {
          # Handle the failure (No advance on fatal error)
          my_pipeline_steps$steps$step01$status <- "Failed! Folder not created."
          state$message <- "ERROR: Could not create temporary work folder."
          # NOTE: Do NOT call obs$destroy() here, so it can be re-tried if 'check' is reset.
        }

        print("Step 01 - End")

      } %...!% {
        # --- ERROR HANDLING (If a fatal future error occurs) ---
        error_message <- paste("Unexpected error in Step 01 (Future):", .$message)
        warning(error_message)
        state$message <- error_message
        my_pipeline_steps$steps$step01$status <- "Fatal Error!"

        # NOTE: Do NOT call obs$destroy() here either, for re-try capability.
      }

    }) # 🎯 FIN DEL shinyjs::delay(50, ...)

  }, ignoreInit = TRUE)

  # Return the Observer object
  return(obs)
}
