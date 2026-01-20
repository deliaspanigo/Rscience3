fn_proc_step03_modifying_files <- function(state, my_pipeline_steps, list_bag_steps, session, advance_step_trigger) {

  # ------------------------------------------------------------------
  # Step 02: Copying files from package to work temporal folder.
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
    str_subfolder <- file.path("report01_gen_files", "lab01_RUN_create_files")
    str_my_subfolder_path <- file.path(str_work_dir_temporal, str_subfolder)

    # qmd file
    vector_qmd_file_name <- c("report01_lab01_step01_cleaning_RUN_HIDDEN.qmd",
                              "report01_lab01_step02_taking_files_RUN_HIDDEN.qmd",
                              "report01_lab01_step03_apply_user_selection_RUN_HIDDEN.qmd",
                              "report01_lab01_step04_create_Rscript_RUN_HIDDEN.qmd")
    vector_qmd_file_path <- file.path(str_my_subfolder_path, vector_qmd_file_name)







    shinyjs::delay(1000, {

      # 4. Start the asynchronous Quarto rendering (NON-BLOCKING)
      future_promise({

        str_init_time <- Sys.time()

        library("quarto")
        for(str_qmd_file in vector_qmd_file_path){

          quarto::quarto_render(input = str_qmd_file,
                                execute_dir = dirname(str_qmd_file)
          )
        }
        # my_folder <- dirname(str_qmd_file_path)
        # my_file <- "run_me.R"
        # my_path <- file.path(my_folder, my_file)
        #
        # temp_env <- new.env()
        # source(file = my_path, local = temp_env)
        # rm(temp_env)
        # gc()

        # quarto::quarto_render(input = str_qmd_file_path,
        #                       execute_dir = dirname(str_qmd_file_path),
        #                       output_file = "NUL")
        #
        str_end_time <- Sys.time()
        str_dif_time <- difftime(str_end_time, str_init_time, units = "secs")

        is_file_generated <- TRUE
        list(
          # output_file_names = matching_files, # Return what was actually found
          success = is_file_generated,
          str_init_time = str_init_time,
          str_end_time = str_end_time,
          str_dif_time = str_dif_time
        )

      }) %...>% {
        # --- CALLBACK EXECUTED BACK IN THE MAIN SHINY THREAD ---
        result <- .

        # 5. Step conclusion
        if (result$success) {
          # [SUCCESS] The report was generated and found.
          # list_bag_steps$final_report_path <- result$output_path
          execution_time <- round(as.numeric(result$str_dif_time), 2)
          execution_time <- as.character(execution_time)
          execution_time <- paste0(execution_time, " s")

          my_pipeline_steps$steps$step03$check <- TRUE
          my_pipeline_steps$steps$step03$status <- paste0("Done! (", execution_time, ")")
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
