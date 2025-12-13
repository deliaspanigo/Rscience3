# pipeline_proc_steps/fn_proc_step02_copy_files.R

#' Executes Step 02: Copying necessary files from package source to the 
#' temporal working directory for Quarto rendering.
#'
#' @param state ReactiveValues containing the pipeline's current state (current_step, message, progress).
#' @param my_pipeline_steps ReactiveValues containing step details (check, status, info_step).
#' @param list_bag_steps ReactiveValues containing data passed between steps (temp_work_folder_path).
#' @param session Shiny session object.
#' @param advance_step_trigger The reactiveVal used to force pipeline step advancement. # <--- NUEVO ARGUMENTO

fn_proc_step02_copy_files <- function(state, my_pipeline_steps, list_bag_steps, session, advance_step_trigger) {
  
  # ------------------------------------------------------------------
  # Step 02: Copying files from package to work temporal folder.
  # ------------------------------------------------------------------
  observeEvent(state$current_step, {
    
    # 1. Requirements
    req(state$current_step == 2)
    req(!my_pipeline_steps$steps$step02$check)
    print("Step 02 - Init")
    
    # 2. State info - Update UI immediately
    state$message <- my_pipeline_steps$steps$step02$info_step
    state$progress<- state$current_step / state$max_step
    
    # Get necessary paths from the reactive storage
    str_render_folder_path <- here::here("..", "RQ_02_render")
    print(str_render_folder_path)
    
    # NOTE: We use reactiveValuesToList to safely read the value in the main thread
    # before the future_promise starts.
    str_work_temporal_folder_path <- list_bag_steps$temp_work_folder_path
    
    # 3. Start the asynchronous task (NON-BLOCKING)
    future_promise({
      
      # --- BLOCK OF CODE EXECUTED IN A SEPARATE THREAD/PROCESS ---
      
      # Copy ALL content from source folder to destination
      fs::dir_copy(
        path = str_render_folder_path,
        new_path = str_work_temporal_folder_path,
        overwrite = TRUE
      )
      
      # Verification: Check how many files are in the destination (simple check)
      files_copied_count <- length(dir(str_work_temporal_folder_path, recursive = TRUE))
      
      # Return the result and verification count
      list(
        success = TRUE, # fs::dir_copy usually doesn't fail unless path is bad
        count = files_copied_count
      )
      
    }) %...>% {
      # --- CALLBACK EXECUTED BACK IN THE MAIN SHINY THREAD ---
      result <- .
      
      # 4. Step conclusion
      if (result$success && result$count > 0) {
        # Update status and advance
        my_pipeline_steps$steps$step02$check <- TRUE
        my_pipeline_steps$steps$step02$status <- paste0("Done! (", result$count, " files copied)")
        
        # [CAMBIO CLAVE] Usar el trigger para avanzar al Step 03
        current_step_value <- state$current_step
        advance_step_trigger(current_step_value + 1)
        
      } else {
        # Handle failure (e.g., if the folder was empty or copy failed)
        my_pipeline_steps$steps$step02$status <- "Failed! (Files not copied or count is zero)."
        state$message <- "ERROR: File copy failed or source folder was empty."
      }
      
      print("Step 02 - End")
      
    } %...!% {
      # --- ERROR HANDLING ---
      error_message <- paste("Fatal Error in Step 02 (File Copy Future):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step02$status <- "Fatal Error!"
    }
    
  }, ignoreInit = TRUE)
}