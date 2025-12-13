#' Executes Step 07: Renders the XLSX Quarto document (report05_xlsx.qmd).
#'
#' @param state ReactiveValues containing the pipeline's current state.
#' @param my_pipeline_steps ReactiveValues containing step details.
#' @param list_bag_steps ReactiveValues containing data passed between steps.
#' @param set_default_quiet A non-reactive variable/constant indicating Quarto verbosity.
#' @param advance_step_trigger The reactiveVal used to force pipeline step advancement.

fn_proc_step07_quarto_report05 <- function(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger) {
  
  # ------------------------------------------------------------------
  # Step 07: Rendering the XLSX Quarto document.
  # ------------------------------------------------------------------
  observeEvent(state$current_step, {
    
    # 1. Requirements
    req(state$current_step == 7)
    req(!my_pipeline_steps$steps$step07$check)
    print("Step 07 - Init (Async)")
    
    # 2. State info - Update UI immediately
    state$message <- my_pipeline_steps$steps$step07$info_step
    state$progress <- state$current_step / state$max_step
    
    # 3. Setup paths in the main thread (Captura de valores)
    str_work_dir_temporal <- list_bag_steps$temp_work_folder_path
    str_temp_output_folder_path <- list_bag_steps$temp_output_folder_path
    
    str_subfolder <- "report05_xlsx_HERE"
    str_input_file_name <- "report05_xlsx.qmd"
    
    str_work_dir_new <- file.path(str_work_dir_temporal, str_subfolder)
    str_input_file_path <- file.path(str_work_dir_new, str_input_file_name)
    str_execute_dir <- dirname(str_input_file_path) # Directorio donde Quarto debe ejecutarse
    
    # 4. Start the asynchronous Quarto rendering (NON-BLOCKING)
    future_promise({
      
      # --- CÓDIGO EJECUTADO EN UN HILO SEPARADO ---
      
      # La operación pesada que bloqueaba la UI:
      quarto::quarto_render(
        input = str_input_file_path,
        execute = TRUE,
        quiet = set_default_quiet,
        execute_dir = str_execute_dir
      )
      
      # [VERIFICACIÓN POR PATRÓN]
      # Asumimos que el reporte generado se llama algo como 'zzz_output_report05...'
      pattern_to_check <- "^zzz_output_report05"
      # Consideramos que la extensión es .xlsx o .csv si se genera directamente
      
      # Listar archivos en el directorio de salida
      matching_files <- list.files(
        path = str_temp_output_folder_path,
        pattern = pattern_to_check,
        full.names = TRUE
      )
      
      is_file_generated <- length(matching_files) > 0
      
      # Retornar el resultado
      list(
        success = is_file_generated,
        output_path = if (is_file_generated) matching_files[1] else NULL
      )
      
    }) %...>% {
      # --- CALLBACK EN EL HILO PRINCIPAL DE SHINY ---
      result <- .
      
      # 5. Step conclusion
      if (result$success) {
        # Guardar path en list_bag_steps
        list_bag_steps$final_report_05_path <- result$output_path
        
        my_pipeline_steps$steps$step07$check <- TRUE
        my_pipeline_steps$steps$step07$status <- "Done! (Report 05 XLSX Generated)"
      } else {
        # Manejo de advertencia
        my_pipeline_steps$steps$step07$check <- TRUE
        my_pipeline_steps$steps$step07$status <- "Warning! (Output file missing)"
        state$message <- "WARNING: Quarto ran, but the XLSX output file was not found."
      }
      
      # [AVANCE DESACOPLADO] Avanzar al Step 08
      current_step_value <- state$current_step
      advance_step_trigger(current_step_value + 1)
      print("Step 07 - End (Async)")
      
    } %...!% {
      # --- MANEJO DE ERRORES (CRASH) ---
      error_message <- paste("Fatal Error in Step 07 (Quarto XLSX Rendering):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step07$status <- "Quarto Crash!"
    }
    
  }, ignoreInit = TRUE)
}