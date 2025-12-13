#' Executes Step 09: Renders the Final HTML Quarto document (report99_Rscience_Report_00.qmd).
#'
#' @param state ReactiveValues containing the pipeline's current state.
#' @param my_pipeline_steps ReactiveValues containing step details.
#' @param list_bag_steps ReactiveValues containing data passed between steps.
#' @param set_default_quiet A non-reactive variable/constant indicating Quarto verbosity.
#' @param advance_step_trigger The reactiveVal used to force pipeline step advancement.
#' @param the_timer ReactiveValues containing time tracking info.

fn_proc_step09_quarto_report99 <- function(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger, the_timer) {
  
  # ------------------------------------------------------------------
  # Step 09: Rendering the Final HTML Quarto document.
  # ------------------------------------------------------------------
  observeEvent(state$current_step, {
    
    # 1. Requirements
    req(state$current_step == 9)
    req(!my_pipeline_steps$steps$step09$check)
    print("Step 09 - Init (Async)")
    
    # 2. State info - Update UI immediately
    state$message <- my_pipeline_steps$steps$step09$info_step
    state$progress <- state$current_step / state$max_step
    
    # 3. Setup paths in the main thread (Captura de valores)
    str_work_dir_temporal <- list_bag_steps$temp_work_folder_path
    str_temp_output_folder_path <- list_bag_steps$temp_output_folder_path
    
    str_subfolder <- "report99_Rscience_Report_HERE"
    str_input_file_name <- "report99_Rscience_Report_00.qmd"
    
    str_work_dir_new <- file.path(str_work_dir_temporal, str_subfolder)
    str_input_file_path <- file.path(str_work_dir_new, str_input_file_name)
    str_execute_dir <- dirname(str_input_file_path) # Directorio donde Quarto debe ejecutarse
    
    # 4. Start the asynchronous Quarto rendering (NON-BLOCKING)
    future_promise({
      
      # --- CÓDIGO EJECUTADO EN UN HILO SEPARADO ---
      
      # La operación pesada final:
      quarto::quarto_render(
        input = str_input_file_path,
        execute = TRUE,
        quiet = set_default_quiet,
        execute_dir = str_execute_dir
      )
      
      # [VERIFICACIÓN POR PATRÓN]
      # Asumimos que el reporte generado se llama algo como 'zzz_output_report99...'
      pattern_to_check <- "^zzz_output_report99"
      
      # Listar archivos en el directorio de salida
      matching_files <- list.files(
        path = str_temp_output_folder_path,
        pattern = pattern_to_check,
        full.names = TRUE
      )
      
      is_file_generated <- length(matching_files) > 0
      
      # Calcular el tiempo final
      final_time <- Sys.time()
      
      # Retornar el resultado
      list(
        success = is_file_generated,
        output_path = if (is_file_generated) matching_files[1] else NULL,
        final_time = final_time
      )
      
    }) %...>% {
      # --- CALLBACK EN EL HILO PRINCIPAL DE SHINY ---
      result <- .
      
      # 5. Step conclusion
      if (result$success) {
        # Guardar path final
        list_bag_steps$final_report_99_path <- result$output_path
        
        # Actualizar el temporizador (Tiempo final)
        the_timer$final_time <- result$final_time
        total_time_elapsed <- difftime(the_timer$final_time, the_timer$init_time, units = "secs")
        
        my_pipeline_steps$steps$step09$check <- TRUE
        my_pipeline_steps$steps$step09$status <- paste0("Done! (Total Time: ", round(total_time_elapsed, 1), " secs)")
        
        # Mensaje de finalización
        state$message <- paste0("¡Pipeline Completado! Tiempo total: ", round(total_time_elapsed, 1), " segundos.")
        
      } else {
        # Manejo de advertencia
        my_pipeline_steps$steps$step09$check <- TRUE # Aún si falla, el proceso terminó.
        my_pipeline_steps$steps$step09$status <- "Warning! (Output file missing)"
        state$message <- "WARNING: Quarto ran, but the final HTML report was not found."
      }
      
      # [AVANCE DESACOPLADO] Avanzar al Step 10 (asumiendo que es el paso final de limpieza o conclusión)
      # Si este es el paso final real, puedes optar por no avanzar más.
      current_step_value <- state$current_step
      advance_step_trigger(current_step_value + 1) # Esto debería llevar a state$current_step = 10
      print("Step 09 - End (Async)")
      
    } %...!% {
      # --- MANEJO DE ERRORES (CRASH) ---
      error_message <- paste("Fatal Error in Step 09 (Final Quarto Rendering):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step09$status <- "Quarto Crash!"
    }
    
  }, ignoreInit = TRUE)
}