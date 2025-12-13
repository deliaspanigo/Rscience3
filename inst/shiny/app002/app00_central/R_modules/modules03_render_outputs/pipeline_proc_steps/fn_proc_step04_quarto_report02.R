# pipeline_proc_steps/fn_proc_step04_quarto_report02.R

#' Executes Step 04: Renders the second Quarto document (report02_R.qmd).
#'
#' @param state ReactiveValues containing the pipeline's current state.
#' @param my_pipeline_steps ReactiveValues containing step details.
#' @param list_bag_steps ReactiveValues containing data passed between steps.
#' @param set_default_quiet A non-reactive variable/constant indicating Quarto verbosity.
#' @param advance_step_trigger The reactiveVal used to force pipeline step advancement.

fn_proc_step04_quarto_report02 <- function(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger) {
  
  # ------------------------------------------------------------------
  # Step 04: Rendering the second Quarto document.
  # ------------------------------------------------------------------
  # Observador SIMPLE: escucha solo el avance de estado
  observeEvent(state$current_step, {
    
    # 1. Requirements
    req(state$current_step == 4)
    req(!my_pipeline_steps$steps$step04$check)
    print("Step 04 - Init")
    
    # 2. State info - Update UI immediately
    state$message      <- my_pipeline_steps$steps$step04$info_step
    state$progress     <- state$current_step / state$max_step
    
    # 3. Setup paths in the main thread (reading reactive values safely)
    str_work_dir_temporal <- list_bag_steps$temp_work_folder_path
    # Capturamos la ruta del output TEMPORAL para la verificación
    str_temp_output_folder_path <- list_bag_steps$temp_output_folder_path 
    
    str_subfolder <- "report02_R_script_HERE"
    str_input_file_name <- "report02_R.qmd"
    
    str_work_dir_new <- file.path(str_work_dir_temporal, str_subfolder)
    str_input_file_path <- file.path(str_work_dir_new, str_input_file_name)
    str_execute_dir <- dirname(str_input_file_path)
    
    # 4. Start the asynchronous Quarto rendering (NON-BLOCKING)
    future_promise({
      
      # --- BLOCK OF CÓDIGO EJECUTADO EN UN HILO SEPARADO ---
      
      # Ejecutar Quarto Rendering. Usamos execute_dir.
      quarto::quarto_render(
        input = str_input_file_path,
        execute = TRUE,
        quiet = set_default_quiet,
        execute_dir = str_execute_dir
      )
      
      # [!!! CAMBIO CLAVE: VERIFICACIÓN POR PATRÓN !!!]
      
      # Patrón a buscar para Step 04 (archivos o directorios que comiencen con "zzz_output_report02")
      pattern_to_check <- "^zzz_output_report02" 
      
      # Listar archivos/carpetas en el directorio de salida que coincidan con el patrón
      matching_files <- list.files(
        path = str_temp_output_folder_path,
        pattern = pattern_to_check,
        full.names = TRUE # Incluir la ruta completa
      )
      
      # El éxito se define si se encontró AL MENOS un archivo/carpeta coincidente.
      is_file_generated <- length(matching_files) > 0
      
      # Retornar el resultado
      list(
        output_file_names = matching_files,
        success = is_file_generated,
        output_path = if (is_file_generated) matching_files[1] else NULL
      )
      
    }) %...>% {
      # --- CALLBACK EJECUTADO DE VUELTA EN EL HILO PRINCIPAL DE SHINY ---
      result <- . 
      
      # 5. Step conclusion
      if (result$success) {
        # [ÉXITO] El reporte se generó y se encontró.
        list_bag_steps$final_report_02_path <- result$output_path 
        
        my_pipeline_steps$steps$step04$check  <- TRUE
        my_pipeline_steps$steps$step04$status <- "Done! (Report 02 Generated)"
        print("Step 04: TODO OK!")
        
      } else {
        # [ADVERTENCIA] Quarto se ejecutó, pero la verificación falló.
        my_pipeline_steps$steps$step04$check  <- TRUE # Marcamos como completado para avanzar
        my_pipeline_steps$steps$step04$status <- "Warning! (Output files missing)"
        state$message <- "WARNING: Quarto ran, but expected output files (zzz_output_report02*) were not found."
        print("Step 04: ERROR! (Files Missing, but continuing)")
      }
      
      # [!!! AVANCE INCONDICIONAL !!!]
      # Si llegamos aquí, Quarto no crasheó. Por lo tanto, avanzamos al Step 05.
      current_step_value <- state$current_step 
      advance_step_trigger(current_step_value + 1) # Establece 5
      
      print("Step 04 - End")
      
    } %...!% {
      # --- MANEJO DE ERRORES (CRASH) ---
      error_message <- paste("Fatal Error in Step 04 (Quarto Rendering):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step04$status <- "Quarto Crash!"
      # El pipeline se detiene aquí.
    }
    
  }, ignoreInit = TRUE)
}