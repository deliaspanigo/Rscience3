# pipeline_proc_steps/step03.R

#' Executes Step 03: Renders the Quarto document using execute_dir.
#'
# ... (Parámetros sin cambios)

fn_proc_step03_quarto_report01 <- function(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger) {
  
  # ------------------------------------------------------------------
  # Step 03: Rendering Quarto document.
  # ------------------------------------------------------------------
  observeEvent(state$current_step, {
    
    # 1. Requirements
    req(state$current_step == 3)
    req(!my_pipeline_steps$steps$step03$check)
    print("Step 03 - Init")
    
    # 2. State info - Update UI immediately
    state$message  <- my_pipeline_steps$steps$step03$info_step
    state$progress <- state$current_step / state$max_step
    
    # 3. Setup paths in the main thread
    str_work_dir_temporal <- list_bag_steps$temp_work_folder_path
    # Capturamos la ruta del output TEMPORAL para la verificación (se pasa al future)
    str_temp_output_folder_path <- list_bag_steps$temp_output_folder_path 
    
    str_subfolder <- "report01_RQuarto_HERE"
    str_input_file_name <- "report01_RQuarto_00.qmd"
    
    str_work_dir_new <- file.path(str_work_dir_temporal, str_subfolder)
    str_input_file_path <- file.path(str_work_dir_new, str_input_file_name)
    str_execute_dir <- dirname(str_input_file_path)
    
    # 4. Start the asynchronous Quarto rendering (NON-BLOCKING)
    future_promise({
      
      # --- BLOCK OF CÓDIGO EJECUTADO EN UN HILO SEPARADO ---
      
      quarto::quarto_render(
        input = str_input_file_path,
        execute = TRUE,
        quiet = set_default_quiet,
        execute_dir = str_execute_dir
      )
      
      # [!!! CAMBIO CLAVE: VERIFICACIÓN POR PATRÓN !!!]
      
      # Patrón a buscar (archivos o directorios que comiencen con "zzz_output_report01")
      pattern_to_check <- "^zzz_output_report01" 
      
      # Listar archivos/carpetas en el directorio de salida que coincidan con el patrón
      # La ruta str_temp_output_folder_path es accesible aquí porque fue capturada
      # en el hilo principal antes de future_promise.
      matching_files <- list.files(
        path = str_temp_output_folder_path,
        pattern = pattern_to_check,
        full.names = TRUE # Incluir la ruta completa
      )
      
      # El éxito se define si se encontró AL MENOS un archivo/carpeta coincidente.
      is_file_generated <- length(matching_files) > 0
      
      list(
        output_file_names = matching_files, # Devolvemos lo que realmente se encontró
        success = is_file_generated,
        output_path = if (is_file_generated) matching_files[1] else NULL # Usamos el primer archivo encontrado como path principal
      )
      
    }) %...>% {
      # --- CALLBACK EJECUTADO DE VUELTA EN EL HILO PRINCIPAL DE SHINY ---
      result <- .
      
      # 5. Step conclusion
      if (result$success) {
        # [ÉXITO] El reporte se generó y se encontró.
        list_bag_steps$final_report_path <- result$output_path
        
        my_pipeline_steps$steps$step03$check <- TRUE
        my_pipeline_steps$steps$step03$status <- "Done! (Report Generated)"
        print("TODO OK!")
        
      } else {
        # [ADVERTENCIA] Quarto se ejecutó, pero la verificación falló.
        my_pipeline_steps$steps$step03$check  <- TRUE # Marcamos como completado para avanzar
        my_pipeline_steps$steps$step03$status <- "Warning! (Output files missing)"
        state$message <- "WARNING: Quarto ran, but expected output files were not found."
        print("ERROR! (Files Missing, but continuing)")
      }
      
      # [!!! AVANCE INCONDICIONAL !!!]
      current_step_value <- state$current_step
      advance_step_trigger(current_step_value + 1)
      
      print("Step 03 - End")
      
    } %...!% {
      # --- MANEJO DE ERRORES (CRASH) ---
      error_message <- paste("Fatal Error in Step 03 (Quarto Rendering):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step03$status <- "Quarto Crash!"
    }
    
  }, ignoreInit = TRUE)
}