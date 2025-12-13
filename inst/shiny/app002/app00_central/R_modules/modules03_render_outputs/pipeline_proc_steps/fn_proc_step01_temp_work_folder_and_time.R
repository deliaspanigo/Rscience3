# pipeline_proc_steps/fn_proc_step01_temp_work_folder_and_time.R

# Definimos una función que toma los objetos reactivos como argumentos
fn_proc_step01_temp_work_folder_and_time <- function(state, my_pipeline_steps, list_bag_steps, the_timer, advance_step_trigger) {
  
  # Paso 01 ------------------------------------------------------------------
  # Preparing work output temporal folder and sys time.
  observeEvent(state$current_step, { # Simplificamos el observeEvent para evitar fallos de reactividad.
    
    # 1. Requeriments
    req(state$current_step == 1)
    req(!my_pipeline_steps$steps$step01$check)
    print("Step 01 - Init")
    
    # 2. State info - Actualiza la UI INMEDIATAMENTE al estado 'Running...'
    state$message <- my_pipeline_steps$steps$step01$info_step
    state$progress<- state$current_step / state$max_step
    
    # 3. Iniciar la tarea asíncrona (NO BLOQUEA LA UI)
    future_promise({
      
      # --- BLOQUE DE CÓDIGO EJECUTADO EN UN HILO/PROCESO SEPARADO ---
      
      # Opcional: Simular la duración del antiguo shinyjs::delay
      # Sys.sleep(1)
      
      # 5. Step actions...
      the_sys_time <- Sys.time()
      
      # 5.2. Special format
      timestamp_format <- format(the_sys_time, "%Y%m%d_%H%M%S")
      
      # 5.3 Folder path
      # NOTA: fn_app_str_new_temporal_output_folder_path debe ser accesible/cargada en el entorno global
      str_new_temp_folder_path <- fn_app_str_new_temporal_output_folder_path(timestamp_format = timestamp_format)
      
      # 5.4. Create new folder
      # Asumo que str_output_folder_const es una constante cargada previamente.
      str_output_folder_const <- "zzz_zzz_USER_OUPUT_FOLDER" # Reemplazar con el valor real si es constante
      
      dir.create(str_new_temp_folder_path, recursive = TRUE)
      
      # 5.5. Check folder existence
      check_new_temp_folder_path <- dir.exists(str_new_temp_folder_path)
      
      # Retornamos un objeto con todos los datos calculados para usarlos en el paso 6.
      list(
        check = check_new_temp_folder_path,
        sys_time = the_sys_time,
        timestamp_format = timestamp_format,
        temp_work_folder_path = str_new_temp_folder_path,
        temp_output_folder_path = file.path(str_new_temp_folder_path, str_output_folder_const)
      )
      
    }) %...>% {
      # --- BLOQUE DE CÓDIGO EJECUTADO DE VUELTA EN EL HILO PRINCIPAL DE SHINY (Callback) ---
      result <- .
      
      # 6. Step conclusion (usando el 'result' del hilo separado)
      if(result$check){
        # Actualizar reactiveValues
        list_bag_steps$sys_time <- result$sys_time
        list_bag_steps$timestamp_format <- result$timestamp_format
        list_bag_steps$temp_work_folder_path <- result$temp_work_folder_path
        list_bag_steps$temp_output_folder_path <- result$temp_output_folder_path
        
        the_timer$init_time <- result$sys_time
        
        # Marcar el paso como completado
        my_pipeline_steps$steps$step01$check <- TRUE
        my_pipeline_steps$steps$step01$status <- "Done!"
        
        # [CAMBIO CLAVE] Usar el trigger para avanzar al Step 02
        current_step_value <- state$current_step
        advance_step_trigger(current_step_value + 1)
        
      } else {
        # Manejar el fallo (No se avanza si hay error fatal)
        my_pipeline_steps$steps$step01$status <- "Failed! Folder not created."
        state$message <- "ERROR: No se pudo crear la carpeta de trabajo temporal."
      }
      
      print("Step 01 - End")
      
    } %...!% {
      # --- MANEJO DE ERRORES (Si ocurre un error fatal en el future) ---
      error_message <- paste("Error inesperado en Step 01 (Future):", .$message)
      warning(error_message)
      state$message <- error_message
      my_pipeline_steps$steps$step01$status <- "Fatal Error!"
    }
    
  }, ignoreInit = TRUE)
  
}