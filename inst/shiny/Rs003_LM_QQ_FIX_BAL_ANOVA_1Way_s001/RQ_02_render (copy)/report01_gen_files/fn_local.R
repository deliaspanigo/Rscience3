fn_get_nested_value_from_list <- function(the_list, key) {
  if (is.list(the_list)) {
    if (key %in% names(the_list)) {
      return(the_list[[key]])
    } else {
      # Si no está en este nivel, busca en los sub-niveles
      for (child in the_list) {
        result <- fn_get_nested_value_from_list(child, key)
        if (!is.null(result)) return(result)
      }
    }
  }
  return(NULL)
}



fn_extract_and_concatenate_code <- function(vector_files, 
                                            str_output_path = "output_code_extracted.R", 
                                            marker_string = "code_internal\\s*=\\s*TRUE") {
  
  all_extracted_code <- c()
  
  # Iniciamos el archivo de salida vacío (o lo sobrescribimos)
  cat("", file = str_output_path)
  
  for (str_file in vector_files) {
    
    if (!file.exists(str_file)) {
      message("Skipping: Archivo no encontrado -> ", str_file)
      next
    }
    
    message("Procesando: ", str_file, "...")
    
    # 1. Leer líneas
    vector_lines <- readLines(str_file, warn = FALSE)
    
    # 2. Identificar aperturas y cierres
    positions_open_R <- grep("^```\\{r.*\\}\\s*$", vector_lines)
    positions_close_R <- grep("^```\\s*$", vector_lines)
    
    if (length(positions_open_R) == 0) {
      message("  - Sin chunks de R encontrados.")
      next
    }
    
    # 3. Emparejar
    matched_closes <- vapply(positions_open_R, function(open_pos) {
      possible_closes <- positions_close_R[positions_close_R > open_pos]
      if (length(possible_closes) > 0) return(possible_closes[1])
      return(NA_real_)
    }, numeric(1))
    
    df_chunks <- data.frame(start = positions_open_R, end = matched_closes)
    df_chunks <- df_chunks[!is.na(df_chunks$end), ]
    
    # 4. Filtrar por el marcador
    header_lines <- vector_lines[df_chunks$start]
    keep_indices <- grepl(marker_string, header_lines)
    df_chunks_filtered <- df_chunks[keep_indices, ]
    
    message("  - Chunks detectados con el marcador: ", nrow(df_chunks_filtered))
    
    if (nrow(df_chunks_filtered) > 0) {
      # Construir el bloque de este archivo
      file_header <- c(
        paste0("# ", paste(rep("-", 50), collapse = "")),
        paste0("# FROM FILE: ", basename(str_file)),
        paste0("# ", paste(rep("-", 50), collapse = "")), 
        ""
      )
      
      # Extraer bloques
      list_of_blocks <- lapply(1:nrow(df_chunks_filtered), function(i) {
        block_code <- vector_lines[(df_chunks_filtered$start[i] + 1):(df_chunks_filtered$end[i] - 1)]
        block_code <- block_code[!grepl("```", block_code)]
        block_code <- block_code[!grepl("^#\\|", block_code)]
        return(c(block_code, "")) # Espacio entre chunks
      })
      
      # CONCATENACIÓN CRUCIAL: 
      # Sumamos el header y los bloques al acumulador global
      all_extracted_code <- c(all_extracted_code, file_header, unlist(list_of_blocks), "")
    }
  }
  
  # 6. Guardar el gran acumulador
  if (length(all_extracted_code) > 0) {
    writeLines(all_extracted_code, str_output_path)
    message("\n>>> ÉXITO: Todos los archivos procesados en: ", str_output_path)
  } else {
    message("\n>>> ERROR: No se extrajo nada de ningún archivo.")
  }
  
  return(str_output_path)
}



fn_copying_files_from_pattern <- function(target_pattern, destination_dir, overwrite = T){
  
  # 2. List all files/folders matching the pattern in the current directory
  # ^ ensures it matches the start of the string
  files_to_check <- list.files(
    path = ".", 
    pattern = target_pattern, 
    all.files = TRUE, 
    full.names = FALSE, 
    no.. = TRUE
  )
  
  # 3. Construct the potential destination paths
  # file.path() handles slashes correctly across different OS
  destination_paths <- file.path(destination_dir, files_to_check)
  
  # 4. Identify which items are missing in the destination folder
  missing_indices <- !file.exists(destination_paths)
  items_to_copy <- files_to_check[missing_indices]
  
  # 5. Execute copy process if there are pending items
  if (length(items_to_copy) > 0) {
    
    # Ensure the destination directory exists before copying
    if (!dir.exists(destination_dir)) {
      dir.create(destination_dir, recursive = TRUE)
    }
    
    # Copy items (recursive = TRUE handles both files and folders)
    copy_results <- file.copy(
      from = items_to_copy, 
      to = destination_dir, 
      overwrite = overwrite, 
      recursive = TRUE
    )
    
    message(sprintf("Successfully copied %d new items to: %s", sum(copy_results), destination_dir))
    
  } else {
    message("Sync complete: No new files found to copy.")
  }
}


