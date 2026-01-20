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



fn_get_marked_code_from_qmd <- function(vector_files, str_output_file_path, marker_string = "take_code\\s*=\\s*(TRUE|T)") {
  
  # 1. Clean/Create the output file
  if (file.exists(str_output_file_path)) file.remove(str_output_file_path)
  
  # Temporary file to store the intermediate concatenated script
  temp_full_script <- tempfile(fileext = ".R")
  
  for (f in vector_files) {
    if (!file.exists(f)) {
      warning(paste("File not found:", f))
      next
    }
    
  # --- CAMBIO CLAVE AQUÍ ---
  # Limpiamos el contador de chunks y las etiquetas usadas para que purl no detecte duplicados
  knitr::opts_knit$set(unnamed.chunk.label = paste0("chunk-", basename(f))) 
  # -------------------------
    
    # 2. Extract all chunks using purl (documentation = 1 preserves headers)
    temp_purl <- tempfile(fileext = ".R")
    knitr::purl(input = f, output = temp_purl, documentation = 1, quiet = TRUE)
    
    # 3. Read lines and remove automatic comments from eval=FALSE chunks
    lines <- readLines(temp_purl, warn = FALSE)
    processed_lines <- c()
    is_eval_false_chunk <- FALSE
    
    for (line in lines) {
      # Detect chunk header
      if (startsWith(line, "## ----")) {
        is_eval_false_chunk <- grepl("eval\\s*=\\s*FALSE", line)
        processed_lines <- c(processed_lines, line)
        next
      }
      
      # Remove the '#' added by purl if the chunk was eval=FALSE
      if (is_eval_false_chunk) {
        clean_line <- sub("^#\\s?", "", line)
        processed_lines <- c(processed_lines, clean_line)
      } else {
        processed_lines <- c(processed_lines, line)
      }
    }
    
    # Append processed content to the intermediate script
    cat(paste(processed_lines, collapse = "\n"),
        file = temp_full_script, append = TRUE, fill = TRUE)
    
    unlink(temp_purl)
  }
  
  # 4. Final Filtering: Keep only chunks with the marker_string
  all_extracted_lines <- readLines(temp_full_script, warn = FALSE)
  final_code_lines <- c()
  keep_block <- FALSE
  
  for (line in all_extracted_lines) {
    # Check if we are at a chunk header
    if (startsWith(line, "## ----")) {
      # Does this header contain our target marker?
      if (grepl(marker_string, line)) {
        keep_block <- TRUE
      } else {
        keep_block <- FALSE
      }
      next # Skip the header line itself
    }
    
    # If the block is marked, collect the code
    if (keep_block) {
      final_code_lines <- c(final_code_lines, line)
    }
  }
  
  # 5. Save the final clean script
  writeLines(final_code_lines, str_output_file_path)
  unlink(temp_full_script)
  
  message(paste("Successfully generated:", str_output_file_path))
  return(str_output_file_path)
}


fn_copying_files_from_pattern <- function(target_pattern, destination_dir){
  
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
      overwrite = FALSE, 
      recursive = TRUE
    )
    
    message(sprintf("Successfully copied %d new items to: %s", sum(copy_results), destination_dir))
    
  } else {
    message("Sync complete: No new files found to copy.")
  }
}


