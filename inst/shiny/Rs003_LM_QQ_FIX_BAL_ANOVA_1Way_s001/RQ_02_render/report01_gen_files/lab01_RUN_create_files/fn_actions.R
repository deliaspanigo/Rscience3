

fn_delete_single_file <- function(str_file_target) {
  library("fs")

  # Encabezado con estilo
  cat(paste0("<b>Processing single file:</b> <code>", str_file_target, "</code><br>"))

  # Comentario estilizado en itálicas y gris
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Delete single file if exists.</small><br>"))

  # Encabezado con estilo
  # cat(paste0("<b>Processing single file:</b> <code>", str_file_target, "</code><br>"))

  if (fs::file_exists(str_file_target)) {
    # 1. Intento de borrado dentro de tryCatch para capturar el resultado
    check_delete <- tryCatch({
      fs::file_delete(str_file_target)

      # 2. Reconfirmación post-borrado
      if (!fs::file_exists(str_file_target)) {
        cat("<span style='color: #28a745;'>✔ <b>Success:</b> File deleted and verified.</span><br><br>")
        TRUE # Retorno exitoso
      } else {
        cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Delete command issued, but file still exists.</span><br><br>")
        FALSE # Retorno fallido (comando no efectivo)
      }

    }, error = function(e) {
      # Error del sistema (archivo bloqueado, etc.)
      cat("<span style='color: #dc3545;'>✘ <b>Error:</b> File could not be deleted (it might be in use).</span><br>")
      cat(paste0("<small style='color: #b02a37;'>Technical Detail: ", e$message, "</small><br><br>"))
      FALSE # Retorno fallido por error técnico
    })

    return(check_delete) # Autoprint del resultado del tryCatch

  } else {
    # Si no existe desde el inicio, el objetivo se cumple (estado limpio)
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> File does not exist. Clean state confirmed.</span><br><br>")
    return(TRUE) # Autoprint directo
  }
}

fn_check_single_file <- function(str_file_path) {
  library("fs")

  # 1. Protection against NULL or empty values
  if (is.null(str_file_path) || str_file_path == "") {
    cat("<span style='color: #dc3545;'>✘ <b>Critical Error:</b> Provided file path is NULL or empty.</span><br><br>")
    return(FALSE)
  }

  # 2. Normalization
  str_file_path <- fs::path_tidy(str_file_path)

  # 3. Standardized Header with Code Path
  cat(paste0("<b>Verifying single file:</b> <code>", str_file_path, "</code><br>"))

  # 4. Standardized italicized gray action line
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Existence and accessibility audit.</small><br>"))

  # 5. Verification Logic
  if (fs::file_exists(str_file_path)) {
    # SUCCESS CASE
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> The file exists and is accessible.</span><br><br>")
    return(TRUE)

  } else {
    # ERROR CASE
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> File not found.</span><br>")

    # Audit Block with code detail
    cat(paste0("<div style='background-color: #fff5f5; border-left: 3px solid #dc3545; padding: 8px; margin-top: 5px;'>",
               "<small style='color: #6c757d;'><b>Expected Path:</b> <code>", str_file_path, "</code></small></div><br>"))
    return(FALSE)
  }
}

fn_rename_single_file <- function(str_path_from, str_path_to) {
  library("fs")

  # 1. Path normalization and extraction
  str_path_from <- fs::path_tidy(str_path_from)
  str_path_to   <- fs::path_tidy(str_path_to)
  str_name_from <- fs::path_file(str_path_from)
  str_name_to   <- fs::path_file(str_path_to)

  # 2. Standardized Header
  cat(paste0("<b>Renaming single file<br>"))

  # 3. Action line and Route Detail (FROM/TO)
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Change filename and relocate.</small><br>"))
  cat(paste0("<small style='color: #6c757d;'><b>FROM:</b> <code>", str_path_from, "</code></small><br>"))
  cat(paste0("<small style='color: #6c757d;'><b>TO:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b> &nbsp;&nbsp;<code>", str_path_to, "</code></small><br>"))

  # 4. Execution logic
  check_rename <- FALSE
  if (fs::file_exists(str_path_from)) {
    try({
      fs::file_move(path = str_path_from, new_path = str_path_to)
      # Double check existence in new location
      if (fs::file_exists(str_path_to)) check_rename <- TRUE
    }, silent = TRUE)
  }

  # 5. Visual Output
  if (check_rename) {
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> File renamed and verified.</span><br><br>")
    return(TRUE)
  } else {
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Could not rename file.</span><br>")

    # Error Audit Block
    cat(paste0("<div style='background-color: #fff5f5; border-left: 3px solid #dc3545; padding: 8px; margin-top: 5px;'>",
               "<small style='color: #b02a37;'><b>Technical Alert:</b> Source file missing or target path is invalid/locked.</small></div><br>"))
    return(FALSE)
  }
}

fn_copy_verify_single_file <- function(str_from, str_to) {
  library("fs")

  # 1. Path normalization and extraction
  str_from <- fs::path_tidy(str_from)
  str_to   <- fs::path_tidy(str_to)
  file_name <- fs::path_file(str_from)

  # 2. Standardized Header
  cat(paste0("<b>Copying & Verifying:</b> <code>", file_name, "</code><br>"))

  # 3. Action line and Route Detail (FROM/TO)
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Individual file transfer and integrity check.</small><br>"))
  cat(paste0("<small style='color: #6c757d;'><b>FROM:</b>&nbsp;<code>", str_from, "</code></small><br>"))
  cat(paste0("<small style='color: #6c757d;'><b>TO:</b>&nbsp;&nbsp;&nbsp;<code>", str_to, "</code></small><br>"))

  # 4. Execution Logic
  copy_status <- FALSE
  if (fs::file_exists(str_from)) {
    try({
      fs::file_copy(str_from, str_to, overwrite = TRUE)
      if (fs::file_exists(str_to)) copy_status <- TRUE
    }, silent = TRUE)
  }

  # 5. Visual Audit (Badges)
  status_from <- ifelse(fs::file_exists(str_from),
                        "<span style='background:#28a745; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>ORIGIN_OK</span>",
                        "<span style='background:#dc3545; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>ORIGIN_MISSING</span>")

  status_to <- ifelse(copy_status,
                      "<span style='background:#28a745; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>COPIED_OK</span>",
                      "<span style='background:#dc3545; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>COPY_FAILED</span>")

  cat(paste0("<div style='margin-top:8px; margin-bottom:10px;'>", status_from, " &nbsp; ⮕ &nbsp; ", status_to, "</div>"))

  # 6. Final Verdict
  if (copy_status) {
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> File copied and verified.</span><br><br>")
    return(TRUE)
  } else {
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Copy failed. Check source existence or destination permissions.</span><br><br>")
    return(FALSE)
  }
}


fn_check_single_folder <- function(str_folder_target) {
  library("fs")

  # 1. Protection against NULL or empty values
  if (is.null(str_folder_target) || str_folder_target == "") {
    cat("<span style='color: #dc3545;'>✘ <b>Critical Error:</b> Provided folder path is NULL or empty.</span><br><br>")
    return(FALSE)
  }

  # 2. Path normalization
  str_folder_target <- fs::path_tidy(str_folder_target)

  # 3. Standardized Header with Code Path
  cat(paste0("<b>Verifying folder:</b> <code>", str_folder_target, "</code><br>"))

  # 4. Standardized italicized gray action line
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Existence and accessibility audit.</small><br>"))

  # 5. Verification Logic
  if (fs::dir_exists(str_folder_target)) {
    # SUCCESS CASE
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> Directory exists and is accessible.</span><br><br>")
    return(TRUE)

  } else {
    # ERROR CASE
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Directory not found.</span><br>")

    # Standard technical detail block for missing folders
    cat(paste0("<div style='background-color: #fff5f5; border-left: 3px solid #dc3545; padding: 8px; margin-top: 5px;'>",
               "<small style='color: #6c757d;'><b>Expected Path:</b> <code>", str_folder_target, "</code></small></div><br>"))
    return(FALSE)
  }
}


fn_check_and_create_single_folder <- function(str_folder_target) {
  library("fs")

  # Encabezado con estilo
  cat(paste0("<b>Processing single folder:</b> <code>", str_folder_target, "</code><br>"))

  # Comentario estilizado en itálicas y gris
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Check existence or create directory if missing.</small><br>"))

  if (fs::dir_exists(str_folder_target)) {
    # CASO 1: Ya existe
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> Directory already exists.</span><br><br>")
    return(TRUE)

  } else {
    # CASO 2: No existe, intentamos crearla
    cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> Directory not found. Attempting to create...</span><br>")

    # Usamos una variable para capturar el resultado del bloque tryCatch
    check_final <- tryCatch({
      # Creamos la carpeta (recurse = TRUE por si faltan carpetas intermedias)
      fs::dir_create(str_folder_target, recurse = TRUE)

      # Reconfirmación post-creación
      if (fs::dir_exists(str_folder_target)) {
        cat("<span style='color: #28a745;'>✔ <b>Success:</b> Directory created and verified.</span><br><br>")
        TRUE
      } else {
        cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Command issued but directory not detected.</span><br><br>")
        FALSE
      }

    }, error = function(e) {
      # Error de permisos, disco lleno o ruta inválida
      cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Could not create directory. Check permissions.</span><br>")
      cat(paste0("<small style='color: #b02a37;'>Technical Detail: ", e$message, "</small><br><br>"))
      FALSE
    })

    return(check_final)
  }
}


fn_cleaning_single_folder <- function(str_folder_target) {
  library("fs")

  # Encabezado con estilo
  cat(paste0("<b>Purging single folder content:</b> <code>", str_folder_target, "</code><br>"))

  # Comentario estilizado
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Remove all internal files and subdirectories.</small><br>"))

  if (fs::dir_exists(str_folder_target)) {

    # Listamos todo el contenido (all = TRUE incluye archivos ocultos)
    content_to_delete <- fs::dir_ls(str_folder_target, all = TRUE)
    total_items <- length(content_to_delete)

    if (total_items > 0) {
      check_purge <- tryCatch({

        # Borrado recursivo item por item
        for (item in content_to_delete) {
          if (fs::is_dir(item)) {
            fs::dir_delete(item)
          } else {
            fs::file_delete(item)
          }
        }

        # Reconfirmación post-borrado
        remaining <- length(fs::dir_ls(str_folder_target))

        if (remaining == 0) {
          cat(paste0("<span style='color: #28a745;'>✔ <b>Success:</b> All content removed (", total_items, " items deleted).</span><br><br>"))
          TRUE
        } else {
          cat(paste0("<span style='color: #dc3545;'>✘ <b>Error:</b> Purge incomplete. ", remaining, " items still remain.</span><br><br>"))
          FALSE
        }

      }, error = function(e) {
        cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Could not complete purge (some files might be in use).</span><br>")
        cat(paste0("<small style='color: #b02a37;'>Technical Detail: ", e$message, "</small><br><br>"))
        FALSE
      })

      return(check_purge)

    } else {
      cat("<span style='color: #28a745;'>✔ <b>Success:</b> Folder is already empty. Nothing to purge.</span><br><br>")
      return(TRUE)
    }

  } else {
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Target directory does not exist. Verify path.</span><br><br>")
    return(FALSE)
  }
}

################################################################################

fn_check_multi_files <- function(vector_paths) {
  library("fs")
  library("knitr")
  library("kableExtra")
  library("dplyr")

  # Encabezado del proceso
  cat("<b>Checking Multiple Files:</b><br>")
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Batch existence audit for ", length(vector_paths), " items.</small><br><br>"))

  # 1. Crear un dataframe con los resultados
  df_results <- data.frame(
    File = fs::path_file(vector_paths),
    Status = ifelse(fs::file_exists(vector_paths), "EXISTS", "MISSING"),
    Path = vector_paths,
    stringsAsFactors = FALSE
  )
  rownames(df_results) <- 1:nrow(df_results)

  # 2. Generar la tabla estilizada
  tabla_html <- df_results %>%
    mutate(
      Status = cell_spec(Status, color = "white", bold = TRUE,
                         background = ifelse(Status == "EXISTS", "#28a745", "#dc3545"))
    ) %>%
    kable(format = "html", escape = FALSE, align = "lll") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F,
                  position = "left") %>%
    column_spec(1, bold = TRUE, width = "250px") %>%
    column_spec(3, color = "#6c757d", extra_css = "font-size: 10px;")

  # Imprimir la tabla
  cat(tabla_html)
  cat("<br>")

  # 3. Resumen final con contador (X/Y)
  total_files   <- length(vector_paths)
  total_exists  <- sum(fs::file_exists(vector_paths))
  total_missing <- total_files - total_exists

  if (total_missing == 0) {
    # Todos están bien
    cat(paste0("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> All files are present (", total_exists, "/", total_files, ").</span><br><br>"))
    return(TRUE)
  } else {
    # Faltan algunos
    cat(paste0("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> ", total_missing, " file(s) are missing! (Only ", total_exists, "/", total_files, " found).</span><br><br>"))
    return(FALSE)
  }
}



fn_delete_multi_files <- function(vector_paths) {
  library("fs")
  library("knitr")
  library("kableExtra")
  library("dplyr")

  # 1. Guard Clause: Check if the vector has any content
  if (is.null(vector_paths) || length(vector_paths) == 0) {
    cat("<b>Batch Purge Process:</b><br>")
    cat("<small style='color: #6c757d;'>Files deleted: 0</small><br>")
    cat("<span style='color: #28a745;'>✔ <b>Clean:</b> No files provided for deletion. Everything is in order.</span><br><br>")
    return(TRUE)
  }

  # Report Header
  cat("<b>Batch Purge Process:</b><br>")

  # 2. Execute deletion and collect results
  results_list <- lapply(vector_paths, function(path) {
    status_item <- "UNKNOWN"

    if (!fs::file_exists(path)) {
      status_item <- "ALREADY GONE"
    } else {
      tryCatch({
        fs::file_delete(path)
        status_item <- "DELETED"
      }, error = function(e) {
        status_item <- "ERROR/LOCKED"
      })
    }

    df_output <- data.frame(
      File = fs::path_file(path),
      Result = status_item,
      Path = path,
      stringsAsFactors = FALSE
    )
    return(df_output)
  })

  df_results <- do.call(rbind, results_list)

  # 3. Summary Phrase
  deleted_count <- sum(df_results$Result == "DELETED")
  cat(paste0("<small style='color: #333;'>Files deleted: <b>", deleted_count, "</b></small><br>"))
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Attempting to purge ", length(vector_paths), " specific items.</small><br><br>"))

  # 4. Generate Stylized HTML Table
  html_table <- df_results %>%
    mutate(
      Result = cell_spec(Result, color = "white", bold = TRUE,
                         background = case_when(
                           Result == "DELETED"      ~ "#28a745", # Green
                           Result == "ALREADY GONE" ~ "#28a745", # Green (Success confirmation)
                           Result == "ERROR/LOCKED" ~ "#dc3545", # Red
                           TRUE                     ~ "#ffc107"  # Yellow
                         ))
    ) %>%
    kable(format = "html", escape = FALSE, align = "lll") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F,
                  position = "left") %>%
    column_spec(1, bold = TRUE, width = "250px") %>%
    column_spec(3, color = "#6c757d", extra_css = "font-size: 10px;")

  # Print Table
  cat(html_table)
  cat("<br>")

  # 5. Final Verdict
  files_still_there <- sum(fs::file_exists(vector_paths))

  if (files_still_there == 0) {
    cat("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> All target files have been removed or confirmed absent.</span><br><br>")
    return(TRUE)
  } else {
    cat(paste0("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Clean-up incomplete. ", files_still_there, " file(s) still exist.</span><br><br>"))
    return(FALSE)
  }
}


fn_copy_verify_multi_files <- function(vector_from, vector_to) {
  library("fs")
  library("knitr")
  library("kableExtra")
  library("dplyr")

  # 1. Identificar carpetas para el encabezado
  dir_from <- fs::path_dir(vector_from[1])
  dir_to   <- fs::path_dir(vector_to[1])

  # Encabezado con estilo (Corregido: Processing)
  cat("<b>Batch Copy & Verify Process:</b><br>")
  cat(paste0("<small style='color: #6c757d;'><b>FROM:</b> <code>", dir_from, "</code></small><br>"))
  cat(paste0("<small style='color: #6c757d;'><b>TO:</b> <code>", dir_to, "</code></small><br>"))
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Processing and auditing ", length(vector_from), " items.</small><br><br>"))

  # 2. Ejecutar la copia con manejo de errores
  for (i in seq_along(vector_from)) {
    if (fs::file_exists(vector_from[i])) {
      try(fs::file_copy(vector_from[i], vector_to[i], overwrite = TRUE), silent = TRUE)
    }
  }

  # 3. Construir Dataframe de Auditoría
  df_audit <- data.frame(
    File = fs::path_file(vector_from),
    folder_FROM = ifelse(fs::file_exists(vector_from), "ORIGIN_OK", "ORIGIN_MISSING"),
    folder_TO = ifelse(fs::file_exists(vector_to), "COPIED_OK", "COPY_FAILED"),
    stringsAsFactors = FALSE
  )
  rownames(df_audit) <- 1:nrow(df_audit)

  # 4. Generar Tabla Estilizada
  tabla_html <- df_audit %>%
    mutate(
      folder_FROM = cell_spec(folder_FROM, color = "white", bold = TRUE,
                         background = ifelse(folder_FROM == "ORIGIN_OK", "#28a745", "#dc3545")),
      folder_TO = cell_spec(folder_TO, color = "white", bold = TRUE,
                         background = ifelse(folder_TO == "COPIED_OK", "#28a745", "#dc3545"))
    ) %>%
    kable(format = "html", escape = FALSE, align = "lll") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F, position = "left") %>%
    column_spec(1, bold = TRUE, width = "300px") %>%
    column_spec(2:3, width = "150px")

  # Imprimir tabla
  cat(tabla_html)
  cat("<br>")

  # 5. Veredicto Final con Retorno Booleano
  total_files <- length(vector_from)
  total_success <- sum(fs::file_exists(vector_to))

  if (total_success == total_files) {
    cat(paste0("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> All files copied successfully (", total_success, "/", total_files, ").</span><br><br>"))
    return(TRUE) # Retorno para Autoprint y Control Maestro
  } else {
    cat(paste0("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Copy incomplete! (Only ", total_success, "/", total_files, " verified in destination).</span><br><br>"))
    return(FALSE) # Retorno para Autoprint y Control Maestro
  }
}


fn_export_files_zzz_output <- function(str_path_from, str_path_to, bln_overwrite = FALSE, bln_production = FALSE) {
  library("fs")
  library("dplyr")

  # 1. Path Normalization
  str_path_from <- fs::path_tidy(str_path_from)
  str_path_to   <- fs::path_tidy(str_path_to)

  # 2. Report Header
  cat(paste0("<b>Export Audit:</b> <code>zzz_output*</code><br>"))

  env_label <- if (bln_production)
    "<span style='background:#007bff; color:white; padding:2px 5px; border-radius:3px; font-size:10px;'>PRODUCTION</span>" else
      "<span style='background:#6f42c1; color:white; padding:2px 5px; border-radius:3px; font-size:10px;'>TESTING/DEV</span>"

  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Status: ", env_label, "</small><br>"))

  # 3. File Listing and Analysis
  list_files <- fs::dir_ls(str_path_from, regexp = "/zzz_output")
  if (length(list_files) == 0) {
    cat("<span style='color: #6c757d;'>ℹ <b>Info:</b> No files found with prefix 'zzz_output'.</span><br><br>")
    return(TRUE)
  }

  file_names <- fs::path_file(list_files)
  dest_paths <- fs::path(str_path_to, file_names)
  exists_at_dest <- fs::file_exists(dest_paths)

  # 4. Decision Logic
  final_overwrite <- if (!bln_production) TRUE else bln_overwrite

  # 5. Intent Phrase and Overwrite Label
  ovw_status_label <- if (final_overwrite) {
    "<span style='color: #28a745;'>Enabled</span>"
  } else {
    "<span style='color: #dc3545;'>Disabled</span>"
  }

  intent_phrase <- if (!bln_production) {
    "Testing Mode: All existing files will be overwritten to ensure the latest version."
  } else if (bln_production && bln_overwrite) {
    "Production Mode: Manual overwrite authorized for all files."
  } else {
    "Production Mode: Only files that DO NOT exist in destination will be copied (Safe Mode)."
  }

  # Detailed Summary Block
  cat(paste0("<div style='margin-top: 10px; margin-bottom: 15px; border-left: 3px solid #dee2e6; padding-left: 10px;'>",
             "<small style='color: #333;'><b>Overwrite:</b> ", ovw_status_label, "</small><br>",
             "<small style='color: #333;'><b>Scan Result:</b> Detected <b>", length(list_files), "</b> files (<b>", sum(exists_at_dest), "</b> already exist).</small><br>",
             "<small style='color: #6c757d; font-weight: bold;'><i>", intent_phrase, "</i></small></div>"))

  # Critical Error Case: Production + All exist + No Overwrite
  if (bln_production && !bln_overwrite && all(exists_at_dest)) {
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> All files already exist and overwrite is disabled.</span><br><br>")
    return(FALSE)
  }

  # 6. Audit Table
  cat("<table style='width: 100%; font-size: 11px; border-collapse: collapse; margin-bottom: 15px;'>")
  cat("<tr style='background-color: #f8f9fa; border-bottom: 2px solid #dee2e6;'>
        <th style='text-align: left; padding: 5px;'>File Name</th>
        <th style='text-align: center; padding: 5px;'>Current Status</th>
        <th style='text-align: center; padding: 5px;'>Action Taken</th>
      </tr>")

  execution_list <- list()

  for (i in seq_along(list_files)) {
    f_name <- file_names[i]
    already_there <- exists_at_dest[i]

    if (already_there && !final_overwrite) {
      action <- "<span style='color: #6c757d;'>Skipped</span>"
      status <- "Found"
      do_copy <- FALSE
    } else if (already_there && final_overwrite) {
      action <- "<span style='color: #ffc107;'>Overwritten</span>"
      status <- "Found"
      do_copy <- TRUE
    } else {
      action <- "<span style='color: #28a745;'>Copied</span>"
      status <- "New"
      do_copy <- TRUE
    }

    cat(paste0("<tr style='border-bottom: 1px solid #eee;'>
                <td style='padding: 4px;'><code>", f_name, "</code></td>
                <td style='text-align: center;'>", status, "</td>
                <td style='text-align: center;'><b>", action, "</b></td>
               </tr>"))

    if (do_copy) execution_list <- c(execution_list, list_files[i])
  }
  cat("</table>")

  # 7. Physical Execution
  if (length(execution_list) > 0) {
    check_export <- tryCatch({
      if (!fs::dir_exists(str_path_to)) fs::dir_create(str_path_to, recurse = TRUE)
      fs::file_copy(unlist(execution_list), str_path_to, overwrite = final_overwrite)
      TRUE
    }, error = function(e) return(e$message))
  } else {
    check_export <- TRUE
  }

  # 8. Final Verdict
  if (isTRUE(check_export)) {
    cat("<span style='color: #28a745;'>✔ <b>Audit Complete:</b> Synchronization finished successfully.</span><br><br>")
    return(TRUE)
  } else {
    cat(paste0("<span style='color: #dc3545;'>✘ <b>Error:</b> ", check_export, "</span><br><br>"))
    return(FALSE)
  }
}
