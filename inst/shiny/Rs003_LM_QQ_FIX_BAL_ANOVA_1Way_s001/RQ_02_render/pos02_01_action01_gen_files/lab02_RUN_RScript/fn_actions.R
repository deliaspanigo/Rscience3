

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

  # 2. Header with standardized style
  cat(paste0("<b>Verifying single file:</b> <code>", str_file_path, "</code><br>"))

  # 3. Standardized italicized gray action comment
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Check existence of a specific file.</small><br>"))

  # 4. Verification Logic
  if (fs::file_exists(str_file_path)) {
    # SUCCESS CASE
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> The file exists and is accessible.</span><br><br>")
    return(TRUE)

  } else {
    # ERROR CASE
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> File not found at target location.</span><br>")

    # Standard technical detail block for missing paths
    cat(paste0("<div style='background-color: #fff5f5; border-left: 3px solid #dc3545; padding: 5px; margin-top: 5px;'>",
               "<small style='color: #6c757d;'><b>Expected Path:</b> ", str_file_path, "</small></div><br>"))
    return(FALSE)
  }
}

fn_rename_single_file <- function(str_path_from, str_path_to) {
  library("fs")

  # Extraemos nombres para un reporte más limpio
  str_name_from <- fs::path_file(str_path_from)
  str_name_to   <- fs::path_file(str_path_to)

  # Encabezado informativo
  cat(paste0("<b>Renaming single file:</b><br>"))
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Change filename<br>"))

  # Detalle de rutas completas en pequeño
  cat(paste0("<small style='color: #6c757d;'><b>Path From:</b> ", str_path_from, "</small><br>"))
  cat(paste0("<small style='color: #6c757d;'><b>Path To:</b> ", str_path_to, "</small><br>"))

  # Ejecución del renombrado
  check_rename <- FALSE
  try({
    # Usamos fs::file_move porque es más potente que file.rename
    # (funciona mejor entre diferentes volúmenes de disco)
    if (fs::file_exists(str_path_from)) {
      fs::file_move(path = str_path_from, new_path = str_path_to)
      check_rename <- TRUE
    }
  }, silent = TRUE)

  # Salida visual basada en el resultado
  if (check_rename) {
    cat("<span style='color: #28a745;'>✔ <b>Success:</b> File renamed successfully.</span><br><br>")
    return(TRUE)
  } else {
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Could not rename file. Verify if the source exists or if the target name is already in use.</span><br><br>")
    return(FALSE)
  }
}

fn_copy_verify_single_file <- function(str_from, str_to) {
  library("fs")

  # 1. Preparar nombres para el reporte
  file_name <- fs::path_file(str_from)

  # Encabezado compacto
  cat("<b>Single File Copy & Verify:</b><br>")
  cat(paste0("<small style='color: #6c757d;'><b>FROM:</b> <code>", str_from, "</code></small><br>"))
  cat(paste0("<small style='color: #6c757d;'><b>TO:</b> <code>", str_to, "</code></small><br>"))

  # cat(paste0("<code style='background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 1px 4px; border-radius: 3px;'>", file_name, "</code><br>"))
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Copying and auditing individual item.</small><br>"))

  # 2. Ejecutar la copia con manejo de errores
  copy_status <- FALSE
  if (fs::file_exists(str_from)) {
    # Intentar la copia
    try_copy <- try(fs::file_copy(str_from, str_to, overwrite = TRUE), silent = TRUE)

    # Verificar si la copia realmente existe en el destino
    if (fs::file_exists(str_to)) {
      copy_status <- TRUE
    }
  }

  # 3. Auditoría Visual (Badges de estado)
  status_from <- ifelse(fs::file_exists(str_from),
                        "<span style='background:#28a745; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>ORIGIN_OK</span>",
                        "<span style='background:#dc3545; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>ORIGIN_MISSING</span>")

  status_to <- ifelse(copy_status,
                      "<span style='background:#28a745; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>COPIED_OK</span>",
                      "<span style='background:#dc3545; color:white; padding:2px 6px; border-radius:3px; font-size:10px;'>COPY_FAILED</span>")

  # Imprimir línea de auditoría simple
  cat(paste0("<div style='margin-top:5px; margin-bottom:10px;'>", status_from, " &nbsp; ⮕ &nbsp; ", status_to, "</div>"))

  # 4. Veredicto Final y Retorno Físico
  if (copy_status) {
    cat("<span style='color: #28a745;'>✔ <b>Final Verdict:</b> File copied and verified.</span><br><br>")
    return(TRUE) # Retorno común para Autoprint
  } else {
    cat("<span style='color: #dc3545;'>✘ <b>Final Verdict:</b> Copy failed or source missing.</span><br>")
    cat(paste0("<small style='color: #6c757d;'>Path TO: ", str_to, "</small><br><br>"))
    return(FALSE) # Retorno común para Autoprint
  }
}


fn_check_single_folder <- function(str_folder_target) {
  library("fs")

  # Encabezado con estilo
  # cat(paste0("<b>Processing single folder.</b><br>"))
  cat(paste0("<b>Processing single folder:</b> <code>", str_folder_target, "</code><br>"))

  # Comentario estilizado en itálicas y gris
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Check if target directory exists.</small><br>"))

  # Encabezado con estilo
  # cat(paste0("<b>Selected folder:</b> <code>", str_folder_target, "</code><br>"))


  if (!is.null(str_folder_target) && str_folder_target != "") {

    if (fs::dir_exists(str_folder_target)) {
      # CASO ÉXITO
      cat("<span style='color: #28a745;'>✔ <b>Success:</b> Directory exists and is accessible.</span><br><br>")
      return(TRUE) # Retorno para autoprint y control

    } else {
      # CASO ERROR: No existe
      cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Directory not found. Please verify the path.</span><br><br>")
      return(FALSE) # Retorno para autoprint y control
    }

  } else {
    # CASO ERROR: Ruta inválida
    cat("<span style='color: #dc3545;'>✘ <b>Error:</b> Provided folder path is empty or NULL.</span><br><br>")
    return(FALSE) # Retorno para autoprint y control
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

  # Encabezado con estilo
  cat("<b>Batch Purge Process:</b><br>")
  cat(paste0("<small style='color: #6c757d; font-style: italic;'>Action: Attempting to delete ", length(vector_paths), " specific items.</small><br><br>"))

  # 1. Ejecutar el borrado y recolectar resultados
  resultados <- lapply(vector_paths, function(path) {
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

    return(data.frame(
      File = fs::path_file(path),
      Result = status_item,
      Path = path,
      stringsAsFactors = FALSE
    ))
  })

  df_results <- do.call(rbind, resultados)

  # 2. Generar la tabla estilizada
  tabla_html <- df_results %>%
    mutate(
      Result = cell_spec(Result, color = "white", bold = TRUE,
                         background = case_when(
                           Result == "DELETED"      ~ "#28a745", # Verde
                           Result == "ALREADY GONE" ~ "#28a745", #6c757d", # Gris
                           Result == "ERROR/LOCKED" ~ "#dc3545", # Rojo
                           TRUE                     ~ "#ffc107"
                         ))
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

  # 3. Veredicto Final
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
