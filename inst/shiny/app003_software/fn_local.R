
source_rscience_modules <- function(paths, pattern = "^module_.*\\.R$") {

  # Asegurar que solo procesamos carpetas que existen para evitar errores
  valid_paths <- paths[dir.exists(paths)]

  if (length(valid_paths) == 0) {
    warning("Ninguna de las rutas especificadas existe.")
    return(NULL)
  }

  # Listar todos los archivos en todas las carpetas válidas
  all_files <- list.files(
    path = valid_paths,
    pattern = pattern,
    full.names = TRUE,
    recursive = FALSE
  )

  if (length(all_files) > 0) {
    # Aplicar source a cada archivo
    # Usamos local = FALSE para asegurar que se carguen en el Global Environment
    invisible(lapply(all_files, source, local = FALSE))

    # Mensaje informativo agrupado por carpeta para el log
    lapply(valid_paths, function(p) {
      count <- sum(grepl(p, all_files))
      message(sprintf("✓ [%d] módulos cargados desde: %s", count, p))
    })

  } else {
    warning("No se encontraron archivos que coincidan con el patrón en las rutas dadas.")
  }

  return(all_files)
}

