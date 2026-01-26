# Función auxiliar para copiar carpetas de manera segura
copy_tool_folder <- function(original_file, target_folder) {

  # Obtener directorio original
  original_dir <- dirname(normalizePath(original_file))

  # Listar todos los archivos y carpetas
  all_files <- list.files(original_dir, full.names = TRUE, all.files = FALSE)

  # Crear directorio destino
  if (!dir.exists(target_folder)) {
    dir.create(target_folder, recursive = TRUE)
  }

  # Copiar cada archivo/carpeta
  for (file_path in all_files) {
    dest_path <- file.path(target_folder, basename(file_path))

    if (dir.exists(file_path)) {
      # Es una carpeta - copiar recursivamente
      file.copy(file_path, dirname(dest_path), recursive = TRUE, overwrite = TRUE)
    } else {
      # Es un archivo
      file.copy(file_path, dest_path, overwrite = TRUE)
    }
  }

  return(target_folder)
}

