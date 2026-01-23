

clean_and_create_folder <- function(folder_path) {

  if (!base::dir.exists(folder_path)) {
    # Si no existe, se crea
    base::dir.create(folder_path, recursive = TRUE)

  } else {
    # 1. Listar todo el contenido (archivos y subcarpetas)
    # no.. = TRUE evita referencias circulares a carpetas superiores
    files_to_clean <- base::list.files(
      folder_path,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )

    # 2. Eliminar el contenido solo si hay algo que borrar
    if (length(files_to_clean) > 0) {
      base::unlink(files_to_clean, recursive = TRUE)
    }

    # 3. Limpiar el objeto de la memoria de R
    base::rm(files_to_clean)
  }
}


