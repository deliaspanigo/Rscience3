
fn_app_MY_PACKAGE_NAME <- function(){return("Rscience3")}

fn_app_cut_path_to_package <- function(full_path, package_name) {
  # Escapar caracteres especiales para regex
  safe_package <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", package_name)
  
  # Patrón: todo hasta e incluyendo package_name
  pattern <- paste0("^(.*?", safe_package, ")")
  
  # Aplicar
  result <- sub(paste0("(", safe_package, ").*$"), "\\1", full_path)
  
  # Verificar que el resultado contiene el package_name
  if (!grepl(package_name, result, fixed = TRUE)) {
    warning("Package name not found in path. Returning original path.")
    return(full_path)
  }
  
  return(result)
}


fn_app_set_reactive_values_from_list <- function(rv, data_list, list_default) {

  # 1. Verificar que ambos son objetos válidos
  if (!is.list(data_list)) {
    stop("'data_list' must be a standard R list")
  }

  if (!is.list(list_default)) {
    stop("'list_default' must be a standard R list")
  }

  # 2. Obtener nombres permitidos desde list_default
  allowed_names <- names(list_default)
  new_names <- names(data_list)

  # 3. Validación: Prevenir creación de nuevos items
  unauthorized_names <- setdiff(new_names, allowed_names)
  if (length(unauthorized_names) > 0) {
    stop("Attempt to create new items in reactiveValues: ",
         paste(unauthorized_names, collapse = ", "),
         "\nAllowed names: ", paste(allowed_names, collapse = ", "))
  }

  # 4. Validación opcional de tipos de datos
  for (name in new_names) {
    if (name %in% allowed_names) {
      default_type <- class(list_default[[name]])
      new_type <- class(data_list[[name]])

      if (!identical(default_type, new_type)) {
        warning("Type mismatch for '", name, "': ", default_type, " vs ", new_type,
                ". Value will be assigned but types differ.")
      }
    }
  }

  # 5. Iterar sobre los nombres de la lista de datos
  for (name in names(data_list)) {
    rv[[name]] <- data_list[[name]]
  }

  invisible(rv)
}

fn_app_reset_stone <- function(rv, list_default) {
  # Simplemente usa la lista default completa
  fn_app_set_reactive_values_from_list(
    rv = rv,
    data_list = list_default,
    list_default = list_default
  )
}



fn_app_validate_stone_integrity <- function(rv, list_default) {
  # Esta SÍ puede ir en un contexto reactivo (como un observer)
  current_state <- reactiveValuesToList(rv)

  list(
    is_valid = identical(sort(names(current_state)), sort(names(list_default))),
    current_structure = names(current_state),
    expected_structure = names(list_default),
    issues = if (!identical(sort(names(current_state)), sort(names(list_default)))) {
      list(
        missing = setdiff(names(list_default), names(current_state)),
        extra = setdiff(names(current_state), names(list_default))
      )
    } else NULL
  )
}
###############################################################################
fn_app_str_new_temporal_output_folder_path <- function(timestamp_format){
  my_temp_folder <- tempdir()


  new_sub_folder <- paste0("temp_", timestamp_format)
  new_folder <- file.path(my_temp_folder, new_sub_folder)
  return(new_folder)
}


fn_app_find_my_folder_path_package <- function(MY_PACKAGE_NAME){

  selected_package_path <- tryCatch(
    # Intenta ejecutar este código
    expr = {
      find.package(MY_PACKAGE_NAME)
    },
    # Si ocurre un error, ejecuta este código y devuelve su resultado
    error = function(e) {
      # El error de 'find.package' se dispara cuando no encuentra el paquete.
      # En ese caso, devolvemos getwd(), que es el path del archivo app.R
      # y lo recortamos para quedarnos en la subcarpeta del package.
      the_local_path <- strsplit(getwd(), MY_PACKAGE_NAME)
      the_local_path <-file.path(the_local_path[[1]][1], MY_PACKAGE_NAME, "inst")
      return(the_local_path)
    }
  )




  vector_folder_paths <- list.dirs(path = selected_package_path, recursive = T)
  dt_selected_quarto_folder <- grepl("quarto$", vector_folder_paths, ignore.case = TRUE)
  selected_quarto_folder_path <- vector_folder_paths[dt_selected_quarto_folder]

  #print(selected_quarto_folder_path)

  return(selected_quarto_folder_path)
}



#
fn_app_update_ENV_modal_progress_bar <- function(progress_bar, value, message, detail = "") {
  progress_bar$set(value = value, message = message, detail = detail)

  # Lógica JavaScript para actualizar la UI del modal
  percentage <- round(value * 100)

  shinyjs::runjs(
    paste0(
      'document.getElementById("ID_progress_message").innerHTML = "<b>', message, '</b>";',
      'document.getElementById("ID_progress_detail").innerHTML = "', detail, '";',
      'document.getElementById("ID_progress_bar").style.width = "', percentage, '%";'
    )
  )
}



fn_app_find_file_recursive <- function(root_dir, filename) {
  # Listar todos los archivos recursivamente
  all_files <- list.files(
    path = root_dir,
    pattern = filename,        # Filtrar por nombre
    recursive = TRUE,          # Búsqueda recursiva
    full.names = TRUE,         # Paths completos
    ignore.case = TRUE         # Insensible a mayúsculas
  )

  # Filtrar solo los que terminan exactamente con el nombre
  matching_files <- all_files[
    tolower(basename(all_files)) == tolower(filename)
  ]

  return(matching_files)
}
