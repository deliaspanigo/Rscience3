#' @title set_reactive_values_from_list
#' @description Sobrescribe los valores de un objeto reactiveValues (rv)
#'              con los valores proporcionados en una lista (data_list).
#'              Solo actualiza las claves que existen en la lista de datos.
#'
#' @param rv Un objeto reactiveValues de Shiny (pasado por referencia).
#' @param data_list Una lista R estándar cuyos elementos se usarán para actualizar rv.
#' @return El objeto rv modificado (modifica por referencia, pero se devuelve para consistencia).
set_reactive_values_from_list <- function(rv, data_list) {

  # 1. Verificar que ambos son objetos válidos
  if (!is.list(data_list)) {
    stop("El argumento 'data_list' debe ser una lista R estándar.")
  }

  # 2. Iterar sobre los nombres de la lista de datos
  for (name in names(data_list)) {
    # 3. Asignar el valor de la lista al elemento correspondiente en reactiveValues
    #    Esto automáticamente crea/actualiza el elemento en rv
    rv[[name]] <- data_list[[name]]
  }

  # Nota: Aunque rv se modifica por referencia, devolverlo es una buena práctica.
  invisible(rv)
}
