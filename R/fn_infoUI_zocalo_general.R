#' @export
fn_infoUI_zocalo_general <- function(data_obj) {
  req(data_obj)
  # req(length(data_obj) > 0)
  # list_safe <- data_obj
  list_safe <- tryCatch(data_obj, error = function(e) NULL)
  if (is.null(list_safe)) return("Sin datos.")

  # 2. Generar dinámicamente los elementos de la UI
  # Iteramos sobre los nombres (etiquetas) de la lista.
  info_elements <- lapply(names(list_safe), function(name) {
    # Evitar mostrar elementos con valor NULL o muy largos.
    value <- list_safe[[name]]

    # Formatear el valor: si es NULL o vacío, mostrar 'N/A'.
    # Si es un vector o lista, convertir a texto plano.
    if (is.null(value) || length(value) == 0) {
      display_value <- "N/A"
    } else {
      # Usar toString para manejar vectores o listas simples.
      display_value <- toString(value)

      # Opcional: Truncar valores muy largos (ej. rutas de archivo)
      if (nchar(display_value) > 80) {
        display_value <- paste0(substr(display_value, 1, 77), "...")
      }
    }

    # Crea el tag div para un par de Nombre/Valor
    div(class = "me-4 mb-2",
        # Formatea el nombre para que se vea bien (ej. 'data_source' -> 'Data Source')
        tags$b(style = "padding-left: 10px; text-transform: capitalize;", gsub("_", " ", name)),
        span(display_value, style = "font-family: monospace;")
    )
  })

  div(
    class = "p-3 rounded shadow-sm",
    style = "background: linear-gradient(to right, #f8f9fa, #ffffff);",

    # Título principal
    h4(
      class = "mb-3 pb-2",
      style = "border-bottom: 2px solid #0d6efd; color: #0d6efd;",
      icon("info-circle"),
      "Data Selection Metadata" # Título modificado para ser más genérico
    ),

    div(
      class = "mb-3 p-2 rounded",
      style = "background-color: rgba(13, 110, 253, 0.05); border-left: 4px solid #0d6efd;",

      h5(class = "text-primary",
         icon("database",
              style = "padding-left: 10px;",
              class = "me-2"), "Data Details"),

      # 3. Insertar la lista de elementos generados dinámicamente
      div(class = "d-flex flex-wrap",
          info_elements # Aquí se insertan todos los divs generados por lapply
      )
    )
  )
}
