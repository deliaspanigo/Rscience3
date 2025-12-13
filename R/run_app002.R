# En el archivo R/run_app.R de tu paquete:

#' Ejecutar la aplicación Shiny
#'
#' @description
#' Esta función inicia la aplicación Shiny incluida en este paquete.
#'
#' @return No retorna valor.
#' @export
run_app002 <- function() {
  app_dir <- system.file("shiny", "app002", "app00_central", package = "Rscience3")

  if (app_dir == "") {
    stop("No se pudo encontrar la aplicación. Pruebe reinstalando el paquete.", call. = FALSE)
  }

  shiny::runApp(app_dir)
}
