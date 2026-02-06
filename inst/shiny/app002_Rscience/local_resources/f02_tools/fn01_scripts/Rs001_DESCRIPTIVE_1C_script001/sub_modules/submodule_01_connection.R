submodule_01_connection_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("debug_conexion_dashboard"))
}

submodule_01_connection_server <- function(id, OR_01_import_dataset, debug_toggle = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. DATA EXTRACTION ---
    # Usamos req() dentro de los reactivos para evitar errores de cascada
    current_bundle <- reactive({
      req(OR_01_import_dataset())
      OR_01_import_dataset()
    })

    actual_df <- reactive({
      req(is.data.frame(current_bundle()$my_dataset))
      current_bundle()$my_dataset
    })

    # --- 2. CONNECTION DASHBOARD ---
    output$debug_conexion_dashboard <- renderUI({
      # Si no hay datos aún, mostrar un mensaje neutro o nada
      if(is.null(OR_01_import_dataset())) return(p("No data connected."))

      df    <- actual_df()
      ready <- isTRUE(current_bundle()$ready)

      bslib::layout_column_wrap(
        width = 1/2,
        bslib::value_box(
          title = "Hub Connection",
          value = if(ready) "CONNECTED" else "FAIL",
          theme = if(ready) "success" else "danger",
          showcase = icon("plug")
        ),
        bslib::value_box(
          title = "Detected Rows",
          value = if(ready) nrow(df) else 0,
          theme = "secondary",
          showcase = icon("table")
        )
      )
    })
  })
}
