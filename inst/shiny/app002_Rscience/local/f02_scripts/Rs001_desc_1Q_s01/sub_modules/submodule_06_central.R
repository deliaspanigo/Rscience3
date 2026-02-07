submodule_06_central_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::navset_card_tab(
      id = ns("central_tabs"),
      title = "Master Configuration Hub",

      # Pestaña 1: Solo información de estado del Selector
      bslib::nav_panel(
        title = "Selector Status",
        value = "status_selector",
        icon = shiny::icon("check-double"),
        bslib::card(
          bslib::card_header("Variable Selection Data"),
          shiny::verbatimTextOutput(ns("view_selector_data"))
        )
      ),

      # Pestaña 2: Solo información de estado de Settings
      bslib::nav_panel(
        title = "Settings Status",
        value = "status_settings",
        icon = shiny::icon("palette"),
        bslib::card(
          bslib::card_header("Visual Configuration Data"),
          shiny::verbatimTextOutput(ns("view_settings_data"))
        )
      ),

      # Pestaña 3: El objeto final que verá el Padre
      bslib::nav_panel(
        title = "Unified Master PACK",
        value = "status_master",
        icon = shiny::icon("box-open"),
        bslib::card(
          bslib::card_header("Final Aggregated Object"),
          shiny::verbatimTextOutput(ns("view_master_pack"))
        )
      )
    )
  )
}

submodule_06_central_server <- function(id, PACK_selector, PACK_settings, debug_toggle = reactive({FALSE})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. LÓGICA DE UNIFICACIÓN ---
    # Este es el corazón del módulo: fundir dos fuentes en una.
    PACK_central <- shiny::reactive({
      # Consumimos los reactivos que vienen de fuera
      sel_data <- PACK_selector()
      set_data <- PACK_settings()

      # El ready global depende de la confirmación de ambos
      is_all_ready <- isTRUE(sel_data$ready) && isTRUE(set_data$ready)

      list(
        ready     = is_all_ready,
        timestamp = Sys.time(),
        selector  = sel_data,
        settings  = set_data
      )
    })

    # --- 2. DISPLAYS (UI PROPIA) ---
    output$view_selector_data <- shiny::renderPrint({
      utils::str(PACK_selector())
    })

    output$view_settings_data <- shiny::renderPrint({
      utils::str(PACK_settings())
    })

    output$view_master_pack <- shiny::renderPrint({
      utils::str(PACK_central())
    })

    # --- 3. RETORNO ---
    # El padre ahora solo necesita observar este único objeto
    return(PACK_central)
  })
}
