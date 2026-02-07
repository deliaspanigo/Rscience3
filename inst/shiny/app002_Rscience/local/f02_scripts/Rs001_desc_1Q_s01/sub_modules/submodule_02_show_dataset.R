submodule_02_show_dataset_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # CSS para ocultar pestañas si es necesario (mantengo la estructura por si usas el debug_toggle)
    shiny::tags$head(
      shiny::tags$style(htmltools::HTML(sprintf("
        #%s.hide-tabs .nav-tabs { display: none !important; }
        #%s.hide-tabs .card-header { display: none !important; }
        #%s.hide-tabs { border-top: none !important; }
      ", ns("wrapper"), ns("wrapper"), ns("wrapper"))))
    ),
    shiny::div(
      id = ns("wrapper"),
      class = "hide-tabs",
      bslib::navset_tab(
        id = ns("dataset_tabset"),
        selected = "view_tab",

        # --- MAIN VIEW PANEL ---
        bslib::nav_panel(
          title = "Data Explorer",
          value = "view_tab",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              shiny::div(class = "d-flex justify-content-between align-items-center",
                         shiny::span(shiny::icon("table"), "Dataset Viewer"),
                         shiny::textOutput(ns("status_text"))
              )
            ),
            # UI dinámica que alterna entre la tabla y el mensaje de error
            shiny::uiOutput(ns("display_logic_gate"))
          )
        ),

        # --- DEBUG PANEL ---
        bslib::nav_panel(
          title = "Debug",
          value = "debug_tab",
          shiny::verbatimTextOutput(ns("debug_pack"))
        )
      )
    )
  )
}

submodule_02_show_dataset_server <- function(id, OR_01_import_dataset, debug_toggle = shiny::reactive({FALSE})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. DATA AVAILABILITY CHECK ---
    # Verificamos si el bundle existe y si contiene un dataframe válido
    is_data_ready <- shiny::reactive({
      bundle <- OR_01_import_dataset()
      !is.null(bundle) && is.data.frame(bundle$my_dataset) && nrow(bundle$my_dataset) > 0
    })

    # --- 2. VISIBILITY LOGIC (DEBUG) ---
    shiny::observe({
      if (isTRUE(debug_toggle())) {
        shinyjs::removeClass(id = "wrapper", class = "hide-tabs")
      } else {
        shinyjs::addClass(id = "wrapper", class = "hide-tabs")
        bslib::nav_select("dataset_tabset", "view_tab")
      }
    })

    # --- 3. DYNAMIC UI GATE ---
    output$display_logic_gate <- shiny::renderUI({
      if (is_data_ready()) {
        # Si hay datos, mostramos el contenedor de la tabla
        DT::DTOutput(ns("main_table"))
      } else {
        # Si no hay datos, mostramos un aviso ameno
        bslib::layout_column_wrap(
          width = 1,
          shiny::div(
            class = "text-center py-5",
            shiny::icon("circle-exclamation", class = "fa-3x text-warning mb-3"),
            shiny::h4("No Dataset Available"),
            shiny::p("Please import or select a dataset in the previous step to see the results here.", class = "text-muted")
          )
        )
      }
    })

    # --- 4. TABLE & STATUS ---
    output$main_table <- DT::renderDT({
      shiny::req(is_data_ready())

      DT::datatable(
        OR_01_import_dataset()$my_dataset,
        filter = "top",
        selection = "none",
        extensions = c("FixedHeader", "Scroller"),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          deferRender = TRUE,
          dom = 'lfrtip',
          style = "bootstrap5"
        )
      )
    })

    output$status_text <- shiny::renderText({
      if (is_data_ready()) {
        df <- OR_01_import_dataset()$my_dataset
        paste("Records:", nrow(df), "| Columns:", ncol(df))
      } else {
        "Status: Waiting for data..."
      }
    })

    # --- 5. DEBUG & RETURN ---
    output$debug_pack <- shiny::renderPrint({ utils::str(OR_01_import_dataset()) })

    # Retornamos simplemente la señal de si está listo para que el padre la use
    return(is_data_ready)
  })
}
