submodule_05_settings_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
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
        id = ns("settings_selector"),
        selected = "the_selector",
        bslib::nav_panel(
          title = "Action", value = "the_selector",
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              bslib::card_header("Plot Appearance"),
              shiny::div(
                id = ns("selection_controls_wrapper"),
                # Utiliza colourpicker para una selección visual de colores
                colourpicker::colourInput(
                  ns("selected_color"),
                  "Select Plot Accent Color:",
                  value = "#0d6efd", # Azul por defecto (Bootstrap primary)
                  showColour = "both",
                  palette = "square"
                )
              ),
              shiny::hr(),
              bslib::layout_column_wrap(
                width = 1/3,
                shiny::actionButton(ns("btn_accept"), "Confirm", icon = shiny::icon("check"), class = "btn-success"),
                shiny::actionButton(ns("btn_edit"),   "Edit",    icon = shiny::icon("pen"),   class = "btn-warning"),
                shiny::actionButton(ns("btn_reset"),  "Reset",   icon = shiny::icon("trash"), class = "btn-danger")
              )
            ),
            bslib::card(
              bslib::card_header("Configuration Detail"),
              shiny::uiOutput(ns("selection_detail_display")),
              bslib::card_footer(shiny::textOutput(ns("selection_status_text")))
            )
          )
        ),
        bslib::nav_panel(
          title = "Debug Settings", value = "settings_DEBUG",
          bslib::card(bslib::card_header("Settings Bundle"), shiny::verbatimTextOutput(ns("debug_ANCESTRAL_list")))
        )
      )
    )
  )
}

submodule_05_settings_server <- function(id, OR_01_import_dataset, debug_toggle = shiny::reactive({FALSE})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    PACK_master <- shiny::reactiveVal(list(ready = FALSE, color = "#0d6efd"))

    # --- 1. VISIBILITY LOGIC ---
    shiny::observe({
      if (isTRUE(debug_toggle())) {
        shinyjs::removeClass(id = "wrapper", class = "hide-tabs")
      } else {
        shinyjs::addClass(id = "wrapper", class = "hide-tabs")
        bslib::nav_select("settings_selector", "the_selector")
      }
    })

    # --- 2. BUTTONS LOGIC ---
    shiny::observeEvent(input$btn_accept, {
      PACK_master(list(
        ready = TRUE,
        color = input$selected_color,
        timestamp = Sys.time()
      ))

      shinyjs::disable("selection_controls_wrapper")
      shinyjs::disable("btn_accept")
      shinyjs::enable("btn_edit")
      shiny::showNotification("Color settings locked.", type = "message")
    })

    shiny::observeEvent(input$btn_edit, {
      PACK_master(list(ready = FALSE, color = input$selected_color))
      shinyjs::enable("selection_controls_wrapper")
      shinyjs::enable("btn_accept")
      shinyjs::disable("btn_edit")
    })

    shiny::observeEvent(input$btn_reset, {
      # Reseteamos al color azul original
      colourpicker::updateColourInput(session, "selected_color", value = "#0d6efd")
      PACK_master(list(ready = FALSE, color = "#0d6efd"))
      shinyjs::enable("selection_controls_wrapper")
      shinyjs::enable("btn_accept")
      shinyjs::disable("btn_edit")
    })

    # --- 3. RENDERING ---
    output$selection_detail_display <- shiny::renderUI({
      confirmed <- isTRUE(PACK_master()$ready)

      if (!confirmed) {
        shiny::div(class="alert alert-info", shiny::icon("info-circle"), "Choose a color and confirm.")
      } else {
        shiny::div(
          shiny::p(shiny::strong("Confirmed Color:")),
          shiny::div(
            style = sprintf("width: 100%%; height: 50px; background-color: %s; border-radius: 5px; border: 1px solid #ddd; margin-bottom: 10px;",
                            PACK_master()$color)
          ),
          shiny::code(PACK_master()$color)
        )
      }
    })

    output$selection_status_text <- shiny::renderText({
      if(isTRUE(PACK_master()$ready)) {
        paste("Selected hex:", PACK_master()$color)
      } else {
        "Status: Pending Confirmation"
      }
    })

    output$debug_ANCESTRAL_list <- shiny::renderPrint({ utils::str(PACK_master()) })

    # Retorno reactivo para que el Padre use el color en el histograma
    return(shiny::reactive({ PACK_master() }))
  })
}
