module_tool_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        /* Forzar el estado deshabilitado en Selectize */
        .selectize-control.disabled {
          opacity: 1 !important; /* Mantenemos opacidad para que se vea el texto gris */
        }

        .selectize-control.disabled .selectize-input,
        .selectize-control.disabled .selectize-input input {
          background-color: #f2f2f2 !important; /* Gris claro de fondo */
          color: #a1a1a1 !important;           /* Texto gris */
          cursor: not-allowed !important;      /* Cursor de prohibido */
          border-color: #d1d1d1 !important;
          box-shadow: none !important;
        }

        .selectize-control.disabled .selectize-input * {
          cursor: not-allowed !important;
        }
      ")))
    ),

    shinyjs::useShinyjs(),

    # Panel de edición (selectize) - SIEMPRE presente pero oculto/mostrado
    shinyjs::hidden(
      div(
        id = ns("panel_edit"),
        card(
          style = "overflow: visible;",
          card_header("Tools & Scripts"),
          layout_column_wrap(
            width = 1/3,
            # COLUMNA 1: Category
            div(
              selectizeInput(ns("sel_category"), "1. Category", choices = "", width = "100%"),
              tags$small(id = ns("msg_category"), class = "text-muted",
                         style = "display: block; margin-top: -15px; margin-bottom: 10px; min-height: 15px;")
            ),
            # COLUMNA 2: Tool
            div(
              selectizeInput(ns("sel_tool_id"), "2. Tool", choices = "", width = "100%"),
              tags$small(id = ns("msg_tool"), class = "text-muted",
                         style = "display: block; margin-top: -15px; margin-bottom: 10px; min-height: 15px;")
            ),
            # COLUMNA 3: Script
            div(
              selectizeInput(ns("sel_script"), "3. Script", choices = "", width = "100%"),
              tags$small(id = ns("msg_script"), class = "text-muted",
                         style = "display: block; margin-top: -15px; margin-bottom: 10px; min-height: 15px;")
            )
          )
        )
      )
    ),

    # Panel de resumen (texto) - SIEMPRE presente pero oculto/mostrado
    shinyjs::hidden(
      div(
        id = ns("panel_summary"),
        card(
          class = "bg-light border-success shadow-sm",
          card_header("Tools & Scripts (User selection)"),
          card_body(
            style = "padding: 15px;", # Espaciado interno controlado
            uiOutput(ns("summary_tool_ui")) # Aquí se inyectará el texto grande
          )
        )
      )
    ),

    br(),
    uiOutput(ns("tool_control_btns")),
    br(),
    withSpinner(uiOutput(ns("details_display_ui")), type = 8)
  )
}

mod_tools_hub_server <- function(id, config_path = "tools_config_DEV.yml",
                                 check_external = reactive({TRUE}),
                                 debug_toggle = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. CONFIG & STATE ---
    config_data <- reactiveFileReader(1000, session, config_path, function(path) {
      tryCatch(yaml::read_yaml(path), error = function(e) list(tools = list(), cat_code = list()))
    })

    tools_list  <- reactive({
      req(config_data()$tools)
      unname(config_data()$tools)
    })

    rv <- reactiveValues(ui_state = "edit", is_done = FALSE)

    # --- 2. CARGA INICIAL DE CATEGORÍAS (Solo una vez o cuando el archivo cambie) ---
    observeEvent(tools_list(), {
      t_list <- tools_list()
      req(length(t_list) > 0)

      cats_code <- sapply(t_list, function(x) x$cat_code)
      cats_name <- sapply(t_list, function(x) x$category)
      unique_idx <- which(!duplicated(cats_code))
      final_cats <- setNames(cats_code[unique_idx], cats_name[unique_idx])

      # Importante: update sin disparar eventos innecesarios
      updateSelectizeInput(session, "sel_category",
                           choices = c("Select Category..." = "", final_cats),
                           server = TRUE)
    }, priority = 10)

    # --- 3. CASCADA: HERRAMIENTAS (Con protección de freeze) ---
    observeEvent(input$sel_category, {
      # Si está vacío, reseteamos hijos y salimos
      if (is.null(input$sel_category) || input$sel_category == "") {
        updateSelectizeInput(session, "sel_tool_id", choices = character(0))
        updateSelectizeInput(session, "sel_script", choices = character(0))
        return()
      }

      # Congelamos el valor del hijo para que no se dispare su observeEvent
      freezeReactiveValue(input, "sel_tool_id")

      filtered <- Filter(function(x) x$cat_code == input$sel_category, tools_list())
      tool_choices <- setNames(sapply(filtered, function(x) x$id), sapply(filtered, function(x) x$name))

      updateSelectizeInput(session, "sel_tool_id",
                           choices = c("Select Tool..." = "", tool_choices),
                           server = TRUE)
    })

    # --- 4. CASCADA: SCRIPTS ---
    observeEvent(input$sel_tool_id, {
      if (is.null(input$sel_tool_id) || input$sel_tool_id == "") {
        updateSelectizeInput(session, "sel_script", choices = character(0))
        return()
      }

      freezeReactiveValue(input, "sel_script")

      matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
      req(length(matches) > 0)
      scripts <- names(matches[[1]]$folder_scripts)

      updateSelectizeInput(session, "sel_script",
                           choices = c("Select Script..." = "", scripts),
                           server = TRUE)
    })

    # --- 5. UI DINÁMICA DEL WRAPPER ---
    output$dynamic_wrapper <- renderUI({
      # Aquí usamos conditionalPanel interno o simples tags de Shiny para alternar vistas
      # basándonos en rv$ui_state sin destruir los inputs

      clase_actual <- if (isTRUE(debug_toggle())) "" else "hide-tabs"

      div(
        id = ns("wrapper"),
        class = clase_actual,
        bslib::navset_tab(
          id = ns("tools_workflow_steps"),
          selected = "step03_action",

          bslib::nav_panel("01. Config", value = "step01_check", verbatimTextOutput(ns("debug_config"))),

          bslib::nav_panel(
            title = "03. Action: Tools",
            value = "step03_action",
            br(),
            # Usamos CSS para ocultar/mostrar en lugar de IF de R para no matar los inputs
            div(id = ns("container_edit"),
                style = if(rv$ui_state == "edit") "display: block;" else "display: none;",
                bslib::card(
                  style = "overflow: visible;",
                  card_header("Tools & Scripts Selection"),
                  bslib::layout_column_wrap(
                    width = 1/3,
                    div(selectizeInput(ns("sel_category"), "1. Category", choices = NULL)),
                    div(selectizeInput(ns("sel_tool_id"), "2. Tool", choices = NULL)),
                    div(selectizeInput(ns("sel_script"), "3. Script", choices = NULL))
                  )
                )
            ),
            div(id = ns("container_summary"),
                style = if(rv$ui_state == "locked") "display: block;" else "display: none;",
                bslib::card(
                  class = "bg-light border-success",
                  card_header("Selected Tool Configuration"),
                  card_body(uiOutput(ns("summary_tool_ui")))
                )
            ),
            br(),
            uiOutput(ns("tool_control_btns"))
          )
        )
      )
    })

    # Botones y lógica de bloqueo (Igual que antes pero cambiando rv$ui_state)
    observeEvent(input$btn_lock_tool, {
      req(input$sel_script)
      rv$ui_state <- "locked"
      rv$is_done <- TRUE
    })

    observeEvent(input$btn_edit_tool, {
      rv$ui_state <- "edit"
      rv$is_done <- FALSE
    })

    # ... Resto de outputs (summary_tool_ui, tool_control_btns, etc.)
  })
}
