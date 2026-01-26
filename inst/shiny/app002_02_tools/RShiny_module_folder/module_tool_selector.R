#' Tool Selector Orchestrator
#' Handles YAML-based tool discovery, filtering, and confirmation.

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

    div(
      id = ns("panel_edit"),
      card(
        style = "overflow: visible;",
        card_header("Selection Filters"),
        layout_column_wrap(
          width = 1/3,
          selectizeInput(ns("sel_category"), "1. Category", choices = ""),
          selectizeInput(ns("sel_tool_id"),  "2. Tool",     choices = ""),
          selectizeInput(ns("sel_script"),   "3. Script",   choices = "")
        )
      )
    ),

    uiOutput(ns("ui_locked_summary")),
    br(),
    uiOutput(ns("tool_control_btns")),
    br(),
    withSpinner(uiOutput(ns("details_display_ui")), type = 8)
  )
}

module_tool_selector_server <- function(id, config_path = "tools_config_DEV.yml") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. CONFIG & STATE ---
    config_data <- reactiveFileReader(1000, session, config_path, yaml::read_yaml)
    tools_list  <- reactive({ config_data()$tools })
    cat_details <- reactive({ config_data()$cat_code })

    rv <- reactiveValues(
      ui_state = "edit", # 'edit' o 'locked'
      is_done = FALSE
    )

    # --- 2. INITIALIZATION & SEQUENTIAL LOCKING ---

    # Al cargar el módulo, configuramos el estado inicial
    observe({
      t_list <- tools_list()
      req(length(t_list) > 0)

      cats_code <- sapply(t_list, function(x) x$cat_code)
      cats_name <- sapply(t_list, function(x) x$category)
      final_cats <- setNames(cats_code[!duplicated(cats_code)],
                             cats_name[!duplicated(cats_code)])

      # Nivel 1: Siempre habilitado al inicio
      updateSelectizeInput(session, "sel_category",
                           choices = c("", final_cats),
                           server = TRUE,
                           options = list(placeholder = '1. Select Category...', dropdownParent = "body"))

      # Niveles 2 y 3: Nacen bloqueados con texto explicativo
      updateSelectizeInput(session, "sel_tool_id", choices = "", server = TRUE,
                           options = list(placeholder = '(Locked - Complete step 1)', dropdownParent = "body"))
      updateSelectizeInput(session, "sel_script", choices = "", server = TRUE,
                           options = list(placeholder = '(Locked - Complete step 2)', dropdownParent = "body"))

      shinyjs::disable("sel_tool_id")
      shinyjs::disable("sel_script")
    })

    # --- 3. CASCADING LOGIC (Step-by-Step) ---

    # Nivel 2: Se activa cuando el Nivel 1 tiene selección
    observeEvent(input$sel_category, {
      if (input$sel_category == "") {
        updateSelectizeInput(session, "sel_tool_id", choices = "", server = TRUE,
                             options = list(placeholder = '(Locked - Complete step 1)'))
        updateSelectizeInput(session, "sel_script", choices = "", server = TRUE,
                             options = list(placeholder = '(Locked - Complete step 2)'))
        shinyjs::disable("sel_tool_id")
        shinyjs::disable("sel_script")
        return()
      }

      # Cargamos herramientas de la categoría elegida
      filtered <- Filter(function(x) x$cat_code == input$sel_category, tools_list())
      choices  <- setNames(sapply(filtered, function(x) x$id), sapply(filtered, function(x) x$name))

      updateSelectizeInput(session, "sel_tool_id",
                           choices = c("", choices), server = TRUE,
                           options = list(placeholder = '2. Select Tool...', dropdownParent = "body"))

      shinyjs::enable("sel_tool_id")
      # El nivel 3 se mantiene bloqueado hasta que elijan el nivel 2
      shinyjs::disable("sel_script")
      updateSelectizeInput(session, "sel_script", choices = "", server = TRUE,
                           options = list(placeholder = '(Locked - Complete step 2)'))
    }, ignoreInit = TRUE)

    # Nivel 3: Se activa cuando el Nivel 2 tiene selección
    observeEvent(input$sel_tool_id, {
      if (input$sel_tool_id == "") {
        updateSelectizeInput(session, "sel_script", choices = "", server = TRUE,
                             options = list(placeholder = '(Locked - Complete step 2)'))
        shinyjs::disable("sel_script")
        return()
      }

      matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
      req(length(matches) > 0)

      # Cargamos scripts de la herramienta elegida
      updateSelectizeInput(session, "sel_script",
                           choices = c("", names(matches[[1]]$folder_scripts)), server = TRUE,
                           options = list(placeholder = '3. Select Script...', dropdownParent = "body"))

      shinyjs::enable("sel_script")
    }, ignoreInit = TRUE)

    # --- 4. ACTION BUTTONS & VISIBILITY ---

    output$tool_control_btns <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock_tool"), " Confirm Tool", icon = icon("check"),
                     class = paste("btn-success", if(is_locked) "disabled")),
        actionButton(ns("btn_edit_tool"), " Change Tool", icon = icon("pen"),
                     class = paste("btn-warning", if(!is_locked) "disabled")),
        actionButton(ns("btn_reset_tool"), " Reset All", icon = icon("rotate"), class = "btn-danger")
      )
    })

    observeEvent(input$btn_lock_tool, {
      if(nzchar(input$sel_script %||% "")) {
        rv$ui_state <- "locked"
        rv$is_done <- TRUE
        # Al confirmar, bloqueamos incluso el primer nivel
        shinyjs::disable("sel_category")
        shinyjs::disable("sel_tool_id")
        shinyjs::disable("sel_script")
      } else {
        showNotification("Please finish all 3 steps.", type = "warning")
      }
    })

    observeEvent(input$btn_edit_tool, {
      rv$ui_state <- "edit"
      rv$is_done <- FALSE
      # Al volver a editar, habilitamos según lo que esté lleno
      shinyjs::enable("sel_category")
      if (input$sel_category != "") shinyjs::enable("sel_tool_id")
      if (input$sel_tool_id != "")  shinyjs::enable("sel_script")
    })

    observeEvent(input$btn_reset_tool, { session$reload() })

    # --- 5. SUMMARY & DETAILS ---

    output$ui_locked_summary <- renderUI({
      req(rv$ui_state == "locked", input$sel_tool_id)
      tool_node <- Filter(function(x) x$id == input$sel_tool_id, tools_list())[[1]]

      card(
        class = "bg-light border-success shadow-sm",
        card_header(span(icon("lock"), " Tool Locked", class="text-success")),
        layout_column_wrap(
          width = 1/3,
          p(tags$b("Category: "), span(class="badge bg-primary", tool_node$category)),
          p(tags$b("Tool: "),     span(class="badge bg-secondary", tool_node$name)),
          p(tags$b("Script: "),   span(class="badge bg-info", input$sel_script))
        )
      )
    })

    output$details_display_ui <- renderUI({
      req(input$sel_category, input$sel_category != "")

      # Info de Categoría
      desc_cat <- cat_details()[[input$sel_category]]$description %||% "No info."
      ui_list <- list(card(card_header("Category Context"), p(desc_cat)))

      if (nzchar(input$sel_tool_id %||% "")) {
        matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
        if(length(matches) > 0) {
          node <- matches[[1]]
          ui_list[[2]] <- card(card_header("Tool Info", class="bg-primary text-white"),
                               h4(node$name), p(node$description_long))

          if (nzchar(input$sel_script %||% "")) {
            s_info <- node$folder_scripts[[input$sel_script]]
            is_locked <- rv$ui_state == "locked"
            ui_list[[3]] <- card(
              class = if(is_locked) "border-success shadow" else "",
              card_header(if(is_locked) "READY" else "Script Details",
                          class=if(is_locked) "bg-success text-white" else "bg-secondary text-white"),
              p(tags$b("Module Path: "), tags$code(s_info$special_module_file_path)),
              p(if(is_locked) tags$i(s_info$description_long) else s_info$description_short)
            )
          }
        }
      }
      tagList(ui_list)
    })

    # --- 6. DATA RETURN ---
    return(reactive({
      if (!rv$is_done) return(list(is_done = FALSE))

      node <- Filter(function(x) x$id == input$sel_tool_id, tools_list())[[1]]
      script_info <- node$folder_scripts[[input$sel_script]]

      list(
        is_done       = TRUE,
        category      = input$sel_category,
        tool_id       = input$sel_tool_id,
        tool_name     = node$name,
        script_key    = input$sel_script,
        folder_script = script_info$folder_script,
        special_path  = script_info$special_module_file_path
      )
    }))
  })
}
