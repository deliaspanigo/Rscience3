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
          card_header("Selection Filters"),
          layout_column_wrap(
            width = 1/3,
            selectizeInput(ns("sel_category"), "1. Category", choices = ""),
            selectizeInput(ns("sel_tool_id"),  "2. Tool",     choices = ""),
            selectizeInput(ns("sel_script"),   "3. Script",   choices = "")
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
          style = "min-height: 120px; padding: 20px 0;",
          card_header("Selection Summary"),
          layout_column_wrap(
            width = 1/3,
            div(
              tags$b("Category: "),
              p(style = "margin-top: 10px; font-size: 1.2em;",
                span(class = "badge bg-primary", style = "font-size: 1em; padding: 8px 12px;", id = ns("summary_category")))
            ),
            div(
              tags$b("Tool: "),
              p(style = "margin-top: 10px; font-size: 1.2em;",
                span(class = "badge bg-secondary", style = "font-size: 1em; padding: 8px 12px;", id = ns("summary_tool")))
            ),
            div(
              tags$b("Script: "),
              p(style = "margin-top: 10px; font-size: 1.2em;",
                span(class = "badge bg-info", style = "font-size: 1em; padding: 8px 12px;", id = ns("summary_script")))
            )
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

module_tool_selector_server <- function(id, config_path = "tools_config_DEV.yml") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. CONFIG & STATE ---
    config_data <- reactiveFileReader(1000, session, config_path, yaml::read_yaml)
    tools_list  <- reactive({ unname(config_data()$tools) })
    cat_details <- reactive({ config_data()$cat_code })
    rv          <- reactiveValues(ui_state = "edit", is_done = FALSE)

    # Mostrar panel de edición al inicio
    observe({
      shinyjs::show("panel_edit")
    })

    # --- 2. INITIALIZATION (Conteo Inicial y Bloqueo) ---
    observe({
      t_list <- tools_list()
      req(length(t_list) > 0)

      # Extraemos categorías únicas de forma limpia
      cats_code <- sapply(t_list, function(x) x$cat_code)
      cats_name <- sapply(t_list, function(x) x$category)
      unique_indices <- which(!duplicated(cats_code))

      final_choices <- sapply(unique_indices, function(i) {
        code <- cats_code[i]
        count <- sum(cats_code == code)
        paste0(cats_name[i], " (", count, ")")
      })

      final_cats <- setNames(unname(cats_code[unique_indices]), unname(final_choices))

      updateSelectizeInput(session, "sel_category",
                           label = paste0("1. Category (", length(final_cats), ")"),
                           choices = c("", final_cats),
                           server = TRUE,
                           options = list(placeholder = 'Select Category...', dropdownParent = "body"))

      updateSelectizeInput(session, "sel_tool_id", label = "2. Tool", choices = "", server = TRUE,
                           options = list(placeholder = '(Locked - Complete step 1)'))
      updateSelectizeInput(session, "sel_script", label = "3. Script", choices = "", server = TRUE,
                           options = list(placeholder = '(Locked - Complete step 2)'))
    })

    # --- 3. CASCADING LOGIC ---
    observeEvent(input$sel_category, {
      if (input$sel_category == "") {
        updateSelectizeInput(session, "sel_tool_id", label = "2. Tool", choices = "", server = TRUE,
                             options = list(placeholder = '(Locked - Complete step 1)'))
        return()
      }

      filtered <- Filter(function(x) x$cat_code == input$sel_category, tools_list())

      tool_labels <- sapply(filtered, function(x) {
        n_scripts <- length(x$folder_scripts)
        paste0(x$name, " (", n_scripts, ")")
      })
      tool_ids <- sapply(filtered, function(x) x$id)

      tool_choices <- setNames(unname(tool_ids), unname(tool_labels))

      updateSelectizeInput(session, "sel_tool_id",
                           label = paste0("2. Tool (", length(filtered), ")"),
                           choices = c("", tool_choices), server = TRUE,
                           options = list(placeholder = 'Select Tool...', dropdownParent = "body"))
    }, ignoreInit = TRUE)

    observeEvent(input$sel_tool_id, {
      if (input$sel_tool_id == "") {
        updateSelectizeInput(session, "sel_script", label = "3. Script", choices = "", server = TRUE,
                             options = list(placeholder = '(Locked - Complete step 2)'))
        return()
      }

      matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
      req(length(matches) > 0)
      scripts <- names(matches[[1]]$folder_scripts)

      updateSelectizeInput(session, "sel_script",
                           label = paste0("3. Script (", length(scripts), ")"),
                           choices = c("", unname(scripts)), server = TRUE,
                           options = list(placeholder = 'Select Script...', dropdownParent = "body"))
    }, ignoreInit = TRUE)

    # --- 4. BUTTONS & UI STATE ---
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

        # Actualizar los textos del resumen
        if (input$sel_category != "") {
          t_list <- tools_list()
          cats_name <- sapply(t_list, function(x) x$category)
          cats_code <- sapply(t_list, function(x) x$cat_code)
          # Encontrar la categoría correspondiente
          matching_cats <- cats_name[cats_code == input$sel_category]
          if(length(matching_cats) > 0) {
            cat_name <- matching_cats[1]
            shinyjs::html("summary_category", cat_name)
          }
        }

        if (input$sel_tool_id != "") {
          matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
          if(length(matches) > 0) {
            shinyjs::html("summary_tool", matches[[1]]$name)
          }
        }

        if (input$sel_script != "") {
          shinyjs::html("summary_script", input$sel_script)
        }

        # Cambiar visibilidad de paneles
        shinyjs::hide("panel_edit")
        shinyjs::show("panel_summary")
      }
    })

    observeEvent(input$btn_edit_tool, {
      rv$ui_state <- "edit"
      rv$is_done <- FALSE

      # Cambiar visibilidad de paneles
      shinyjs::hide("panel_summary")
      shinyjs::show("panel_edit")
    })

    observeEvent(input$btn_reset_tool, {
      # Resetear valores
      updateSelectizeInput(session, "sel_category", selected = "")
      updateSelectizeInput(session, "sel_tool_id", selected = "")
      updateSelectizeInput(session, "sel_script", selected = "")

      rv$ui_state <- "edit"
      rv$is_done <- FALSE

      # Cambiar visibilidad de paneles
      shinyjs::hide("panel_summary")
      shinyjs::show("panel_edit")
    })

    # --- 5. DISPLAYS (Details) ---
    output$details_display_ui <- renderUI({
      req(input$sel_category, input$sel_category != "")
      desc_cat <- cat_details()[[input$sel_category]]$description %||% "No context info available."
      ui_list <- list(card(card_header("Category Context"), p(desc_cat)))

      if (nzchar(input$sel_tool_id %||% "")) {
        matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
        if(length(matches) > 0) {
          node <- matches[[1]]
          ui_list[[2]] <- card(
            card_header("Tool Info", class="bg-primary text-white"),
            h4(node$name), p(node$description_long)
          )
          if (nzchar(input$sel_script %||% "")) {
            s_info <- node$folder_scripts[[input$sel_script]]
            is_locked <- rv$ui_state == "locked"
            ui_list[[3]] <- card(
              class = if(is_locked) "border-success shadow" else "",
              card_header(
                if(is_locked) span(icon("check-circle"), " READY") else "Script Details",
                class = if(is_locked) "bg-success text-white" else "bg-secondary text-white"
              ),
              p(tags$b("Folder: "), tags$code(s_info$folder_script)),
              p(tags$b("Path: "), tags$code(s_info$special_module_file_path)),
              hr(),
              p(if(is_locked) tags$i(s_info$description_long) else s_info$description_short)
            )
          }
        }
      }
      tagList(ui_list)
    })

    # --- 6. RETURN ---
    return(reactive({
      if (!rv$is_done) return(list(is_done = FALSE))
      node <- Filter(function(x) x$id == input$sel_tool_id, tools_list())[[1]]
      script_info <- node$folder_scripts[[input$sel_script]]
      list(
        is_done = TRUE, category = input$sel_category, tool_id = input$sel_tool_id,
        tool_name = node$name, script_key = input$sel_script,
        folder_script = script_info$folder_script, special_path = script_info$special_module_file_path
      )
    }))
  })
}
