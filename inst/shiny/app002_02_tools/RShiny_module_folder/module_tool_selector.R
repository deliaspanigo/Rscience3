library(shiny)
library(bslib)
library(yaml)

# ==============================================================================
# MÓDULO: TOOL SELECTOR
# ==============================================================================

module_tool_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui_filter_container")),
    br(),
    uiOutput(ns("tool_control_btns")),
    br(),
    uiOutput(ns("details_display_ui"))
  )
}



module_tool_selector_server <- function(id, config_path = "tools_config_DEV.yml") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. CARGA INTERNA DEL YAML ---
    config_data <- reactiveFileReader(
      intervalMillis = 1000,
      session = session,
      filePath = config_path,
      readFunc = yaml::read_yaml
    )

    tools_list  <- reactive({ config_data()$tools })
    cat_details <- reactive({ config_data()$cat_code })

    # --- 2. VALORES REACTIVOS DE ESTADO ---
    rv <- reactiveValues(
      ui_state = "edit",
      is_done = FALSE
    )

    # --- 3. UI DINÁMICA ---
    output$ui_filter_container <- renderUI({
      if (rv$ui_state == "edit") {
        card(
          style = "overflow: visible;",
          card_header("Selection Filters"),
          layout_column_wrap(
            width = 1/3,
            selectizeInput(ns("sel_category"), "1. Category", choices = NULL,
                           options = list(placeholder = 'Select...', dropdownParent = "body")),
            selectizeInput(ns("sel_tool_id"), "2. Tool", choices = NULL,
                           options = list(placeholder = 'Select...', dropdownParent = "body")),
            selectizeInput(ns("sel_script"), "3. Script", choices = NULL,
                           options = list(placeholder = 'Select...', dropdownParent = "body"))
          )
        )
      } else {
        req(input$sel_category, input$sel_tool_id)
        # Extraer nombres para el resumen
        t_list <- tools_list()
        tool_node <- Filter(function(x) x$id == input$sel_tool_id, t_list)[[1]]

        card(
          class = "bg-light border-success",
          card_header(span(icon("lock"), " Selected Configuration")),
          layout_column_wrap(
            width = 1/3,
            p(tags$b("Category: "), span(class="badge bg-primary", tool_node$category)),
            p(tags$b("Tool: "), span(class="badge bg-secondary", tool_node$name)),
            p(tags$b("Script: "), span(class="badge bg-info", input$sel_script))
          )
        )
      }
    })

    # --- 4. BOTONERA ---
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

    # --- 5. EVENTOS ---
    observeEvent(input$btn_lock_tool, {
      if(!is.null(input$sel_script) && input$sel_script != "") {
        rv$ui_state <- "locked"; rv$is_done <- TRUE
      } else {
        rv$is_done <- FALSE
        showNotification("Please select a script.", type = "warning")
      }
    })
    observeEvent(input$btn_edit_tool, { rv$ui_state <- "edit"; rv$is_done <- FALSE })
    observeEvent(input$btn_reset_tool, { session$reload() })

    # --- 6. CASCADA SELECTORES (Ajustada para nueva estructura YAML) ---
    observe({
      req(rv$ui_state == "edit")
      t_list <- tools_list()
      # Extraemos códigos y nombres de las listas internas
      cats_code <- sapply(t_list, function(x) x$cat_code)
      cats_name <- sapply(t_list, function(x) x$category)

      final_cats <- cats_code[!duplicated(cats_code)]
      names(final_cats) <- cats_name[!duplicated(cats_code)]
      updateSelectizeInput(session, "sel_category", choices = c("", final_cats), server = TRUE)
    })

    observeEvent(input$sel_category, {
      req(rv$ui_state == "edit")
      if (is.null(input$sel_category) || input$sel_category == "") return()
      filtered <- Filter(function(x) x$cat_code == input$sel_category, tools_list())
      choices <- setNames(sapply(filtered, function(x) x$id), sapply(filtered, function(x) x$name))
      updateSelectizeInput(session, "sel_tool_id", choices = c("", choices), server = TRUE)
    })

    observeEvent(input$sel_tool_id, {
      req(rv$ui_state == "edit")
      if (is.null(input$sel_tool_id) || input$sel_tool_id == "") return()
      tool_node <- Filter(function(x) x$id == input$sel_tool_id, tools_list())[[1]]
      updateSelectizeInput(session, "sel_script", choices = c("", names(tool_node$folder_scripts)), server = TRUE)
    })

    # --- 7. DETALLES ---
    output$details_display_ui <- renderUI({
      req(input$sel_category, input$sel_category != "")
      desc_cat <- cat_details()[[input$sel_category]]$description %||% "No info"
      ui_list <- list(card(card_header("Category Context"), p(desc_cat)))

      if (!is.null(input$sel_tool_id) && input$sel_tool_id != "") {
        node <- Filter(function(x) x$id == input$sel_tool_id, tools_list())[[1]]
        ui_list[[2]] <- card(card_header("Tool Info", class="bg-primary text-white"), h4(node$name), p(node$description_long))

        if (!is.null(input$sel_script) && input$sel_script != "") {
          s_info <- node$folder_scripts[[input$sel_script]]
          is_locked <- rv$ui_state == "locked"
          ui_list[[3]] <- card(
            class = if(is_locked) "border-success shadow" else "",
            card_header(if(is_locked) "READY" else "Script Info", class=if(is_locked) "bg-success text-white" else "bg-secondary text-white"),
            p(tags$b("Folder: "), tags$code(s_info$folder_script)),
            p(tags$b("Module Path: "), tags$code(s_info$special_module_file_path)),
            if(is_locked) p(tags$i(s_info$description_long)) else p(s_info$description_short)
          )
        }
      }
      tagList(ui_list)
    })

    # --- RETORNO DE DATOS EXTENDIDO ---
    return(reactive({
      folder_path <- NULL
      module_path <- NULL

      if (!is.null(input$sel_tool_id) && input$sel_tool_id != "" &&
          !is.null(input$sel_script) && input$sel_script != "") {
        node <- Filter(function(x) x$id == input$sel_tool_id, tools_list())[[1]]
        script_info <- node$folder_scripts[[input$sel_script]]
        folder_path <- script_info$folder_script
        module_path <- script_info$special_module_file_path
      }

      list(
        is_done       = rv$is_done,
        cat           = input$sel_category,
        tool          = input$sel_tool_id,
        script_key    = input$sel_script,
        folder_script = folder_path,
        special_path  = module_path
      )
    }))
  })
}
