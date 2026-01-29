#######################################################################
# MODULE: ORCHESTRATOR 01 - IMPORT DATASET
#######################################################################

module_orchestrator_01_import_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .preview-card-locked { border: 2px solid #198754 !important; transition: all 0.3s ease; }
        .preview-header-locked { background-color: #198754 !important; color: white !important; }
        .preview-header-edit { background-color: #f8f9fa; color: #212529; }
        /* Ajuste estético para que el placeholder se vea bien */
        .selectize-input::after { content: none !important; }
      "))
    ),
    shinyjs::useShinyjs(),

    # --- PANEL 1: EDICIÓN (CARD PRINCIPAL) ---
    div(
      id = ns("panel_edit"),
      card(
        card_header("Import Dataset"),
        card_body(
          style = "overflow: visible; min-height: 150px;",
          div(
            style = "display: flex; gap: 20px; align-items: flex-start;",
            div(style = "min-width: 220px;",
                # Placeholder configurado via options para que no sea una opción elegible
                selectizeInput(ns("selected_data_source"), "1. Choose a source:",
                               choices = NULL,
                               options = list(
                                 placeholder = 'Select one...',
                                 onInitialize = I('function() { this.setValue(""); }'),
                                 dropdownParent = "body"
                               ))
            ),
            div(style = "flex-grow: 1;",
                uiOutput(ns("submodule_uis"))
            )
          )
        )
      )
    ),

    # --- PANEL 2: RESUMEN (CARD BLOQUEADO) ---
    shinyjs::hidden(
      div(
        id = ns("panel_summary"),
        card(
          class = "bg-light border-success shadow-sm",
          card_header(
            div(class = "d-flex justify-content-between align-items-center",
                "Import Dataset (User selection)",
                icon("circle-check", class = "text-success"))
          ),
          card_body(
            style = "padding: 15px;",
            uiOutput(ns("summary_locked_ui"))
          )
        )
      )
    ),
    uiOutput(ns("list_btn")),
    br(),
    # --- ÁREA DE PREVIEW ---
    div(
      style = "min-height: 400px; margin-top: 20px;",
      withSpinner(uiOutput(ns("centralized_preview_ui")), type = 6, color = "#2c3e50")
    ),

    br()

  )
}

module_orchestrator_01_import_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # --- 1. REGISTRO DE RECURSOS ---
    resources <- list(
      "data_source_R"    = list(label = "R Data Objects", ui = module_import_01_RDataset_ui, srv = module_import_01_RDataset_server),
      "data_source_xlsx" = list(label = "xlsx Files",     ui = module_import_02_xlsx_ui,     srv = module_import_02_xlsx_server)
    )

    rv <- reactiveValues(
      ui_state = "edit",
      instantiated_servers = list()
    )

    # Inicializar Selectize (Placeholder limpio)
    observe({
      method_choices <- setNames(names(resources), sapply(resources, `[[`, "label"))
      updateSelectizeInput(session, "selected_data_source",
                           choices = method_choices,
                           selected = character(0),
                           server = TRUE)
    })

    # --- 2. DISPATCHER DE SUB-MÓDULOS ---
    output$submodule_uis <- renderUI({
      req(input$selected_data_source)
      src <- input$selected_data_source
      resources[[src]]$ui(ns(paste0("mod_", src)))
    })

    observe({
      req(input$selected_data_source)
      src <- input$selected_data_source
      if (is.null(rv$instantiated_servers[[src]])) {
        rv$instantiated_servers[[src]] <- resources[[src]]$srv(paste0("mod_", src), show_my_table = FALSE)
      }
    })

    # --- 3. GESTIÓN DE DATOS ---
    current_active_data <- reactive({
      req(input$selected_data_source)
      src <- input$selected_data_source
      server_res_reactive <- rv$instantiated_servers[[src]]
      req(server_res_reactive)

      data_list <- server_res_reactive()

      # Times
      init_time <- data_list$"dataset"$"init_time"
      end_time  <- data_list$"dataset"$"end_time"
      diff_secs <- data_list$"dataset"$"diff_secs"

      internal_is_done <-  data_list$"dataset"$"is_done"
      external_is_done <-  TRUE
      stone_is_done <- isTRUE(internal_is_done) && isTRUE(external_is_done)

      # Inyección de metadatos
      data_list <- fn3_IMPORT_set_import_data(data_list, category = NULL, field = "is_done",   value = stone_is_done)
      data_list <- fn3_IMPORT_set_import_data(data_list, category = NULL, field = "init_time", value = init_time)
      data_list <- fn3_IMPORT_set_import_data(data_list, category = NULL, field = "end_time",  value = end_time)
      data_list <- fn3_IMPORT_set_import_data(data_list, category = NULL, field = "diff_secs", value = diff_secs)

      data_list <- fn3_IMPORT_set_import_data(data_list, "orquestator_import", "name_internal", src)
      data_list <- fn3_IMPORT_set_import_data(data_list, "orquestator_import", "name_external", resources[[src]]$label)

      return(data_list)
    })

    # --- 4. RENDERS (ESTADO BLOQUEADO Y PREVIEW) ---
    output$summary_locked_ui <- renderUI({
      info <- current_active_data()
      req(info$dataset$is_done)

      div(
        style = "font-size: 1.15rem; line-height: 1.6; color: #2c3e50;",
        div(tags$b("Data source: "), span(style = "color: #1a1a1a;", info$orquestator_import$name_external)),
        div(tags$b("File/Object: "), span(style = "color: #1a1a1a;", info$dataset$label_file_name)),
        div(tags$b("Dimensions: "),  span(style = "color: #1a1a1a;",
                                          paste(info$dataset$rows, "rows ×", info$dataset$cols, "cols"))
        )
      )
    })

    output$centralized_preview_ui <- renderUI({
      info <- current_active_data()
      req(info$dataset$is_done, info$dataset$my_dataset)

      is_locked <- rv$ui_state == "locked"
      str_label <- paste0("Preview:", info$dataset$label_file_name)
      if(is_locked) str_label <- paste0(str_label, " (Confirmed)")
      # "&nbsp;&nbsp;
      card(
        class = if(is_locked) "preview-card-locked shadow" else "shadow-sm",
        card_header(
          if(is_locked) span(icon("check-circle"), str_label) else str_label,
          class = if(is_locked) "bg-success text-white" else "bg-secondary text-white"
          )

        ,
        div(style = "overflow-x: auto; width: 100%; background-color: white;",
            tableOutput(ns("main_table_output")))
      )

    })

    output$main_table_output <- renderTable({
      info <- current_active_data()
      req(info$dataset$is_done, info$dataset$my_dataset)
      head(info$dataset$my_dataset, 5)
    }, class = "table table-hover table-sm text-nowrap mb-0", width = "100%", align = "l")

    # --- 5. BOTONES DE ACCIÓN ---
    output$list_btn <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock"), " Confirm", icon = icon("check"),
                     class = paste("btn-success", if(is_locked) "disabled")),
        actionButton(ns("btn_edit"), " Edit", icon = icon("pen"),
                     class = paste("btn-warning", if(!is_locked) "disabled")),
        actionButton(ns("btn_reset"), " Reset All", icon = icon("rotate"),
                     class = "btn-danger")
      )
    })

    observeEvent(input$btn_lock, {
      info <- current_active_data()
      if (isTRUE(info$dataset$is_done)) {
        rv$ui_state <- "locked"
        shinyjs::hide("panel_edit")
        shinyjs::show("panel_summary")
        showNotification("Data source confirmed.", type = "message")
      } else {
        showNotification("The source is not ready. Please complete the steps.", type = "warning")
      }
    })

    observeEvent(input$btn_edit, {
      rv$ui_state <- "edit"
      shinyjs::hide("panel_summary")
      shinyjs::show("panel_edit")
    })

    observeEvent(input$btn_reset, { session$reload() })

    # --- 6. RETURN ---
    return(reactive({
      if (rv$ui_state == "locked") return(current_active_data())
      return(list(is_done = FALSE))
    }))
  })
}
