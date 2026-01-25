library(shiny)
library(bslib)
library(shinycssloaders)

# UI del Orquestador
module_orchestrator_01_import_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("col_01")),
    div(
      style = "min-height: 400px; margin-top: 20px;",
      withSpinner(uiOutput(ns("centralized_preview_ui")), type = 6, color = "#2c3e50")
    ),
    br(),
    uiOutput(ns("list_btn"))
  )
}





module_orchestrator_01_import_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- FASE 1: ESTADO ---
    rv <- reactiveValues(
      ui_state = "edit",
      instantiated_servers = list()
    )

    # Enviamos el estado a la UI para el conditionalPanel
    output$current_ui_state <- renderText({ rv$ui_state })
    outputOptions(output, "current_ui_state", suspendWhenHidden = FALSE)

    resources <- list(
      "data_source_R"    = list(ui = module_import_01_RDataset_ui, srv = module_import_01_RDataset_server),
      "data_source_xlsx" = list(ui = module_import_02_xlsx_ui,      srv = module_import_02_xlsx_server)
    )

    # --- FASE 2: DISPATCHER LÓGICO ---
    observe({
      req(input$selected_data_source)
      src <- input$selected_data_source
      if (is.null(rv$instantiated_servers[[src]])) {
        rv$instantiated_servers[[src]] <- resources[[src]]$srv(paste0("mod_", src), show_my_table = FALSE)
      }
    })

    current_active_data <- reactive({
      req(input$selected_data_source)
      src <- input$selected_data_source
      server_res <- rv$instantiated_servers[[src]]
      if (is.null(server_res)) return(NULL)
      server_res()
    })

    # --- FASE 3: LÓGICA DE BOTONES ---
    observeEvent(input$btn_lock, {
      data_info <- current_active_data()
      if (!is.null(data_info$my_dataset)) {
        rv$ui_state <- "locked"
      } else {
        showNotification("Seleccione un dataset válido.", type = "warning")
      }
    })

    observeEvent(input$btn_edit, { rv$ui_state <- "edit" })
    observeEvent(input$btn_reset, { session$reload() })

    # --- FASE 4: RENDERIZADO UI ---
    output$col_01 <- renderUI({
      card(
        card_header("Import Control Center"),
        card_body(
          style = "overflow: visible;",

          # PANEL DE EDICIÓN
          conditionalPanel(
            condition = sprintf("output['%s'] == 'edit'", ns("current_ui_state")),
            tagList(
              div(
                # CAMBIO AQUÍ: 'align-items: flex-start' para pegar todo arriba
                style = "display: flex; gap: 20px; align-items: flex-start; justify-content: flex-start; overflow: visible;",

                div(style = "width: auto; min-width: 200px;",
                    selectizeInput(ns("selected_data_source"), "Choose Method:",
                                   choices = c("Select..." = "", names(resources)),
                                   selected = input$selected_data_source,
                                   options = list(dropdownParent = "body"))
                ),

                div(style = "width: auto; overflow: visible;",
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'data_source_R'", ns("selected_data_source")),
                      module_import_01_RDataset_ui(ns("mod_data_source_R"))
                    ),
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'data_source_xlsx'", ns("selected_data_source")),
                      module_import_02_xlsx_ui(ns("mod_data_source_xlsx"))
                    )
                )
              )
            )
          ),

          # PANEL DE BLOQUEO
          conditionalPanel(
            condition = sprintf("output['%s'] == 'locked'", ns("current_ui_state")),
            uiOutput(ns("summary_locked_ui"))
          )
        )
      )
    })

    output$summary_locked_ui <- renderUI({
      info <- current_active_data()
      req(info)
      div(class = "p-3 bg-light border border-success rounded",
          h5("Source Locked", class = "text-success"),
          p(tags$b("Method: "), input$selected_data_source),
          p(tags$b("Dataset: "), info$name),
          p(tags$small("Rows: ", nrow(info$my_dataset)))
      )
    })

    # Previsualización y Botonera (permanecen similares)
    output$centralized_preview_ui <- renderUI({
      info <- current_active_data()
      req(info$my_dataset)
      card(card_header(paste("Preview:", info$name)), tableOutput(ns("main_table_output")))
    })

    output$main_table_output <- renderTable({
      req(current_active_data()$my_dataset)
      head(current_active_data()$my_dataset, 5)
    })

    output$list_btn <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock"), "", icon = icon("check"), class = paste("btn-success", if(is_locked) "disabled")),
        actionButton(ns("btn_edit"), "", icon = icon("pen"), class = paste("btn-warning", if(!is_locked) "disabled")),
        actionButton(ns("btn_reset"), "", icon = icon("rotate"), class = "btn-danger")
      )
    })

    return(current_active_data)
  })
}
