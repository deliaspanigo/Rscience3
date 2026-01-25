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

# Server del Orquestador
# Server del Orquestador
module_orchestrator_01_import_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    vector_data_choices <- c("R Data" = "data_source_R", "Excel files" = "data_source_xlsx")
    rv <- reactiveValues(ui_state = "edit", is_done = FALSE)

    OR_r    <- module_import_01_RDataset_server("id_r", show_my_table = FALSE)
    OR_xlsx <- module_import_02_xlsx_server("id_xlsx", show_my_table = FALSE)

    # Creamos un output que el navegador pueda leer para los paneles condicionales
    output$current_state <- renderText({ rv$ui_state })
    outputOptions(output, "current_state", suspendWhenHidden = FALSE)

    current_active_data <- reactive({
      # Evitamos que se rompa si no hay selección inicial
      if (is.null(input$selected_data_source) || input$selected_data_source == "") return(NULL)

      if (input$selected_data_source == "data_source_R") {
        OR_r()
      } else if (input$selected_data_source == "data_source_xlsx") {
        OR_xlsx()
      } else {
        NULL
      }
    })

    # --- LÓGICA DE BOTONES ---
    observeEvent(input$btn_lock, {
      data_info <- current_active_data()
      # Validación: debe haber un dataset y debe ser dataframe
      if (!is.null(data_info$my_dataset) && is.data.frame(data_info$my_dataset)) {
        rv$ui_state <- "locked"
        rv$is_done <- TRUE
      } else {
        showNotification("Error: Selecciona un dataset válido antes de bloquear.", type = "warning")
      }
    })

    observeEvent(input$btn_edit, {
      rv$ui_state <- "edit"
      rv$is_done <- FALSE
    })

    observeEvent(input$btn_reset, { session$reload() })

    # --- RENDERIZADO UI ---
    # --- RENDERIZADO UI ---
    output$col_01 <- renderUI({
      card(
        card_header("Import Control Center"),
        card_body(
          style = "overflow: visible;",

          # PANEL DE EDICIÓN: Se oculta mediante JS, manteniendo los valores intactos
          conditionalPanel(
            condition = sprintf("output['%s'] == 'edit'", ns("current_state")),
            div(
              style = "display: flex; gap: 20px; align-items: flex-end; justify-content: flex-start; overflow: visible;",
              div(style = "width: auto; min-width: 200px;",
                  # Recuperamos tu selectizeInput original con sus opciones
                  selectizeInput(
                    ns("selected_data_source"),
                    "Choose your method:",
                    choices = c("Select one..." = "", vector_data_choices),
                    selected = input$selected_data_source,
                    width = "fit-content",
                    options = list(dropdownParent = "body")
                  )
              ),
              div(style = "width: auto; overflow: visible;",
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'data_source_R'", ns("selected_data_source")),
                    module_import_01_RDataset_ui(ns("id_r"))
                  ),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'data_source_xlsx'", ns("selected_data_source")),
                    module_import_02_xlsx_ui(ns("id_xlsx"))
                  )
              )
            )
          ),

          # PANEL BLOQUEADO: Solo se muestra cuando rv$ui_state es 'locked'
          conditionalPanel(
            condition = sprintf("output['%s'] == 'locked'", ns("current_state")),
            uiOutput(ns("summary_locked_ui"))
          )
        )
      )
    })

    # Este output es vital para que el conditionalPanel detecte el cambio de estado
    output$current_state <- renderText({ rv$ui_state })
    outputOptions(output, "current_state", suspendWhenHidden = FALSE)

    # Resumen que se muestra cuando está bloqueado
    output$summary_locked_ui <- renderUI({
      data_info <- current_active_data()
      req(data_info)
      div(class = "p-3 bg-light border rounded",
          h5("Data Source Locked", class = "text-success"),
          p(tags$b("Method: "), input$selected_data_source),
          p(tags$b("Dataset: "), data_info$name),
          p(tags$small("Rows: ", nrow(data_info$my_dataset), " | Cols: ", ncol(data_info$my_dataset)))
      )
    })

    # Vista previa centralizada
    output$centralized_preview_ui <- renderUI({
      data_info <- current_active_data()
      req(data_info$my_dataset)
      card(
        full_screen = TRUE,
        card_header(paste("Preview:", data_info$name)),
        tableOutput(ns("main_table_output"))
      )
    })

    output$main_table_output <- renderTable({
      req(current_active_data()$my_dataset)
      head(current_active_data()$my_dataset, 5)
    })

    # Botonera
    output$list_btn <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock"), "", icon = icon("check"),
                     class = paste("btn-success", if(is_locked) "disabled"),
                     disabled = if(is_locked) TRUE else NULL),
        actionButton(ns("btn_edit"), "", icon = icon("pen"),
                     class = paste("btn-warning", if(!is_locked) "disabled"),
                     disabled = if(!is_locked) TRUE else NULL),
        actionButton(ns("btn_reset"), "", icon = icon("rotate"), class = "btn-danger")
      )
    })

    return(reactive({
      if (rv$ui_state != "locked") return(list("is_done" = FALSE))
      info <- current_active_data()
      list("is_done" = TRUE, "my_dataset" = info$my_dataset, "name" = info$name)
    }))
  })
}
