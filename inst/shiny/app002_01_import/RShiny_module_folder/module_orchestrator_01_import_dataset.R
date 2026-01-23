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
module_orchestrator_01_import_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vector_data_choices <- c("R Data" = "data_source_R", "Excel files" = "data_source_xlsx")
    rv <- reactiveValues(ui_state = "edit", is_done = FALSE)

    OR_r    <- module_import_01_RData_server("id_r", show_my_table = FALSE)
    OR_xlsx <- module_import_02_xlsx_server("id_xlsx", show_my_table = FALSE)

    current_active_data <- reactive({
      # Aseguramos que el input existe antes de evaluar
      req(input$selected_data_source)

      # Usamos una estructura switch o if/else para mayor claridad
      res <- if (input$selected_data_source == "data_source_R") {
        OR_r()
      } else if (input$selected_data_source == "data_source_xlsx") {
        OR_xlsx()
      } else {
        # Este es el "cartel" de error en consola
        cat("\n[!] ERROR CRÍTICO: Fuente de datos desconocida:", input$selected_data_source, "\n")
        warning("Se intentó acceder a una opción no válida en el Orquestador.")

        # Retornamos NULL o lanzamos un error silencioso para Shiny
        return(NULL)
      }

      return(res)
    })

    # --- LÓGICA DE BOTONES ---
    observeEvent(input$btn_lock, {
      data_info <- current_active_data()
      if (!is.null(data_info$my_dataset) && is.data.frame(data_info$my_dataset)) {
        rv$ui_state <- "locked"
        rv$is_done <- TRUE
      } else {
        rv$is_done <- FALSE
        showNotification("Error: Dataset inválido o no detectado.", type = "error")
      }
    })

    observeEvent(input$btn_edit, {
      rv$ui_state <- "edit"
      rv$is_done <- FALSE # Al editar, el objeto externo se "limpia"
    })

    observeEvent(input$btn_reset, {
      session$reload()
    })

    # --- RENDERIZADO UI ---
    output$list_btn <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock"), "", icon = icon("check"),
                     class = paste("btn-success", if(is_locked) "disabled"),
                     disabled = if(is_locked) TRUE else NULL),
        actionButton(ns("btn_edit"), "", icon = icon("pen"),
                     class = paste("btn-warning", if(rv$ui_state == "edit") "disabled"),
                     disabled = if(rv$ui_state == "edit") TRUE else NULL),
        actionButton(ns("btn_reset"), "", icon = icon("rotate"), class = "btn-danger")
      )
    })

    output$col_01 <- renderUI({
      card(
        card_header("Import Control Center"),
        card_body(
          style = "overflow: visible;",
          if (rv$ui_state == "edit") {
            tagList(
              div(
                style = "display: flex; gap: 20px; align-items: flex-end; justify-content: flex-start; overflow: visible;",
                div(style = "width: auto; min-width: 200px;",
                    selectizeInput(ns("selected_data_source"), "Choose your method:",
                                   choices = vector_data_choices,
                                   selected = input$selected_data_source,
                                   width = "fit-content",
                                   options = list(dropdownParent = "body"))
                ),
                div(style = "width: auto; overflow: visible;",
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'data_source_R'", ns("selected_data_source")),
                      module_import_01_RData_ui(ns("id_r"))
                    ),
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'data_source_xlsx'", ns("selected_data_source")),
                      module_import_02_xlsx_ui(ns("id_xlsx"))
                    )
                )
              )
            )
          } else {
            data_info <- current_active_data()
            div(class = "p-3 bg-light border rounded",
                h5("Data Source Locked", class = "text-success"),
                p(tags$b("Method: "), input$selected_data_source),
                p(tags$b("Dataset: "), data_info$name),
                p(tags$small("Rows: ", nrow(data_info$my_dataset), " | Cols: ", ncol(data_info$my_dataset)))
            )
          }
        )
      )
    })

    output$centralized_preview_ui <- renderUI({
      req(input$selected_data_source)
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

    # --- SALIDA REACTIVA CONDICIONAL ---
    return(reactive({
      if (rv$ui_state != "locked") {
        return(list("is_done" = FALSE, "my_dataset" = NULL, "name" = NULL))
      }
      info <- current_active_data()
      list(
        "is_done" = TRUE,
        "my_dataset" = info$my_dataset,
        "name" = info$name,
        "timestamp" = Sys.time()
      )
    }))
  })
}
