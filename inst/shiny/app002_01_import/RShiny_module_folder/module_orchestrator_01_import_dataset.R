module_orchestrator_01_import_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Internal CSS for the smooth color transition
    tags$head(
      tags$style(HTML("
        .preview-card-locked { border: 2px solid #198754 !important; transition: all 0.3s ease; }
        .preview-header-locked { background-color: #198754 !important; color: white !important; transition: all 0.3s ease; }
        .preview-header-edit { background-color: #f8f9fa; color: #212529; transition: all 0.3s ease; }
      "))
    ),
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

    rv <- reactiveValues(
      ui_state = "edit",
      instantiated_servers = list()
    )

    output$current_ui_state <- renderText({ rv$ui_state })
    outputOptions(output, "current_ui_state", suspendWhenHidden = FALSE)

    resources <- list(
      "data_source_R"    = list(ui = module_import_01_RDataset_ui, srv = module_import_01_RDataset_server),
      "data_source_xlsx" = list(ui = module_import_02_xlsx_ui,      srv = module_import_02_xlsx_server)
    )

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
      if (is.null(server_res)) return(list(is_done = FALSE))
      server_res()
    })

    observeEvent(input$btn_lock, {
      if (is.null(input$selected_data_source) || input$selected_data_source == "") {
        showNotification("Error: Please select a Method first.", type = "error")
        return()
      }
      data_info <- current_active_data()
      if (isTRUE(data_info$is_done)) {
        rv$ui_state <- "locked"
        showNotification("Success: Data source locked.", type = "message")
      } else {
        showNotification(ui = "Incomplete: The selected source is not ready.", type = "warning")
      }
    })

    observeEvent(input$btn_edit, {
      rv$ui_state <- "edit"
      showNotification("Editing mode: Data source unlocked.", type = "warning")
    })

    observeEvent(input$btn_reset, { session$reload() })

    output$col_01 <- renderUI({
      card(
        card_header("Import Control Center"),
        card_body(
          style = "overflow: visible;",
          conditionalPanel(
            condition = sprintf("output['%s'] == 'edit'", ns("current_ui_state")),
            div(
              style = "display: flex; gap: 20px; align-items: flex-start; justify-content: flex-start; overflow: visible;",
              div(style = "width: auto; min-width: 200px;",
                  selectizeInput(ns("selected_data_source"), "Choose Method:",
                                 choices = c("Select..." = "", "R Data" = "data_source_R", "Excel files" = "data_source_xlsx"),
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
          ),
          conditionalPanel(
            condition = sprintf("output['%s'] == 'locked'", ns("current_ui_state")),
            uiOutput(ns("summary_locked_ui"))
          )
        )
      )
    })

    output$summary_locked_ui <- renderUI({
      info <- current_active_data()
      req(info$is_done)
      div(class = "p-3 bg-light border border-success rounded",
          h5("Source Locked", class = "text-success"),
          p(tags$b("Dataset: "), info$name),
          p(tags$small("Rows: ", nrow(info$my_dataset), " | Columns: ", ncol(info$my_dataset)))
      )
    })

    # --- DYNAMIC PREVIEW: Changes color when Locked ---
    output$centralized_preview_ui <- renderUI({
      info <- current_active_data()
      req(info$is_done, info$my_dataset)

      is_locked <- rv$ui_state == "locked"

      # Determine CSS classes based on state
      header_class <- if(is_locked) "preview-header-locked" else "preview-header-edit"
      card_class   <- if(is_locked) "preview-card-locked" else ""

      card(
        class = card_class,
        card_header(
          class = header_class,
          div(class = "d-flex justify-content-between align-items-center",
              div(icon(if(is_locked) "lock" else "eye"), paste(" Preview:", info$name)),
              if(is_locked)
                span(class="badge bg-white text-success", "CONFIRMED")
              else
                span(class="badge bg-warning text-dark", "DRAFT")
          )
        ),
        tableOutput(ns("main_table_output"))
      )
    })

    output$main_table_output <- renderTable({
      info <- current_active_data()
      req(info$is_done, info$my_dataset)
      head(info$my_dataset, 5)
    })

    output$list_btn <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock"), "Lock", icon = icon("check"), class = paste("btn-success", if(is_locked) "disabled")),
        actionButton(ns("btn_edit"), "Edit", icon = icon("pen"), class = paste("btn-warning", if(!is_locked) "disabled")),
        actionButton(ns("btn_reset"), "Reset", icon = icon("rotate"), class = "btn-danger")
      )
    })

    return(reactive({
      if (rv$ui_state == "locked") {
        return(current_active_data())
      } else {
        return(list(is_done = FALSE, my_dataset = NULL, name = NULL))
      }
    }))
  })
}
