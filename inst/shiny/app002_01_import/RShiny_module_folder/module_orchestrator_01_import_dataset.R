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
    uiOutput(ns("list_btn")),
    br()
  )
}





module_orchestrator_01_import_dataset_ui04 <- function(id) {
  ns <- NS(id)
  tagList(
    # Floating Toolbar Style
    div(class = "p-3 mb-3 bg-white shadow-sm rounded-3 border",
        div(class = "row align-items-center",
            div(class = "col-md-8", uiOutput(ns("col_01"))),
            div(class = "col-md-4 text-end", uiOutput(ns("list_btn")))
        )
    ),
    # Large Preview Area
    div(style = "height: calc(100vh - 250px); overflow-y: auto;",
        withSpinner(uiOutput(ns("centralized_preview_ui")), type = 6)
    )
  )
}


#######################################################################
module_orchestrator_01_import_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- PHASE 1: STATE ---
    rv <- reactiveValues(
      ui_state = "edit",
      instantiated_servers = list()
    )

    # Bridge for conditionalPanel in UI
    output$current_ui_state <- renderText({ rv$ui_state })
    outputOptions(output, "current_ui_state", suspendWhenHidden = FALSE)

    # Registry of sub-modules
    resources <- list(
      "data_source_R"    = list(ui = module_import_01_RDataset_ui, srv = module_import_01_RDataset_server),
      "data_source_xlsx" = list(ui = module_import_02_xlsx_ui,      srv = module_import_02_xlsx_server)
    )

    # --- PHASE 2: DYNAMIC DISPATCHER ---
    observe({
      req(input$selected_data_source)
      src <- input$selected_data_source
      if (is.null(rv$instantiated_servers[[src]])) {
        # Initialize sub-module server on demand
        rv$instantiated_servers[[src]] <- resources[[src]]$srv(paste0("mod_", src), show_my_table = FALSE)
      }
    })

    # Reactive to get the data from the currently active sub-module
    current_active_data <- reactive({
      req(input$selected_data_source)
      src <- input$selected_data_source
      server_res <- rv$instantiated_servers[[src]]
      if (is.null(server_res)) return(list(is_done = FALSE))
      server_res()
    })

    # --- PHASE 3: ACTIONS (Lock, Edit, Reset) ---
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

    # --- PHASE 4: UI RENDERING (Orchestrator Controls) ---
    output$col_01 <- renderUI({
      card(
        card_header("Import Control Center"),
        card_body(
          style = "overflow: visible;",

          # EDITING MODE
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

          # LOCKED MODE (Summary)
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

    # --- PHASE 5: CENTRALIZED PREVIEW (With Horizontal Scroll) ---
    output$centralized_preview_ui <- renderUI({
      info <- current_active_data()
      req(info$is_done, info$my_dataset)

      is_locked <- rv$ui_state == "locked"

      # Aesthetic classes from the UI tags$style
      header_class <- if(is_locked) "preview-header-locked" else "preview-header-edit"
      card_class   <- if(is_locked) "preview-card-locked" else "shadow-sm"

      card(
        class = card_class,
        card_header(
          class = header_class,
          div(class = "d-flex justify-content-between align-items-center",
              div(icon(if(is_locked) "lock" else "eye"), paste(" Preview:", info$name)),
              if(is_locked)
                span(class="badge bg-white text-success", "CONFIRMED")
              else
                span(class="badge bg-warning text-dark", "DRAFT PREVIEW")
          )
        ),
        # WRAPPER FOR HORIZONTAL SCROLL
        div(style = "overflow-x: auto; width: 100%; background-color: white;",
            tableOutput(ns("main_table_output"))
        )
      )
    })

    output$main_table_output <- renderTable({
      info <- current_active_data()
      req(info$is_done, info$my_dataset)
      head(info$my_dataset, 5)
    },
    # text-nowrap is key: it forces the scroll by preventing text wrapping
    class = "table table-hover table-striped table-sm text-nowrap mb-0",
    width = "100%",
    align = "l")

    # --- PHASE 6: BUTTON TOOLBAR ---
    output$list_btn <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock"), "Lock", icon = icon("check"),
                     class = paste("btn-success", if(is_locked) "disabled")),
        actionButton(ns("btn_edit"), "Edit", icon = icon("pen"),
                     class = paste("btn-warning", if(!is_locked) "disabled")),
        actionButton(ns("btn_reset"), "Reset", icon = icon("rotate"),
                     class = "btn-danger")
      )
    })

    # --- FINAL RETURN (Gatekeeper for the rest of the App) ---
    return(reactive({
      if (rv$ui_state == "locked") {
        return(current_active_data())
      } else {
        return(list(is_done = FALSE, my_dataset = NULL, name = NULL))
      }
    }))
  })
}
