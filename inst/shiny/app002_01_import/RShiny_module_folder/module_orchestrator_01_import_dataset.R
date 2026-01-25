library(shiny)
library(bslib)
library(shinycssloaders)
library(readxl)

# ==========================================
# 1. SUB-MODULE: R DATASET
# ==========================================
module_import_01_RDataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns("dataset_sel"),
      label = "Select an R dataset:",
      choices = c("Select one..." = "", "iris", "mtcars", "quakes"),
      width = "fit-content",
      options = list(dropdownParent = "body")
    )
  )
}

module_import_01_RDataset_server <- function(id, show_my_table = TRUE) {
  moduleServer(id, function(input, output, session) {
    OR_import_dataset_01_RData <- reactive({
      # Default "Not Done"
      if (is.null(input$dataset_sel) || input$dataset_sel == "") {
        return(list(is_done = FALSE, my_dataset = NULL, name = NULL))
      }

      df <- get(input$dataset_sel, "package:datasets")
      list(
        is_done = TRUE,
        my_dataset = df,
        name = input$dataset_sel,
        timestamp = Sys.time()
      )
    })
    return(OR_import_dataset_01_RData)
  })
}

# ==========================================
# 2. SUB-MODULE: EXCEL
# ==========================================
module_import_02_xlsx_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; gap: 20px; align-items: flex-start; justify-content: flex-start;",
      div(
        style = "width: auto; min-width: 300px;",
        fileInput(ns("file_upload"), "Choose Excel File:", accept = c(".xlsx", ".xls"), buttonLabel = "Browse...")
      ),
      div(
        style = "width: auto; min-width: 200px;",
        uiOutput(ns("sheet_selector_ui"))
      )
    )
  )
}

module_import_02_xlsx_server <- function(id, show_my_table = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sheets_names <- reactive({
      req(input$file_upload)
      readxl::excel_sheets(input$file_upload$datapath)
    })

    output$sheet_selector_ui <- renderUI({
      req(sheets_names())
      selectizeInput(ns("sheet_sel"), "Select Sheet:",
                     choices = c("Choose sheet..." = "", sheets_names()),
                     options = list(dropdownParent = "body"))
    })

    OR_import_dataset_02_xlsx <- reactive({
      if (is.null(input$file_upload) || is.null(input$sheet_sel) || input$sheet_sel == "") {
        return(list(is_done = FALSE, my_dataset = NULL, name = NULL))
      }

      df <- readxl::read_excel(input$file_upload$datapath, sheet = input$sheet_sel)
      list(
        is_done = TRUE,
        my_dataset = df,
        name = input$file_upload$name,
        sheet = input$sheet_sel
      )
    })
    return(OR_import_dataset_02_xlsx)
  })
}

# ==========================================
# 3. ORCHESTRATOR MODULE
# ==========================================
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

    # --- PHASE 1: STATE ---
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

    # --- PHASE 2: DISPATCHER ---
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

    # --- PHASE 3: AGNOSTIC VALIDATION ---
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
        showNotification(
          ui = paste("Incomplete: The selected source is not ready."),
          type = "warning"
        )
      }
    })

    observeEvent(input$btn_edit, {
      rv$ui_state <- "edit"
      showNotification("Editing mode: Data source unlocked.", type = "warning")
    })

    observeEvent(input$btn_reset, { session$reload() })

    # --- PHASE 4: UI RENDERING ---
    output$col_01 <- renderUI({
      card(
        card_header("Import Control Center"),
        card_body(
          style = "overflow: visible;",

          # EDIT PANEL
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

          # LOCKED PANEL
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

    output$centralized_preview_ui <- renderUI({
      # Only show preview if locked
      req(rv$ui_state == "locked")
      info <- current_active_data()
      req(info$my_dataset)
      card(card_header(paste("Preview:", info$name)), tableOutput(ns("main_table_output")))
    })

    output$main_table_output <- renderTable({
      req(rv$ui_state == "locked")
      info <- current_active_data()
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

    # --- FINAL RETURN (The Gatekeeper) ---
    return(reactive({
      if (rv$ui_state == "locked") {
        return(current_active_data())
      } else {
        return(list(is_done = FALSE, my_dataset = NULL, name = NULL))
      }
    }))
  })
}
