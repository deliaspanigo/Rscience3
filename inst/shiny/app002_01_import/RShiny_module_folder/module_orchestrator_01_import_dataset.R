#######################################################################
# MODULE: ORCHESTRATOR 01 - IMPORT DATASET
#######################################################################

module_orchestrator_01_import_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
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

module_orchestrator_01_import_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- PHASE 1: REGISTRY (The Only Place to Edit) ---
    resources <- list(
      "data_source_R"    = list(label = "R Data Objects", ui = module_import_01_RDataset_ui, srv = module_import_01_RDataset_server),
      "data_source_xlsx" = list(label = "Excel Files",    ui = module_import_02_xlsx_ui,     srv = module_import_02_xlsx_server)
    )

    # --- PHASE 2: STATE & DISPATCHER ---
    rv <- reactiveValues(
      ui_state = "edit",
      instantiated_servers = list() # Servers stay NULL until selected
    )

    output$current_ui_state <- renderText({ rv$ui_state })
    outputOptions(output, "current_ui_state", suspendWhenHidden = FALSE)

    # Lazy Server Loading: Only runs the selected module server
    observe({
      req(input$selected_data_source)
      src <- input$selected_data_source

      if (is.null(rv$instantiated_servers[[src]])) {
        # Initialize the specific server on demand
        rv$instantiated_servers[[src]] <- resources[[src]]$srv(paste0("mod_", src), show_my_table = FALSE)
      }
    })

    # --- PHASE 3: DATA HANDSHAKE ---
    current_active_data <- reactive({
      req(input$selected_data_source)
      src <- input$selected_data_source

      str_name_external <- resources[[src]]$"label"

      server_res_reactive <- rv$instantiated_servers[[src]]
      if (is.null(server_res_reactive)) return(NULL)

      # EXECUTE the reactive function to get the list
      data_list <- server_res_reactive()

      # Dynamic Metadata Injection
      data_list <- fn3_IMPORT_set_import_data(
        current_list = data_list,
        category     = "orquestator_import",
        field        = "name_internal",
        value        = src
      )

      data_list <- fn3_IMPORT_set_import_data(
        current_list = data_list,
        category     = "orquestator_import",
        field        = "name_external",
        value        = str_name_external
      )
      return(data_list)
    })

    # --- PHASE 4: AUTOMATIC UI GENERATION ---
    # Creates hidden containers for all modules; JS shows/hides them
    submodule_ui_panels <- div(
      lapply(names(resources), function(src_name) {
        conditionalPanel(
          condition = sprintf("input['%s'] == '%s'", ns("selected_data_source"), src_name),
          resources[[src_name]]$ui(ns(paste0("mod_", src_name)))
        )
      })
    )

    # --- PHASE 5: UI RENDERING ---
    output$col_01 <- renderUI({
      # Build Choices automatically from the 'resources' labels
      method_choices <- setNames(names(resources), sapply(resources, `[[`, "label"))

      card(
        card_header("Import Control Center"),
        card_body(
          style = "overflow: visible;",
          conditionalPanel(
            condition = sprintf("output['%s'] == 'edit'", ns("current_ui_state")),
            div(
              style = "display: flex; gap: 20px; align-items: flex-start;",
              div(style = "min-width: 200px;",
                  selectizeInput(ns("selected_data_source"), "Choose Method:",
                                 choices = c("Select..." = "", method_choices),
                                 selected = input$selected_data_source,
                                 options = list(dropdownParent = "body"))
              ),
              div(submodule_ui_panels) # The automatic panels
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
      req(info$dataset$is_done)
      div(class = "p-3 bg-light border border-success rounded",
          h5("Source Locked", class = "text-success"),
          p(tags$b("File/Object: "), info$dataset$label_file_name),
          p(tags$small("Dimensions: ", info$dataset$rows, " rows x ", info$dataset$cols, " cols"))
      )
    })

    output$centralized_preview_ui <- renderUI({
      info <- current_active_data()
      req(info$dataset$is_done, info$dataset$my_dataset)

      is_locked <- rv$ui_state == "locked"
      card(
        class = if(is_locked) "preview-card-locked" else "shadow-sm",
        card_header(
          class = if(is_locked) "preview-header-locked" else "preview-header-edit",
          div(class = "d-flex justify-content-between align-items-center",
              div(icon(if(is_locked) "lock" else "eye"), paste(" Preview:", info$dataset$label_file_name)),
              span(class = if(is_locked) "badge bg-white text-success" else "badge bg-warning text-dark",
                   if(is_locked) "CONFIRMED" else "DRAFT PREVIEW")
          )
        ),
        div(style = "overflow-x: auto; width: 100%; background-color: white;",
            tableOutput(ns("main_table_output")))
      )
    })

    output$main_table_output <- renderTable({
      info <- current_active_data()
      req(info$dataset$is_done, info$dataset$my_dataset)
      head(info$dataset$my_dataset, 5)
    }, class = "table table-hover table-striped table-sm text-nowrap mb-0", width = "100%", align = "l")

    # --- PHASE 6: BUTTONS & FINAL RETURN ---
    output$list_btn <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock"), "Lock", icon = icon("check"), class = paste("btn-success", if(is_locked) "disabled")),
        actionButton(ns("btn_edit"), "Edit", icon = icon("pen"), class = paste("btn-warning", if(!is_locked) "disabled")),
        actionButton(ns("btn_reset"), "Reset", icon = icon("rotate"), class = "btn-danger")
      )
    })

    observeEvent(input$btn_lock, {
      req(input$selected_data_source)
      info <- current_active_data()
      if (isTRUE(info$dataset$is_done)) {
        rv$ui_state <- "locked"
        showNotification("Success: Data source locked.", type = "message")
      } else {
        showNotification("Incomplete: The source is not ready.", type = "warning")
      }
    })

    observeEvent(input$btn_edit, { rv$ui_state <- "edit" })
    observeEvent(input$btn_reset, { session$reload() })

    return(reactive({
      if (rv$ui_state == "locked") return(current_active_data())
      return(NULL)
    }))
  })
}
