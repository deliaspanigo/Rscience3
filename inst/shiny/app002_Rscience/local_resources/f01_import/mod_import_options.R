
# --- 1. SUB-MÓDULOS DE IMPORTACIÓN ---

mod_import_r_ui_step03_action <- function(id) {
  ns <- NS(id)
  vector_choices <- c("mtcars", "iris", "diamonds")
  # vector_choices <- c("Select one..." = "", vector_choices)
  selectizeInput(ns("r_dataset_choice"), "Choose a dataset:",
                 choices = vector_choices,
                 options = list(
                   placeholder = 'Select one...',
                   onInitialize = I('function() { this.setValue(""); }'),
                   dropdownParent = "body"
                 ))
  # shiny::selectInput(ns("r_dataset_choice"), "Select Object:",
  #                    choices = vector_choices)
}

mod_import_r_ui_step04_live <- function(id_instancia) {
  # Importante: Como los inputs viven dentro del sub-módulo,
  # necesitamos construir el ID manualmente o pasar el NS
  ns_inst <- NS(id_instancia)

  bslib::card(
    card_header("Live Tool Monitor (No espera Accept)"),
    verbatimTextOutput(ns_inst("SO_live_debug"))
  )
}

mod_import_r_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Main logic bundle
    dataset_bundle <- reactive({
      shiny::req(input$r_dataset_choice)

      df <- if (input$r_dataset_choice == "diamonds") {
        base::as.data.frame(ggplot2::diamonds)
      } else {
        base::get(input$r_dataset_choice, pos = "package:datasets")
      }

      list(
        my_dataset = df,
        dataset_name_short = input$r_dataset_choice,
        dataset_name_long = input$r_dataset_choice,
        extra_info = list(source_type = "Base R Package")
      )
    })

    # 2. RAW DEBUG OUTPUT (No wait for Accept)
    output$SO_live_debug <- renderPrint({
      # Capturamos los inputs locales
      local_inputs <- reactiveValuesToList(input)
      local_inputs$timestamp = Sys.time()

      cat("--- RAW MODULE MONITOR ---\n")
      cat("Status: INTERNAL_DATA_IS_", if(!is.null(dataset_bundle())) "READY" else "EMPTY", "\n")

      # Mostramos los inputs del sub-módulo (dropdowns, etc)
      utils::str(local_inputs)

      cat("\n--- DATA PREVIEW (INTERNAL) ---\n")
      # Mostramos una pizca del dato que YA está calculado en el sub-módulo
      print(utils::head(dataset_bundle()$my_dataset, 3))
    })

    return(dataset_bundle)
  })
}


####################################################
mod_import_xlsx_ui_step03_action <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fileInput(ns("file_upload"), "Upload Spreadsheet:", accept = c(".xlsx", ".xls")),
    # Envolvemos el select para que nazca bloqueado
    shinyjs::disabled(
      selectizeInput(ns("selected_sheet"), "Select Worksheet:",
                     choices = NULL,
                     options = list(
                       placeholder = "Waiting for file...",
                       onInitialize = I('function() { this.setValue(""); }'),
                       dropdownParent = "body"
                     ))
      # shiny::selectInput(ns("selected_sheet"), "Select Worksheet:", choices = "Waiting for file...")
    )
  )
}

mod_import_xlsx_ui_step04_live <- function(id_instancia) {
  ns_inst <- NS(id_instancia)

  bslib::card(
    card_header("Live Tool Monitor: Excel Loader"),
    verbatimTextOutput(ns_inst("SO_live_debug_xlsx"))
  )
}

mod_import_xlsx_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    available_sheets <- reactiveVal(NULL)

    # Lógica de detección de hojas
    shiny::observeEvent(input$file_upload, {
      shiny::req(input$file_upload)
      sheets <- readxl::excel_sheets(input$file_upload$datapath)
      available_sheets(sheets)
      shiny::updateSelectizeInput(session, "selected_sheet",
                                  choices = sheets,
                                  options = list(
                                    placeholder = 'Select one...',
                                    onInitialize = I('function() { this.setValue(""); }'),
                                    dropdownParent = "body"
                                  ))
      shinyjs::enable("selected_sheet")
    })

    # El objeto reactivo principal del módulo
    dataset_bundle_xlsx <- reactive({
      shiny::req(input$file_upload, input$selected_sheet)
      shiny::req(input$selected_sheet %in% available_sheets())

      df <- readxl::read_excel(path = input$file_upload$datapath, sheet = input$selected_sheet)

      list(
        my_dataset = df,
        dataset_name_short = input$file_upload$name,
        dataset_name_long = paste0(input$file_upload$name, " - Sheet: ", input$selected_sheet),
        extra_info = list(
          source_type = "External Excel File",
          sheet_selected = input$selected_sheet,
          file_size = input$file_upload$size
        )
      )
    })

    # --- 2. RAW DEBUG OUTPUT (No wait for Accept) ---
    output$SO_live_debug_xlsx <- renderPrint({
      local_inputs <- reactiveValuesToList(input)

      cat("--- EXCEL MODULE INTERNAL MONITOR ---\n")
      # Verificamos si el bundle interno está listo (tiene archivo y hoja)
      is_ready <- !is.null(input$file_upload) && (input$selected_sheet %in% available_sheets())
      cat("Status: INTERNAL_DATA_IS_", if(is_ready) "READY" else "WAITING_FOR_SELECTION", "\n")
      cat("Timestamp:", format(Sys.time(), "%H:%M:%S"), "\n\n")

      # Mostramos los inputs: nombre del archivo subido, hoja seleccionada, etc.
      utils::str(local_inputs)

      cat("\n--- DATA PREVIEW (INTERNAL) ---\n")
      if(is_ready) {
        print(utils::head(dataset_bundle_xlsx()$my_dataset, 3))
      } else {
        cat("No data to preview yet. Upload a file and select a sheet.")
      }
    })

    return(dataset_bundle_xlsx)
  })
}




#######################################

LIST_mod_import_options <- list(
  r_datasets = list(
    label = "R Library Datasets",
    ui_step03_action = mod_import_r_ui_step03_action,
    ui_step04_live   = mod_import_r_ui_step04_live,
    server           = mod_import_r_server
  ),
  excel_file = list(
    label = "Excel File (.xlsx)",
    ui_step03_action = mod_import_xlsx_ui_step03_action,
    ui_step04_live   = mod_import_xlsx_ui_step04_live,
    server           = mod_import_xlsx_server
  )
)
