library(readxl)

module_import_02_xlsx_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; gap: 20px; align-items: flex-end; justify-content: flex-start;",
      div(style = "width: auto; min-width: 300px;",
          fileInput(ns("file_upload"), "Choose Excel File:", accept = c(".xlsx", ".xls"))
      ),
      div(style = "width: auto;",
          uiOutput(ns("sheet_selector_ui"))
      )
    ),
    # hr(),
    # AGREGAR ESTO para que el módulo pueda mostrar la tabla
    tableOutput(ns("preview"))
  )
}

module_import_02_xlsx_server <- function(id, show_my_table = T) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Get available sheets from the uploaded file
    sheets_names <- reactive({
      req(input$file_upload)
      readxl::excel_sheets(input$file_upload$datapath)
    })

    # 2. Dynamic UI to select the sheet
    output$sheet_selector_ui <- renderUI({
      req(sheets_names())

      vector_choices <- sheets_names()

      selectizeInput(
        inputId = ns("sheet_sel"),
        label = "Select Sheet:",
        choices = vector_choices,
        selected = vector_choices[1],
        width = "fit-content",
        # Ahora 'options' sí funcionará correctamente
        options = list(dropdownParent = "body")
      )

    })

    # 3. Reactive Output Object (OR)
    OR_import_dataset_02_xlsx <- reactive({
      req(input$file_upload, input$sheet_sel)

      # Read the data
      df <- readxl::read_excel(input$file_upload$datapath, sheet = input$sheet_sel)

      # Maintain the same structure as the RData module
      output_list <- list(
        "my_dataset"   = df,
        "name"         = input$file_upload$name,
        "sheet"        = input$sheet_sel,
        "timestamp"    = Sys.time()
      )
      output_list$is_data_frame <- is.data.frame(output_list$my_dataset)

      output_list
    })

    # 4. Local preview
    output$preview <- renderTable({
      req(OR_import_dataset_02_xlsx())
      req(show_my_table)
      head(OR_import_dataset_02_xlsx()$my_dataset, 5)
    })

    # RETURN the reactive for the Orchestrator
    return(OR_import_dataset_02_xlsx)
  })
}
