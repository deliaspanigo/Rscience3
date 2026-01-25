
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
