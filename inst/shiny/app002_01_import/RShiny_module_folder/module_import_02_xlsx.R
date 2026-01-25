module_import_02_xlsx_server <- function(id, show_my_table = T) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Nombres de hojas (Reactivo)
    sheets_names <- reactive({
      req(input$file_upload)
      readxl::excel_sheets(input$file_upload$datapath)
    })

    # 2. Renderizado de la UI del selector
    output$sheet_selector_ui <- renderUI({
      req(sheets_names())
      vector_choices <- c("Choose sheet..." = "", sheets_names())

      selectizeInput(
        inputId = ns("sheet_sel"),
        label = "Select Sheet:",
        choices = vector_choices,
        selected = NULL,
        width = "fit-content",
        options = list(dropdownParent = "body")
      )
    })

    # 3. Objeto de Salida Reactivo (OR) con validación defensiva
    OR_import_dataset_02_xlsx <- reactive({
      # Requerimientos básicos
      req(input$file_upload, input$sheet_sel)
      req(input$sheet_sel != "")

      # --- EL ESCUDO ---
      # Verificamos si la hoja seleccionada existe en el archivo actual
      # Si no existe (porque es de un archivo anterior), detenemos la ejecución
      current_sheets <- sheets_names()
      if (!(input$sheet_sel %in% current_sheets)) {
        return(NULL)
      }

      # Si pasó la validación, leemos
      df <- readxl::read_excel(input$file_upload$datapath, sheet = input$sheet_sel)

      list(
        "my_dataset"   = df,
        "name"         = input$file_upload$name,
        "sheet"        = input$sheet_sel,
        "timestamp"    = Sys.time(),
        "is_data_frame" = is.data.frame(df)
      )
    })

    # 4. Vista previa local
    output$preview <- renderTable({
      # Usamos req() para que no intente renderizar si el escudo devolvió NULL
      res <- OR_import_dataset_02_xlsx()
      req(res, res$my_dataset)
      req(show_my_table)
      head(res$my_dataset, 5)
    })

    return(OR_import_dataset_02_xlsx)
  })
}
