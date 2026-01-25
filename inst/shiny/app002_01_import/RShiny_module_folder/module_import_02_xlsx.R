module_import_02_xlsx_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Estilo CSS inyectado para que el botón de 'Browse' no rompa la estética
    tags$style(HTML(paste0("
      #", ns("file_upload"), "_progress { margin-bottom: 0px; }
      .shiny-input-container:not(.shiny-input-has-error) .progress { background-color: #f5f5f5; }
    "))),

    div(
      # Alineación al inicio (arriba) y gap consistente
      style = "display: flex; gap: 20px; align-items: flex-start; justify-content: flex-start; overflow: visible;",

      # 1. Bloque de Archivo
      div(
        style = "width: 350px;", # Ancho fijo para evitar saltos visuales
        fileInput(
          inputId = ns("file_upload"),
          label = "Choose Excel File:",
          accept = c(".xlsx", ".xls"),
          width = "100%",
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        )
      ),

      # 2. Bloque de Hojas (Dinámico)
      div(
        style = "width: auto; min-width: 250px; overflow: visible;",
        uiOutput(ns("sheet_selector_ui"))
      )
    ),

    # 3. Vista previa local con spinner pequeño
    div(
      style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
      withSpinner(tableOutput(ns("preview")), type = 8, size = 0.5)
    )
  )
}

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
        # width = "fit-content",
        options = list(dropdownParent = "body")
      )
    })

    # 3. Objeto de Salida Reactivo (OR) con validación defensiva
    OR_import_dataset_02_xlsx <- reactive({
      # Default "Not Ready" state
      default_out <- list(is_done = FALSE, my_dataset = NULL, name = NULL)

      # Check requirements
      if (is.null(input$file_upload) || is.null(input$sheet_sel) || input$sheet_sel == "") {
        return(default_out)
      }

      # Defensive check for sheet existence
      if (!(input$sheet_sel %in% sheets_names())) return(default_out)

      # If everything is fine, read and return "is_done = TRUE"
      df <- readxl::read_excel(input$file_upload$datapath, sheet = input$sheet_sel)

      list(
        is_done = TRUE,
        my_dataset = df,
        name = input$file_upload$name,
        sheet = input$sheet_sel
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
