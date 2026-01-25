module_import_01_RDataset_ui <- function(id) {

  vector_choices <- c("iris", "mtcars")
  vector_choices <- c("Select one..." = "", vector_choices)
  ns <- NS(id)
  tagList(
    # card(
      # card_header("Configuración de Importación"),
    selectizeInput(
      inputId = ns("dataset_sel"),
      label = "Select an R dataset:",
      choices = vector_choices,
      selected = vector_choices[1],
      width = "fit-content",
      # Ahora 'options' sí funcionará correctamente
      options = list(dropdownParent = "body")
    ),
       # hr(),
      tableOutput(ns("preview"))
    )
  # )
}

module_import_01_RDataset_server <- function(id, show_my_table = T) {
  moduleServer(id, function(input, output, session) {

    # Objeto de salida reactivo (OR)
    OR_import_dataset_01_RData <- reactive({
      req(input$dataset_sel)

      # Aquí es donde ocurre la magia de la importación
      output_list <- list(
        "my_dataset" = get(input$dataset_sel, "package:datasets"),
        "name"       = input$dataset_sel,
        "timestamp"  = Sys.time()
      )
      output_list$is_data_frame <- is.data.frame(output_list$"my_dataset")
      output_list
    })

    # Vista previa local al módulo
    output$preview <- renderTable({
      req(show_my_table)
      head(OR_import_dataset_01_RData()$my_dataset, 5)
    })

    # DEVOLVEMOS el reactivo para que el Orchestrator lo reciba
    return(OR_import_dataset_01_RData)
  })
}
