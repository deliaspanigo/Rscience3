
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
