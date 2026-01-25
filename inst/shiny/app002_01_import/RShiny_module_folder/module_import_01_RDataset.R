module_import_01_RDataset_ui <- function(id) {
  ns <- NS(id)

  # We define choices here, ensuring the empty string is the first option
  vector_choices <- c("Select one..." = "", "iris", "mtcars", "quakes", "airquality")

  tagList(
    selectizeInput(
      inputId = ns("dataset_sel"),
      label = "Select an R dataset:",
      choices = vector_choices,
      selected = NULL, # Start empty to force a user choice
      width = "fit-content",
      options = list(dropdownParent = "body")
    ),
    # Optional local preview
    tableOutput(ns("preview"))
  )
}

module_import_01_RDataset_server <- function(id, show_my_table = TRUE) {
  moduleServer(id, function(input, output, session) {

    # Standardized Reactive Output (OR)
    OR_import_dataset_01_RData <- reactive({

      # 1. Default "Not Done" state
      default_out <- list(
        is_done = FALSE,
        my_dataset = NULL,
        name = NULL,
        timestamp = Sys.time()
      )

      # 2. Validation: If nothing is selected, return the default
      if (is.null(input$dataset_sel) || input$dataset_sel == "") {
        return(default_out)
      }

      # 3. Successful Import Logic
      # We use get() to fetch the dataset from the base package
      raw_data <- get(input$dataset_sel, "package:datasets")

      list(
        is_done = TRUE, # THE CRITICAL FLAG
        my_dataset = raw_data,
        name = input$dataset_sel,
        timestamp = Sys.time(),
        is_data_frame = is.data.frame(raw_data)
      )
    })

    # Local preview logic
    output$preview <- renderTable({
      res <- OR_import_dataset_01_RData()
      req(show_my_table, res$is_done)
      head(res$my_dataset, 5)
    })

    # Return the standardized reactive
    return(OR_import_dataset_01_RData)
  })
}
