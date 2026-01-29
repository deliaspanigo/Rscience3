
str_path_fn_local <- system.file("shiny/app002_01_import/fn_local.R", package = "Rscience3")
source(file = str_path_fn_local)


module_import_01_RDataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns("dataset_sel"),
      label = "Select an R dataset:",
      choices = c("Select one..." = "",
                  "iris" = "iris",
                  "mtcars" = "mtcars",
                  "quakes" = "quakes"),

      width = "fit-content",
      options = list(dropdownParent = "body")
    )
  )
}

module_import_01_RDataset_server <- function(id, show_my_table = TRUE) {
  moduleServer(id, function(input, output, session) {

    OR_import_dataset_01_RData <- reactive({

      output_list <- fn3_IMPORT_new_import_struct()

      # Default "Not Done"
      if (is.null(input$dataset_sel) || input$dataset_sel == "") {
        return(output_list)
      }

      # Basics
      str_file_path_general <- 'get("_name_", "package:datasets")'
      init_time <- Sys.time()
      my_dataset <- get(input$dataset_sel, "package:datasets")
      is_df <- is.data.frame(my_dataset)
      file_name <- input$dataset_sel
      label_file_name <- file_name
      str_ncol <- ncol(my_dataset)
      str_nrow <- nrow(my_dataset)
      str_file_path_external <- sub(pattern = "_name_", replacement = file_name, x = str_file_path_general)
      str_file_path_internal <- sub(pattern = "_name_", replacement = file_name, x = str_file_path_general)

      is_done <- is.data.frame(my_dataset)
      end_time <- Sys.time()
      diff_secs <- as.numeric(difftime(end_time, init_time, units = "secs"))
      # 01. Importer

      # 02. Dataset
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "is_done", value = is_done)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "is_data_frame", value = is_df)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "my_dataset", value = my_dataset)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "rows", value = str_nrow)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "cols", value = str_ncol)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "file_name", value = file_name)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "label_file_name", value = label_file_name)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "sheet_name", value = NA_character_)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "sheet_pos", value = NA_character_)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "file_path_external", value = str_file_path_external)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "file_path_internal", value = str_file_path_internal)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "init_time", value = init_time)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "end_time", value = end_time)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "diff_secs", value = diff_secs)


      return(output_list)
    })


    return(OR_import_dataset_01_RData)
  })
}
