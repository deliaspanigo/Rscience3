
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

      output_list <- fn3_IMPORT_new_import_struct()

      # Default "Not Done"
      if (is.null(input$file_upload) || is.null(input$sheet_sel) || input$sheet_sel == "") {
        return(output_list)
      }

      # Basics
      df <-


      # Basics
      str_file_path_general <- 'readxl::read_excel(path = "_file_path_", sheet = "_selected_sheet_")'
      init_time <- Sys.time()
      my_dataset <- readxl::read_excel(path = input$file_upload$datapath, sheet = input$sheet_sel)
      is_df <- is.data.frame(my_dataset)
      file_name <- input$file_upload$name

      str_sheet_name <- input$sheet_sel
      str_sheet_pos  <- input$sheet_sel
      label_file_name <- paste0(file_name, " - Sheet: ", str_sheet_name)

      str_ncol <- ncol(my_dataset)
      str_nrow <- nrow(my_dataset)
      str_file_path_external <- str_file_path_general
      str_file_path_external <- sub(pattern = "_file_path_", replacement = file_name, x = str_file_path_external)
      str_file_path_external <- sub(pattern = "_selected_sheet_", replacement = input$file_upload$datapath, x = str_file_path_external)

      str_file_path_internal <- str_file_path_general
      str_file_path_internal <- sub(pattern = "_file_path_", replacement = file_name, x = str_file_path_internal)
      str_file_path_internal <- sub(pattern = "_selected_sheet_", replacement = str_sheet_name, x = str_file_path_internal)


      is_done <- is.data.frame(my_dataset)
      end_time <- Sys.time()
      time_seconds <- as.numeric(difftime(end_time, init_time, units = "secs"))

      # Extra
      str_description_extra <- "File Upload from xlsx importer."
      extra_list <- input$file_upload

      # 01. Importer

      # 02. Dataset
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "is_done", value = is_done)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "is_data_frame", value = is_df)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "my_dataset", value = my_dataset)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "rows", value = str_nrow)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "cols", value = str_ncol)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "file_name", value = file_name)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "label_file_name", value = label_file_name)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "sheet_name", value = str_sheet_name)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "sheet_pos", value = str_sheet_pos)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "file_path_external", value = str_file_path_external)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "file_path_internal", value = str_file_path_internal)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "init_time", value = init_time)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "end_time", value = end_time)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "dataset", field = "time_secs", value = time_seconds)

      # 03. Extra
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "extra", field = "description", value = str_description_extra)
      output_list <- fn3_IMPORT_set_import_data(current_list = output_list, category = "extra", field = "extra_list", value = extra_list)

    })
    return(OR_import_dataset_02_xlsx)
  })
}
