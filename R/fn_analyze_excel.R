#' @export
analyze_excel <- function(file_path) {
  # Validate that the file exists
  if (!file.exists(file_path)) {
    stop("The file does not exist at the specified path")
  }
  
  # Validate that it's an Excel .xlsx file
  if (!grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
    stop("The file does not appear to be an Excel .xlsx file")
  }
  
  # Get the file size in MB
  file_size <- file.size(file_path) / (1024 * 1024)
  
  # Get sheet names
  sheet_names <- readxl::excel_sheets(file_path)
  sheet_count <- length(sheet_names)
  
  # Get information for each sheet
  sheets_info <- lapply(sheet_names, function(sheet) {
    # Read only structure to count columns
    structure <- suppressMessages(
      readxl::read_excel(file_path, sheet = sheet, n_max = 0)
    )
    columns <- ncol(structure)
    
    # Count rows efficiently
    rows <- 0
    chunk_size <- 10000
    
    repeat {
      data_chunk <- suppressMessages(
        readxl::read_excel(file_path, sheet = sheet, 
                           skip = rows, n_max = chunk_size,
                           col_names = FALSE, .name_repair = "minimal")
      )
      
      if (nrow(data_chunk) == 0) break
      
      rows <- rows + nrow(data_chunk)
      
      if (nrow(data_chunk) < chunk_size) break
    }
    
    list(
      name = sheet,
      rows = rows,
      columns = columns
    )
  })
  df_sheets_info <- do.call(rbind.data.frame, sheets_info)
  vector_cols <- df_sheets_info[,"columns"]
  vector_rows <- df_sheets_info[,"rows"]
  
  # Build result
  result <- list(
    file_path = file_path,
    file_size_mb = round(file_size, 2),
    sheet_count = sheet_count,
    sheets_info = df_sheets_info,
    vector_cols = vector_cols,
    vector_rows = vector_rows
  )
  
  
  return(result)
}