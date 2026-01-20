# ------------------------------------------------------------------------------
# 1. LOAD CONFIGURATION
# ------------------------------------------------------------------------------
cat("📄 STEP 1: Loading configuration...\n")
tryCatch({
  # Load YAML configuration
  list_config <- yaml::read_yaml("_quarto.yml")
  cat("   ✅ Configuration loaded successfully\n")
}, error = function(e) {
  stop("❌ Failed to load _quarto.yml: ", e$message)
})
cat("\n")


# cat("📁 STEP 2: List original files and folders...\n")
# vector_original_files   <- list_config$local_inputs$list_local_input_files
# vector_original_files   <- unlist(vector_original_files)
# str_regex_save_files <- list_config$local_inputs$str_regex_save_files
# vector_save_original_files <- list.files(pattern = str_regex_save_files, recursive = FALSE, all.files = T, no.. = TRUE)
# vector_original_files <- c(vector_original_files, vector_save_original_files)



# Delete specific folders ------------------------------------------------------
vector_delete_folders <- c(".quarto", ".RData", ".Rhistory",".gitignore")
unlink(vector_delete_folders, recursive = TRUE, force = TRUE)




# Delete specifics files -------------------------------------------------------
vector_zzz_output <- list.files(
  path = ".", 
  pattern = "^zzz_output_", 
  all.files = TRUE, 
  full.names = FALSE, 
  no.. = TRUE
)
unlink(vector_zzz_output, recursive = TRUE, force = TRUE)



# Delete report00 .qmd
str_external_qmd_file_path <- unlist(list_config$external_inputs$list_external_input_file_path)
str_external_qmd_file_name <- basename(str_external_qmd_file_path)
unlink(str_external_qmd_file_name, recursive = FALSE, force = TRUE)


############################################
# Delete copy if exists
str_new_copy <- list_config$local_outputs$str_copy_qmd_file_name
unlink(x = str_new_copy)