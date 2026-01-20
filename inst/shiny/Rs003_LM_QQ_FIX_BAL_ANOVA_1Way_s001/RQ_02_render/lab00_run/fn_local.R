fn_roll_back_to_init_files_and_folders <- function(target_folder) {

  if (!requireNamespace("yaml", quietly = TRUE)) stop("Package 'yaml' is required.")
  library(yaml)

  # --- 1. PATH SETUP ---
  expected_folder_name <- basename(target_folder)
  full_target_path <- normalizePath(target_folder, winslash = "/", mustWork = FALSE)
  current_dir <- normalizePath(getwd(), winslash = "/")

  # --- 2. SECURITY CHECKPOINTS ---

  if (!dir.exists(full_target_path)) {
    stop("ERROR: Target folder not found at: ", full_target_path)
  }

  if (basename(full_target_path) != expected_folder_name) {
    stop("SECURITY ERROR: Folder found is '", basename(full_target_path),
         "', but expected '", expected_folder_name, "'.")
  }

  if (full_target_path == current_dir) {
    stop("ERROR: Cannot perform cleanup on the current working directory.")
  }

  quarto_config_path <- file.path(full_target_path, "_quarto.yml")
  if (!file.exists(quarto_config_path)) {
    stop("ERROR: _quarto.yml not found. This is not recognized as a valid project folder.")
  }

  # --- 3. YAML PROCESSING (EXTRACT PROTECTED FILES) ---

  config <- yaml::read_yaml(quarto_config_path)

  extract_init_files <- function(node) {
    files <- c()
    if (is.list(node)) {
      if ("file_name" %in% names(node) && isTRUE(node$must_exists_init)) {
        return(node$file_name)
      }
      for (item in node) {
        files <- c(files, extract_init_files(item))
      }
    }
    return(files)
  }

  protected_files <- extract_init_files(config$local_resources)

  if (length(protected_files) == 0) {
    stop("CRITICAL ERROR: No files marked with 'must_exists_init: true' found in YAML.\n",
         "Operation aborted to prevent accidental full directory wipe.")
  }

  # --- 4. IDENTIFY ITEMS TO REMOVE ---

  all_items <- list.files(full_target_path, all.files = TRUE, no.. = TRUE)
  whitelist <- c(protected_files, "_quarto.yml")
  to_remove_names <- setdiff(all_items, whitelist)
  final_removal_paths <- file.path(full_target_path, to_remove_names)

  # --- 5. EXECUTION WITH CONFIRMATION ---

  if(length(final_removal_paths) > 0) {
    # Count directories and files separately
    is_dir <- dir.exists(final_removal_paths)
    count_dirs  <- sum(is_dir)
    count_files <- sum(!is_dir)

    cat("\n====================================================")
    cat("\n   CLEANUP REPORT: ", expected_folder_name)
    cat("\n====================================================")
    cat("\nPATH:", full_target_path)
    cat("\n\nPROTECTED ITEMS (Will stay):\n")
    cat(paste("  [KEEP] ", whitelist, collapse = "\n"), "\n")

    cat("\nEXTRA ITEMS DETECTED (Will be DELETED):\n")
    cat(paste("  [DROP] ", to_remove_names, collapse = "\n"), "\n")
    cat("----------------------------------------------------\n")

    # Summary of items to be deleted
    cat(sprintf("SUMMARY TO DELETE: %d files and %d directories.\n", count_files, count_dirs))
    cat("NOTE: Directories will be removed recursively.\n")
    cat("----------------------------------------------------\n")

    confirm <- readline(prompt="Are you sure you want to delete these items? (Y/N): ")

    if (toupper(confirm) == "Y") {
      unlink(final_removal_paths, recursive = TRUE, force = TRUE)
      cat("\n✅ Folder successfully restored to its initial state.\n")
    } else {
      cat("\n❌ Operation cancelled by user. No changes were made.\n")
    }
  } else {
    cat("\n✨ Folder is already clean. No extra items found.\n")
  }
}
