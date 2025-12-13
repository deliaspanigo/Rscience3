# pre_render_script.R
# This script executes BEFORE Quarto rendering to set up the environment.
cat("🚀 Executing PRE-RENDER script...\n\n")

# ------------------------------------------------------------------------------
# 1. SETUP - Ensure here() works correctly
# ------------------------------------------------------------------------------
cat("📁 STEP 1: Checking project root marker...\n")
here_file <- ".here"

if (!file.exists(here_file)) {
  cat("   Creating .here file at project root...\n")
  here::set_here()
  cat("   ✅ .here file created\n")
} else {
  cat("   ✅ .here file already exists\n")
}

# cat("   Project root detected:", here::here(), "\n\n")
cat("   Project root detected:", "\n\n")

# ------------------------------------------------------------------------------
# 2. CLEANUP - Remove previous zzz_output_ files
# ------------------------------------------------------------------------------
cat("🧹 STEP 2: Cleaning up previous zzz_output_ files...\n")

# Find ALL items (files and directories) starting with 'zzz_output_'
zzz_items <- list.files(
  path = here::here(),
  pattern = "^zzz_output_",
  full.names = TRUE,
  recursive = FALSE,
  include.dirs = TRUE,  # ← IMPORTANTE: Incluir carpetas también
  all.files = FALSE,    # No incluir archivos ocultos
  no.. = TRUE           # Excluir . y ..
)

# Filter to ensure items actually exist
zzz_items <- zzz_items[file.exists(zzz_items) | dir.exists(zzz_items)]

if (length(zzz_items) > 0) {
  cat("   Found", length(zzz_items), "item(s) to delete:\n")
  
  # Categorize and show what we're deleting
  file_count <- 0
  folder_count <- 0
  
  for (item in zzz_items) {
    if (dir.exists(item)) {
      # It's a folder
      folder_count <- folder_count + 1
      
      # Count files inside (optional)
      files_inside <- list.files(item, recursive = TRUE, all.files = TRUE, no.. = TRUE)
      file_count_inside <- length(files_inside)
      
      cat("   📁 Folder:", basename(item))
      if (file_count_inside > 0) {
        cat(" (contains", file_count_inside, "files)")
      }
      cat("\n")
      
    } else if (file.exists(item)) {
      # It's a file
      file_count <- file_count + 1
      
      file_size <- file.size(item)
      file_size_fmt <- if (!is.na(file_size)) {
        format(file_size, big.mark = ",", scientific = FALSE)
      } else {
        "unknown size"
      }
      cat("   📄 File:", basename(item), "(", file_size_fmt, "bytes )\n")
    }
  }
  
  cat("\n   Summary:", file_count, "file(s),", folder_count, "folder(s)\n")
  
  # Delete everything - use unlink() for both files and folders
  success <- sapply(zzz_items, function(item) {
    if (dir.exists(item)) {
      # Delete folder recursively
      result <- unlink(item, recursive = TRUE, force = TRUE)
      return(result == 0)  # unlink returns 0 on success
    } else {
      # Delete file
      return(file.remove(item))
    }
  })
  
  deleted_count <- sum(success, na.rm = TRUE)
  
  if (deleted_count > 0) {
    cat("   ✅ Deleted", deleted_count, "item(s)\n")
  }
  
  # Report failures
  if (any(!success)) {
    failed_items <- zzz_items[!success]
    cat("   ⚠️  Failed to delete", length(failed_items), "item(s):\n")
    for (item in failed_items) {
      item_type <- ifelse(dir.exists(item), "Folder", "File")
      cat("     -", item_type, ":", basename(item), "\n")
    }
  }
} else {
  cat("   ✅ No zzz_output_ items found\n")
}
cat("\n")

# ------------------------------------------------------------------------------
# 3. OUTPUT FOLDER - Minimal setup (CREATE IF MISSING)
# ------------------------------------------------------------------------------
cat("📂 STEP 3: Ensuring output folder exists...\n")

tryCatch({
  config <- yaml::read_yaml("_quarto.yml")
  output_folder <- config$my_global_folders$USER_OUTPUT_FOLDER
  
  if (is.null(output_folder) || output_folder == "") {
    stop("Output folder not specified")
  }
  
  cat("   Target folder:", output_folder, "\n")
  
  if (!dir.exists(output_folder)) {
    cat("   Creating folder...\n")
    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
    
    if (dir.exists(output_folder)) {
      cat("   ✅ Folder created\n")
    } else {
      cat("   ⚠️  Folder may not have been created\n")
    }
  } else {
    cat("   ✅ Folder already exists\n")
  }
  
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
  output_folder <- NULL
})
cat("\n")

# ------------------------------------------------------------------------------
# 4. FINAL VERIFICATION
# ------------------------------------------------------------------------------
cat("🔍 STEP 4: Final verification...\n")

# Verify output folder is accessible
if (dir.exists(output_folder)) {
  cat("   ✅ Output folder is ready:", output_folder, "\n")
  
  # Check permissions
  test_file <- file.path(output_folder, ".test_permission")
  if (file.create(test_file)) {
    file.remove(test_file)
    cat("   ✅ Write permissions confirmed\n")
  } else {
    cat("   ⚠️  Warning: Could not verify write permissions\n")
  }
} else {
  cat("   ⚠️  Output folder not properly configured\n")
}



cat("✅ PRE-RENDER SCRIPT COMPLETED SUCCESSFULLY\n")
