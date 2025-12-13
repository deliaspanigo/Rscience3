# script_R_02_post_render.R
# This script executes AFTER Quarto rendering to organize output files.
cat("🚀 Executing POST-RENDER script...\n\n")

# ------------------------------------------------------------------------------
# 1. LOAD CONFIGURATION
# ------------------------------------------------------------------------------
cat("📄 STEP 1: Loading configuration...\n")
tryCatch({
  # Load YAML configuration
  config <- yaml::read_yaml("_quarto.yml")
  cat("   ✅ Configuration loaded successfully\n")
}, error = function(e) {
  stop("❌ Failed to load _quarto.yml: ", e$message)
})
cat("\n")

# ------------------------------------------------------------------------------
# 2. SETUP OUTPUT FOLDER
# ------------------------------------------------------------------------------
cat("📂 STEP 2: Setting up output folder...\n")

# Get output folder from configuration
output_folder <- config$my_global_folders$USER_OUTPUT_FOLDER

if (is.null(output_folder) || output_folder == "") {
  stop("❌ Output folder not specified in configuration")
}

cat("   Output folder:", output_folder, "\n")

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  cat("   Creating output folder...\n")
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  if (dir.exists(output_folder)) {
    cat("   ✅ Output folder created\n")
  } else {
    stop("❌ Failed to create output folder: ", output_folder)
  }
} else {
  cat("   ✅ Output folder already exists\n")
}
cat("\n")

# ------------------------------------------------------------------------------
# 3. FIND ALL zzz_output_ ITEMS
# ------------------------------------------------------------------------------
cat("🔍 STEP 3: Finding all zzz_output_ items...\n")

# Find ALL items (files and folders) starting with 'zzz_output_'
zzz_items <- list.files(
  path = here::here(),
  pattern = "^zzz_output_",
  full.names = TRUE,
  recursive = TRUE,           # Search in subdirectories too
  include.dirs = TRUE,        # Include directories
  all.files = FALSE,          # Don't include hidden files
  no.. = TRUE                 # Exclude . and ..
)

# Filter to ensure items actually exist
zzz_items <- zzz_items[file.exists(zzz_items) | dir.exists(zzz_items)]

if (length(zzz_items) == 0) {
  cat("   ✅ No zzz_output_ items found\n")
  cat("⚠️  NO FILES TO TRANSFER - SCRIPT COMPLETED\n")
}

# Categorize items
zzz_folders <- zzz_items[dir.exists(zzz_items)]
zzz_files <- zzz_items[file.exists(zzz_items) & !dir.exists(zzz_items)]

cat("   Found", length(zzz_items), "item(s):\n")
cat("   • Folders:", length(zzz_folders), "\n")
cat("   • Files:", length(zzz_files), "\n")

# Show details
if (length(zzz_folders) > 0) {
  cat("\n   Folders to copy:\n")
  for (folder in zzz_folders) {
    # Count items inside folder
    items_inside <- list.files(folder, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    cat("   📁", basename(folder), "(", length(items_inside), "items inside )\n")
  }
}

if (length(zzz_files) > 0) {
  cat("\n   Files to copy:\n")
  for (file in zzz_files) {
    file_size <- file.size(file)
    file_size_mb <- if (!is.na(file_size)) {
      sprintf("%.2f MB", file_size / (1024^2))  # Convert to MB with 2 decimals
    } else {
      "unknown size"
    }
    cat("   📄", basename(file), "(", file_size_mb, ")\n")
  }
}
cat("\n")

# ------------------------------------------------------------------------------
# 4. COPY ALL zzz_output_ ITEMS
# ------------------------------------------------------------------------------
cat("📤 STEP 4: Copying all zzz_output_ items to output folder...\n")

transfer_results <- list(
  folders_copied = character(),
  files_copied = character(),
  failed = character()
)

# Function to copy a folder recursively
copy_folder_recursive <- function(src, dest) {
  # Create destination folder
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  
  # Get all items in source folder
  items <- list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  
  for (item in items) {
    item_dest <- file.path(dest, basename(item))
    
    if (dir.exists(item)) {
      # Recursively copy subfolder
      copy_folder_recursive(item, item_dest)
    } else {
      # Copy file
      file.copy(item, item_dest, overwrite = TRUE)
    }
  }
}

# Copy folders first
if (length(zzz_folders) > 0) {
  for (src_folder in zzz_folders) {
    dest_folder <- file.path(output_folder, basename(src_folder))
    
    tryCatch({
      cat("   📁 Copying folder:", basename(src_folder), "... ")
      copy_folder_recursive(src_folder, dest_folder)
      transfer_results$folders_copied <- c(transfer_results$folders_copied, src_folder)
      cat("✅\n")
    }, error = function(e) {
      cat("❌ -", e$message, "\n")
      transfer_results$failed <- c(transfer_results$failed, src_folder)
    })
  }
}

# Copy files
if (length(zzz_files) > 0) {
  for (src_file in zzz_files) {
    dest_file <- file.path(output_folder, basename(src_file))
    
    tryCatch({
      cat("   📄 Copying file:", basename(src_file), "... ")
      file.copy(src_file, dest_file, overwrite = TRUE)
      transfer_results$files_copied <- c(transfer_results$files_copied, src_file)
      cat("✅\n")
    }, error = function(e) {
      cat("❌ -", e$message, "\n")
      transfer_results$failed <- c(transfer_results$failed, src_file)
    })
  }
}
cat("\n")

# ------------------------------------------------------------------------------
# 5. VERIFICATION
# ------------------------------------------------------------------------------
cat("🔍 STEP 5: Verifying copies...\n")

verified_count <- 0
failed_verification <- character()

# Verify folders
for (src_folder in transfer_results$folders_copied) {
  dest_folder <- file.path(output_folder, basename(src_folder))
  
  if (dir.exists(dest_folder)) {
    # Compare item counts
    src_count <- length(list.files(src_folder, recursive = TRUE, all.files = TRUE, no.. = TRUE))
    dest_count <- length(list.files(dest_folder, recursive = TRUE, all.files = TRUE, no.. = TRUE))
    
    if (dest_count >= src_count) {  # Allow more items in dest (like temp files)
      cat("   ✅ Folder verified:", basename(src_folder), 
          "(", src_count, "→", dest_count, "items )\n")
      verified_count <- verified_count + 1
    } else {
      cat("   ⚠️  Folder incomplete:", basename(src_folder), 
          "(", src_count, "→", dest_count, "items )\n")
      failed_verification <- c(failed_verification, src_folder)
    }
  } else {
    cat("   ❌ Folder missing in destination:", basename(src_folder), "\n")
    failed_verification <- c(failed_verification, src_folder)
  }
}

# Verify files
for (src_file in transfer_results$files_copied) {
  dest_file <- file.path(output_folder, basename(src_file))
  
  if (file.exists(dest_file)) {
    src_size <- file.size(src_file)
    dest_size <- file.size(dest_file)
    
    if (!is.na(src_size) && !is.na(dest_size) && dest_size >= src_size) {
      src_size_mb <- sprintf("%.2f MB", src_size / (1024^2))
      cat("   ✅ File verified:", basename(src_file), 
          "(", src_size_mb, ")\n")
      verified_count <- verified_count + 1
    } else {
      cat("   ⚠️  File size mismatch:", basename(src_file), "\n")
      failed_verification <- c(failed_verification, src_file)
    }
  } else {
    cat("   ❌ File missing in destination:", basename(src_file), "\n")
    failed_verification <- c(failed_verification, src_file)
  }
}
cat("\n")

# ------------------------------------------------------------------------------
# 6. SUMMARY REPORT
# ------------------------------------------------------------------------------
cat("📊 STEP 6: Transfer summary...\n")
cat("   • Folders copied:", length(transfer_results$folders_copied), "\n")
cat("   • Files copied:", length(transfer_results$files_copied), "\n")
cat("   • Items failed:", length(transfer_results$failed), "\n")
cat("   • Items verified:", verified_count, "/", 
    length(transfer_results$folders_copied) + length(transfer_results$files_copied), "\n")
cat("   • Total items found:", length(zzz_items), "\n")

# Check final status
total_successful <- length(transfer_results$folders_copied) + length(transfer_results$files_copied)
total_attempted <- length(zzz_items)

if (length(transfer_results$failed) == 0 && 
    verified_count == total_successful) {
  cat("   ✅ All transfers completed and verified successfully\n")
} else if (total_successful > 0) {
  cat("   ⚠️  Partial success (", total_successful, "/", total_attempted, " items)\n", sep = "")
} else {
  cat("   ❌ All transfers failed\n")
}
cat("\n")

# ------------------------------------------------------------------------------
# 7. OPTIONAL: CLEANUP OF EMPTY DIRECTORIES
# ------------------------------------------------------------------------------
cat("🧹 STEP 7: Optional cleanup of empty directories...\n")

empty_dirs_found <- 0
all_dirs <- list.dirs(recursive = TRUE)
all_dirs <- all_dirs[!grepl(output_folder, all_dirs)]  # Exclude output folder

for (dir in all_dirs) {
  if (dir.exists(dir)) {
    items <- list.files(dir, all.files = TRUE, no.. = TRUE)
    if (length(items) == 0) {
      # Check if directory name starts with zzz_output_ (optional)
      if (grepl("^zzz_output_", basename(dir))) {
        cat("   Removing empty zzz_output_ directory:", basename(dir), "\n")
      } else {
        cat("   Removing empty directory:", basename(dir), "\n")
      }
      unlink(dir, recursive = TRUE)
      empty_dirs_found <- empty_dirs_found + 1
    }
  }
}

if (empty_dirs_found == 0) {
  cat("   ✅ No empty directories found\n")
} else {
  cat("   ✅ Removed", empty_dirs_found, "empty directory(ies)\n")
}
cat("\n")

# ------------------------------------------------------------------------------
# FINAL MESSAGE
# ------------------------------------------------------------------------------

cat("✅ POST-RENDER SCRIPT COMPLETED\n")
