# script_R_02_post_render.R
# This script executes AFTER Quarto rendering to organize output files.
cat("🚀 Executing POST-RENDER script...\n\n")
source(file = "fn_local.R")

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

# ------------------------------------------------------------------------------
# 2. SETUP OUTPUT FOLDER
# ------------------------------------------------------------------------------
cat("📂 STEP 2: Setting up output folder...\n")

# Get output folder from configuration
str_zzz_output <- list_config$local_outputs$str_regex_inclusion_files
str_final_output_folder <- list_config$external_output$str_user_output_folder_path

# 1. Setup paths from your config list
target_pattern <- list_config$local_outputs$str_regex_inclusion_files
destination_dir <- list_config$external_output$str_user_output_folder_path

fn_copying_files_from_pattern(target_pattern, destination_dir)

