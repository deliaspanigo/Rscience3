# --- global.R ---

# 1. Libraries
# In a package, it's better to list these in the DESCRIPTION file,
# but keeping them here for the Shiny session is fine.

library("bslib")
library("lubridate")
library("shiny")
library("shinycssloaders")
library("Rscience3") # Load your own package to access internal functions

# 2. Locate module folders using system.file
# This works regardless of where the package is installed on Windows
path_modules_01_import <- system.file("shiny/app002_01_import/RShiny_module_folder", package = "Rscience3")
path_modules_02_tools  <- system.file("shiny/app002_02_tools/RShiny_module_folder",  package = "Rscience3")

# 3. Security Check: Ensure paths exist
if (path_modules_01_import == "" || path_modules_02_tools == "") {
  stop("Critical Error: Module directories not found within Rscience3 package structure.")
}

# 4. Source the modules
# Assuming 'source_rscience_modules' is now a function inside your package R/ folder
fn3_source_rscience_modules(paths = path_modules_01_import, pattern = "^module_.*\\.R$")
fn3_source_rscience_modules(paths = path_modules_02_tools, pattern = "^module_.*\\.R$")



str_path_fn_local <- system.file("shiny/app002_01_import/fn_local.R", package = "Rscience3")
source(file = str_path_fn_local)
