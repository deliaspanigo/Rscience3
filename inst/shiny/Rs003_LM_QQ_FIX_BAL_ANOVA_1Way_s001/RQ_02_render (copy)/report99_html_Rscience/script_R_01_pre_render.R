# pre_render_script.R
# This script executes BEFORE Quarto rendering to set up the environment.
cat("🚀 Executing PRE-RENDER script...\n\n")



# ------------------------------------------------------------------------------
# 2. CLEANUP - Remove previous zzz_output_ files
# ------------------------------------------------------------------------------
# Lista de carpetas ocultas o temporales que genera Quarto
carpetas_a_borrar <- c(".quarto") 

# Borrado seguro
unlink(carpetas_a_borrar, recursive = TRUE, force = TRUE)