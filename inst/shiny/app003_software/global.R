# 1. Definir la ruta de la carpeta
source(file = "fn_local.R")
path_modules_01_import <- "../app002_01_import/RShiny_module_folder"
path_modules_02_tools <- "../app002_02_tools/RShiny_module_folder"


source_rscience_modules(paths = path_modules_01_import, pattern = "^module_.*\\.R$")
source_rscience_modules(paths = path_modules_02_tools, pattern = "^module_.*\\.R$")

