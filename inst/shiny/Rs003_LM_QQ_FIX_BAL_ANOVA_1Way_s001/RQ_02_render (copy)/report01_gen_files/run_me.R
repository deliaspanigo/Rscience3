
source(file = "fn_local.R")




#Cargar configuración desde _quarto.yml
library(yaml)
list_config <- yaml::read_yaml("_quarto.yml")





vector_external_qmd_file_path <- unlist(list_config$external_inputs$list_external_input_file_path)
vector_external_qmd_file_name <- basename(vector_external_qmd_file_path)


check_copy01 <- file.copy(from = vector_external_qmd_file_path,
                          to = basename(vector_external_qmd_file_path),
                          overwrite = TRUE)
check_copy01


vector_external_qmd_file_path <- unlist(list_config$external_inputs$list_external_input_file_path$str_file_path_qmd)
str_qmd_file_name_original    <- basename(vector_external_qmd_file_path)
str_qmd_file_name_copy        <- unlist(list_config$local_outputs$str_copy_qmd_file_name)

check_copy02 <- file.copy(from = str_qmd_file_name_original,
                          to = str_qmd_file_name_copy, overwrite = T)

check_copy02

# 
vector_lines <- readLines(str_qmd_file_name_copy, warn = FALSE, encoding = "UTF-8")

# 3. Aplicar las modificaciones (gsub)
# Ejemplo: Cambiar el título en el YAML y una variable en el código
vector_lines <- gsub('__import_external_my_dataset__', 'get("mtcars")', vector_lines)
vector_lines <- gsub('__import_internal_my_dataset__', 'get("mtcars")', vector_lines)
vector_lines <- gsub('__var_name_rv__', '"mpg"', vector_lines)
vector_lines <- gsub('__var_name_factor__', '"cyl"', vector_lines)
vector_lines <- gsub('__alpha_value__', '0.05', vector_lines)
vector_lines <- gsub('__vector_ordered_levels__', 'c("8", "4", "6")', vector_lines)
vector_lines <- gsub('__vector_ordered_colors__', 'c("#FF0000", "#00FF00", "#0000FF")', vector_lines)

# 4. Guardar los cambios en el mismo archivo
# useBytes = TRUE es fundamental para que R no cambie la codificación al escribir
writeLines(vector_lines, str_qmd_file_name_copy, useBytes = TRUE)



vector_files <- vector_external_qmd_file_name
vector_files[1] <- str_qmd_file_name_copy

str_file_path_R_script_internal <- list_config$local_outputs$str_R_script_internal
str_file_path_R_script_external <- list_config$local_outputs$str_R_script_external

fn_get_marked_code_from_qmd(vector_files = vector_files, 
                            str_output_file_path = str_file_path_R_script_internal, 
                            marker_string = "code_internal\\s*=\\s*(TRUE|T)")


fn_get_marked_code_from_qmd(vector_files = vector_files, 
                            str_output_file_path = str_file_path_R_script_external, 
                            marker_string = "code_external\\s*=\\s*(TRUE|T)")



file_name_R_script <- str_file_path_R_script_internal
file_name_RData    <- list_config$local_outputs$str_R_internal_RData

temp_env <- new.env()
source(file = str_file_path_R_script_internal, local = temp_env)
objects_to_save <- ls(envir = temp_env)

save(list = objects_to_save, 
     file = file_name_RData, 
     envir = temp_env)

# 3. (Opcional) Limpiar
rm(temp_env)
gc()



str_zzz_output <- list_config$local_outputs$str_regex_inclusion_files
str_final_output_folder <- list_config$external_output$str_user_output_folder_path

# 1. Setup paths from your config list
target_pattern <- list_config$local_outputs$str_regex_inclusion_files
destination_dir <- list_config$external_output$str_user_output_folder_path



fn_copying_files_from_pattern(target_pattern, destination_dir)


