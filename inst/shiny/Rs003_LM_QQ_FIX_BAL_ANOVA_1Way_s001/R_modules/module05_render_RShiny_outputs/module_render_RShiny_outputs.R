#' Theory Module UI
module_render_RShiny_outputs_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("main_render_RShiny"))
}




module_render_RShiny_outputs_server <- function(id, app_state) {

  # Pasar información a la función interna
  moduleServer(id, function(input, output, session) {


    # 1. Session ---------------------------------------------------------------
    ns <- session$ns

    # 2. Libreries -------------------------------------------------------------
    library("dplyr")
    library("shinyjs")
    library("stringr")


    # 3. Hardcoded -------------------------------------------------------------
    my_color_blue   <- "#0d6efd"  # Blue - Bootstrap primary
    my_color_green  <- "#198754"  # Green - Bootstrap success
    my_color_orange <- "#fd7e14"  # Orange - Bootstrap warning
    my_color_red    <- "#dc3545"  # Red - Bootstrap danger

    # MY_PACKAGE_NAME <- "Rscience3"

    # Loading my fns
    vector_fn_files <- list.files(path = ".", pattern = "^fn_.*\\.R$", full.names = TRUE)
    # lapply(vector_fn_files, source)
    invisible(lapply(vector_fn_files, source))
    # source(str_fn_app_inputs)
    # source(str_fn_app_general)

    # str_central_folder_path <- here::here()
    # MY_PACKAGE_NAME <-


    # Uso:
    MY_PACKAGE_NAME <- "Rscience3" #fn_app_MY_PACKAGE_NAME()
    str_central_folder_path <-getwd() #here::here()
    package_path <- fn_app_cut_path_to_package(full_path = str_central_folder_path, package_name = MY_PACKAGE_NAME)




    RBAG_list_R_output <- reactiveValues("R_obj" = list())
    observeEvent(input$play_RShiny,{

      # Folder FROM
      str_temp_work_folder_path <- app_state$str_temp_work_folder_path
      str_subfolder <- "zzz_zzz_output"
      str_subfolder_path <- file.path(str_temp_work_folder_path, str_subfolder)

      # Quarto config
      # str_quarto_file_name <- "_quarto.yml"
      # str_quarto_file_path <- file.path(str_subfolder_path, str_quarto_file_name)
      # library("yaml")
      # list_config <- yaml::read_yaml(str_quarto_file_path)

      # R script .R file
      # 1. Define the path
      str_R_internal_script_file_name <- "zzz_output_action02_lab01_step02_RData_internal.RData"
      str_R_internal_script_file_path <- file.path(str_subfolder_path, str_R_internal_script_file_name)

      # 2. Create an isolated environment
      temp_env <- new.env()
      load(str_R_internal_script_file_path, envir = temp_env)
      list_output <- as.list(temp_env)
      rm(temp_env)
      gc() # Force garbage collection
      RBAG_list_R_output$"R_obj" <- list_output
    })

    # ==========================================================================
    # 🔑 INICIO DE DEFINICIONES REACTIVAS CLAVE (MOVIDAS AL PRINCIPIO)
    # ==========================================================================
    app_state_render <- reactiveValues(is_running = FALSE) # DEFINICIÓN AQUÍ



    output$"main_render_RShiny" <- renderUI({

      div(
        actionButton(inputId = ns("play_RShiny"), label = "Play RShiny"),
        length(reactiveValuesToList(RBAG_list_R_output)$"R_obj"),
        verbatimTextOutput(ns("view01_user"))

      )

    })


    output$"view01_user" <- renderPrint({ # <-- Cambiado a renderPrint
      vector_selected <- c("df_selected_vars", "df_alpha_confidence")

      # Acceso directo al objeto reactivo (más eficiente)
      list_R_output <- RBAG_list_R_output$R_obj

      # Validación: que exista la lista y que tenga los elementos que buscas
      req(list_R_output)

      # Filtrar solo si los elementos existen para evitar errores de subíndice
      list_R_output[names(list_R_output) %in% vector_selected]
    })
    # return -------------------------------------------------------------
    return(app_state_render)


  })

}
