#' Theory Module UI
module_inputs_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("main_inputs_general"))
}



module_inputs_server <- function(id) {

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
    str_fn_app_inputs  <- "fn_app_inputs.R" #here("fn_app_inputs.R")
    str_fn_app_general <- "fn_app_general.R" #here("fn_app_general.R")
    source(str_fn_app_inputs)
    source(str_fn_app_general)

    # str_central_folder_path <- here::here()
    # MY_PACKAGE_NAME <-


    # Uso:
    MY_PACKAGE_NAME <- "Rscience3"
    str_central_folder_path <- getwd()
    package_path <- fn_app_cut_path_to_package(full_path = str_central_folder_path, package_name = MY_PACKAGE_NAME)



    # Uso:
    # str_central_folder_path <- here::here()
    # MY_PACKAGE_NAME <- fn_app_MY_PACKAGE_NAME()







    # 5. Loading fns from the original R folder from our package. --------------
    ## 5.1 Package folder path
    # Our "fn_app.R" folder must be inside our package folder.
    # We cut the path
    find_description_folder <- function(start_dir = ".", max_depth = 10) {
      # Busca recursivamente el archivo DESCRIPTION
      desc_files <- fs::dir_ls(
        path = start_dir,
        recurse = max_depth,
        type = "file",
        regexp = "DESCRIPTION$"
      )

      if (length(desc_files) == 0) {
        message("No DESCRIPTION file found within ", max_depth, " levels.")
        return(NULL)
      }

      # Obtiene los directorios padres
      desc_dirs <- fs::path_dir(desc_files)

      # Normaliza las rutas
      normalized_dirs <- normalizePath(desc_dirs)

      # Retorna todas las carpetas encontradas
      return(normalized_dirs)
    }

    # Uso:
    # folders <- find_description_folder()

    # str_package_folder_path <- str_extract(str_fn_app_file_path, paste0(".*?", MY_PACKAGE_NAME, "/?"))
    str_package_folder_path <- package_path #find_description_folder()
    # str_package_folder_path <- here("../", "../", "../", "../")
    ## 5.2 Sub folder R from our package
    sub_folder_R <- "R"
    str_R_folder_path <- file.path(str_package_folder_path, sub_folder_R)

    # 5.3 Load and source all R function files from R subfolder from our package
    load_custom_functions <- function() {
      if (!dir.exists(str_R_folder_path)) {
        warning("R folder does not exist: ", str_R_folder_path)
        return(FALSE)
      }

      r_files <- list.files(
        path = str_R_folder_path,
        pattern = "\\.R$",
        full.names = TRUE,
        ignore.case = TRUE
      )

      if (length(r_files) == 0) {
        warning("No R script files found in: ", str_R_folder_path)
        return(FALSE)
      }

      success_count <- 0
      for (file_path in r_files) {
        tryCatch({
          source(file_path, encoding = "UTF-8", local = FALSE)
          message("✓ Loaded: ", basename(file_path))
          success_count <- success_count + 1
        }, error = function(e) {
          warning("Failed to load: ", basename(file_path), " - Error: ", e$message)
        })
      }

      message(success_count, " of ", length(r_files), " function files loaded successfully.")
      return(success_count > 0)
    }
    load_custom_functions()

    # 6. Totem package ---------------------------------------------------------
    TOTEM_package <- reactiveValues()
    TOTEM_package$"folder_path" <- NULL
    TOTEM_package$"check" <- NULL
    TOTEM_package$"name" <- NULL

    observe({
      TOTEM_package$"folder_path_package" <- fn_app_find_my_folder_path_package(MY_PACKAGE_NAME = MY_PACKAGE_NAME)
      TOTEM_package$"check" <- dir.exists(TOTEM_package$"folder_path_package")
      TOTEM_package$"name"  <- MY_PACKAGE_NAME
    })

    # 7. Totem app -------------------------------------------------------------
    TOTEM_current_app <- reactiveValues()
    TOTEM_current_app$"folder_path" <- NULL
    TOTEM_current_app$"check" <- NULL
    TOTEM_current_app$"name"  <- NULL

    # observe({
    #   TOTEM_current_app$"folder_path" <- str_target_folder_path
    #   TOTEM_current_app$"check" <- dir.exists(TOTEM_current_app$"folder_path")
    #   TOTEM_current_app$"name" <- basename(TOTEM_current_app$"folder_path")
    # })


    # list_btn_class <- reactiveValues()
    # list_btn_class$"btn_dataset"      <- "btn-primary"
    # list_btn_class$"btn_var_selector" <- "btn-primary"
    # list_btn_class$"btn_settings"     <- "btn-primary"
    # list_btn_class$"btn_play_front"   <- "btn-primary"
    # list_btn_class$"btn_refresh"      <- "btn-primary"
    str_style_btn <- "width: 90px; height: 90px; display: flex; align-items: center; justify-content: center; margin-bottom: 8px;"
    str_style_icon <- "font-size: 50px; display: block; margin: 0 auto;"

    buttons_list_default <- list(
      list(id    = "btn_dataset",
           label = tagList(icon("database", style = str_style_icon)),
           class = "btn-primary",
           style = str_style_btn,
           title = "Dataset"
           ),
      list(id    = "btn_var_selector",
           label = tagList(icon("filter", style = str_style_icon)),
           class = "btn-primary",
           style = str_style_btn,
           title = "Variable Selector"
          ),
      list(id    = "btn_settings",
           label = tagList(icon("sliders", style = str_style_icon)),
           class = "btn-primary",
           style = str_style_btn,
           title = "Settings"
      ),
      list(id    = "btn_play_front",
           label = tagList(icon("play", style = str_style_icon)),
           class = "btn-primary",
           style = str_style_btn,
           title = "Play!"
      ),
      list(id    = "btn_refresh",
           label = tagList(icon("arrows-rotate", style = str_style_icon)),
           class = "btn-primary",
           style = str_style_btn,
           title = "Refresh"
      )
    )


    # USO:
    buttons_controller <- fn_app_init_button_list_reactive(buttons_list_default)

    # Acceder al reactiveValues


    # Resetear a valores default
    # buttons_controller$reset()
    #
    # buttons_list_mod <- reactiveValues()



    output$"menu_input" <- renderUI({

      buttons_list_mod <- buttons_controller$"reactive_obj"

      tagList(
        lapply(buttons_list_mod, function(btn) {
          actionButton(
            inputId = ns(btn$id),
            label = btn$label,
            class = btn$class,
            style = btn$style,
            title = btn$title
          )
        })
      )
    })




    # 5. All Dynamic UI generation for theory tabs inside one renderUI


    output$"main_inputs_general" <- renderUI({


      str_style_NAV_PANEL <- "flex-grow: 1; overflow-y: auto; height: 77vh; width: 100%; overflow: hidden;"


      bslib::navset_card_tab(
        full_screen = TRUE,
        title = tags$div(
          style = "
          min-height: 10px;
          padding: 0px;
        ",
          tags$h4("Inputs")
        ),
        # id = ns("inputs_navset"),
        # height = "100%",  # ← Esto es clave para bslib
        # 1. user_selection

        bslib::nav_panel(
          title = "user_selection",
        bslib::card_body(
          fillable = TRUE,
          # style = "height: 100%; width: 100%; padding: 0;",
          fluidRow(
            column(2, uiOutput(ns("menu_input"))),
            column(10,
              tags$div(
                style = str_style_NAV_PANEL,  # ← Padding general y gap

                # 20% - Primera sección con padding
                div(
                  style = "flex: 0 0 20%; min-height: 0; display: flex; flex-direction: column; padding: 10px;",  # ← Padding interno
                  div(
                    style = "flex: 1; min-height: 0; overflow-y: auto; overflow-x: hidden;",
                    fn_infoUI_zocalo_01_dataset(data_obj = reactiveValuesToList(the_list01_Dataset_stone))
                  )
                ),

                # 20% - Segunda sección con padding
                div(
                  style = "flex: 0 0 20%; min-height: 0; display: flex; flex-direction: column; padding: 10px;",  # ← Padding interno
                  div(
                    style = "flex: 1; min-height: 0; overflow-y: auto; overflow-x: hidden;",
                    fn_infoUI_zocalo_02_VarSelection(data_obj = reactiveValuesToList(the_list02_VarSelection_stone))
                  )
                ),

                # 60% - Tercera sección con padding
                div(
                  style = "flex: 0 0 50%; min-height: 0; display: flex; flex-direction: column; padding: 10px;",  # ← Padding interno
                  fn_infoUI_zocalo_03_container(
                    data_obj = reactiveValuesToList(the_list03_SpecialSettigns_stone),
                    width = "100%",
                    height = "100%"
                  )
                )
              )
                 )
          )
        )
        ),
        bslib::nav_panel(
          title = "dataset",
          style = "height: 100%; width: 100%;",
          bslib::card_body(
            fillable = TRUE,
            style = "height: 100%; width: 100%;",
            h4("Dataset"),
            tableOutput(ns("df_my_dataset"))
          )
        ),
        bslib::nav_panel(
          title = "minidataset",
          # style = "height: 100%; width: 100%;",
          bslib::card_body(
            fillable = TRUE,
            # style = "height: 100%; width: 100%;",
            h4("minidataset"),
            tableOutput(ns("df_my_minidataset"))
          )
        ),
        bslib::nav_panel(
          title = "control",
          style = "height: 100%; width: 100%;",
          bslib::card_body(
            fillable = TRUE,
            style = "height: 100%; width: 100%;",
            h4("Control"),
            tags$div(
              # style = "flex-grow: 1; overflow-y: auto;",
              # style = str_style_NAV_PANEL, # Asegurar que el contenedor tenga altura suficiente

              "- Original vs. Filtered Row Count.", br(),
              "- Rows Removed Due to Missing Data (NA) in selected columns.", br(),
              "- Min/Max by Factor Level for the Response Variable (RV)", br(),

              tags$hr(style = "border-top: 3px solid #000000;"),
              tags$div(
                # Aplicamos Flexbox para control vertical
                # style = "display: flex; flex-direction: column; height: 60vh; overflow-y: auto; padding: 10px;",

                # Elementos que deben fluir
                DT::DTOutput(ns("df_control01")),

                tags$hr(style = "border-top: 3px solid #000000;"),

                DT::DTOutput(ns("df_control02")),

                tags$hr(style = "border-top: 3px solid #000000;")#,

                # Aseguramos que el Plotly tenga un alto que respete el contenedor
                # plotlyOutput por defecto puede ser muy alto o tener un alto fijo.
                # plotly::plotlyOutput("control03_plotly", height = "600px") # Dale un alto inicial manejable
              )
            )
          )
        )
      )
    #   ## 5.1 Style for navigation panels
    #   str_style_NAV_PANEL <- "flex-grow: 1; overflow-y: auto; height: 72vh; width: 100%; overflow: hidden;"
    #
    #
    #   ## 5.2 Create nav panels dynamically
    #   nav_panels <- lapply(theory_list, function(item) {
    #
    #     # Generate output ID for this theory item WITH namespace
    #     output_id <- ns(paste0("html_", gsub("^theory_", "", item$id)))  # CORRECTED
    #
    #     # Create nav panel
    #     bslib::nav_panel(
    #       title = item$label,
    #       fluidRow(
    #         column(2, h4(item$label)),
    #         column(9),
    #         column(1,
    #                actionButton(inputId = ns(paste0("open_", item$id)),  # CORRECTED
    #                             label = NULL,
    #                             icon = icon("binoculars", class = "fa-2x"),
    #                             class = "btn-warning btn-sm"))
    #       ),
    #       tags$div(
    #         style = str_style_NAV_PANEL,
    #         htmlOutput(ns(output_id))  # Already namespaced above
    #       )
    #     )
    #   })
    #
    #   ## 5.3 Create the navset with dynamic panels
    #   do.call(bslib::navset_card_tab, c(
    #     list(
    #       full_screen = TRUE,
    #       title = tags$div(
    #         style = "
    #       min-height: 10px;
    #       padding: 0px;
    #     ",
    #         tags$h4("ClassRoom")
    #       ),
    #       id = ns("theory_navset")  # CORRECTED - added ID for reference
    #     ),
    #     nav_panels
    #   ))
    #
    #
    })


    output$"df_my_dataset" <- renderTable({
      the_list01_Dataset_stone$"my_dataset"
      # the_list01_Dataset_internal()$"my_dataset"
    })

    output$"df_my_minidataset" <- renderTable({
      the_list02_VarSelection_stone$"minidataset"
    })

    # 6. Processing...

    # Standard module for dataset loading (MASTER_module_import - SERVER) ------
    the_list01_Dataset_internal <- MASTER_module_import_server(id = "MASTER_import", show_dev = FALSE)

    # Standard module for dataset loading (MASTER_module_import - UI) ----------
    output$"super_dataset_selection" <- renderUI({

      # Standard module for dataset loading (MASTER_module_import - UI)
      MASTER_module_import_ui(id = ns("MASTER_import"))

    })

    # Stone 01 - Dataset - Default Values --------------------------------------
    the_list01_Dataset_R_default <- list("source" = NA,
                                         "file" = NA,
                                         "str_shape"= NA,
                                         "my_dataset" = NA,
                                         "info_status" = "waiting",
                                         "info_check_go_forward" = FALSE,
                                         "info_color" = my_color_blue)

    the_list01_Dataset_stone <- reactiveValues()
    fn_app_set_reactive_values_from_list(
      rv = the_list01_Dataset_stone,
      data_list = the_list01_Dataset_R_default,
      list_default = the_list01_Dataset_R_default
    )


    # Stone 02 - Var Selection - Default Values --------------------------------
    the_list02_VarSelection_R_default <- list("var_name_factor" = NA,
                                              "var_name_rv" = NA,
                                              "alpha_value" = NA,
                                              "vector_var_names" = NA,
                                              "minidataset" = NA,
                                              "ncol" = NA,
                                              "nrow" = NA,
                                              "str_shape" = NA,
                                              "info_status" = "waiting",
                                              "info_check_go_forward" = FALSE,
                                              "info_color" = my_color_blue)

    the_list02_VarSelection_stone <- reactiveValues()
    fn_app_set_reactive_values_from_list(rv = the_list02_VarSelection_stone,
                                         data_list = the_list02_VarSelection_R_default,
                                         list_default = the_list02_VarSelection_R_default)

    # Stone 03 - SpecialSettings - Default Values ------------------------------
    the_list03_SpecialSettigns_R_default <- list("df_order" = NA,
                                                 "vector_ordered_levels" = NA,
                                                 "vector_ordered_colors" = NA,
                                                 "minidataset" = NA,
                                                 "nrow" = NA,
                                                 "ncol" = NA,
                                                 "info_status" = "waiting",
                                                 "info_check_go_forward" = FALSE,
                                                 "info_color" = my_color_blue,
                                                 "shiny_obj_name" = NA)

    the_list03_SpecialSettigns_stone <- reactiveValues()
    fn_app_set_reactive_values_from_list(rv = the_list03_SpecialSettigns_stone,
                                         data_list = the_list03_SpecialSettigns_R_default,
                                         list_default = the_list03_SpecialSettigns_R_default)

    # Stone 04 - Play - Default Values ------------------------------
    the_list04_Play_default <- list("run" = FALSE,
                                    "status_info" = "waiting")

    the_list04_Play_stone <- reactiveValues()
    fn_app_set_reactive_values_from_list(rv = the_list04_Play_stone,
                                         data_list = the_list04_Play_default,
                                         list_default = the_list04_Play_default)

    ###---------------------------------------------------------------------------

    # validates fns
    fn_internal_validate_Dataset <- function() {

      # Verificar si hay dataset cargado
      if (!the_list01_Dataset_stone$"info_check_go_forward") {
        showNotification(
          "Please, select a dataset.",
          type = "warning"
        )
        return(FALSE)
      }

      return(TRUE)
    }
    fn_internal_validate_VarSelection <- function() {

      # Verificar si hay dataset cargado
      if (!the_list02_VarSelection_stone$"info_check_go_forward") {
        showNotification(
          "Please, select your variables.",
          type = "warning"
        )
        return(FALSE)
      }

      return(TRUE)
    }
    fn_internal_validate_SpecialSettigns <- function() {

      # Verificar si hay dataset cargado
      if (!the_list03_SpecialSettigns_stone$"info_check_go_forward") {
        showNotification(
          "Please, select your special settings.",
          type = "warning"
        )
        return(FALSE)
      }

      return(TRUE)
    }
    ###---------------------------------------------------------------------------

    # Stone 01 - the_list01_Dataset
    observeEvent(input$btn_dataset, {


      showModal(
        modalDialog(
          size = "xl",
          easyClose = FALSE,

          # Aplicamos estilos personalizados para hacer el modal más grande y posicionarlo más arriba
          tags$div(
            tags$style(HTML("
        /* Hacer que el modal sea más grande que xl - ancho y alto */
        .modal-xl {
          max-width: 95% !important; /* Aumentamos el ancho a 95% de la ventana */
          width: 95%;
        }

        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-top: 20px !important; /* Reducimos el margen superior (valor por defecto es 1.75rem ~28px) */
        }

        /* Hacer que el contenido del modal ocupe más espacio vertical */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }

        /* Ajustar el cuerpo del modal para que ocupe el espacio disponible */
        .modal-body {
          flex: 1;
          overflow: hidden; /* Evita scroll doble */
          padding: 0; /* Quitamos padding para maximizar espacio */
        }

        /* Asegurar que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* O el tamaño máximo que prefieras */
          }
        }
      ")),
          ),

          # Contenedor para el módulo de importación - ahora ocupa todo el espacio disponible
          div(
            style = "height: 100%; overflow-y: auto; padding: 15px;",
            uiOutput(ns("super_dataset_selection"))
            # Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
          ),

          footer = tags$div(
            style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
            # Botón Cancelar de ancho completo
            tags$button(
              id = ns("btn_cancel01"),
              type = "button",
              class = "btn btn-default",
              style = "width: 50%; height: 45px;", # Aumentado la altura
              "data-bs-dismiss" = "modal",
              "CANCEL"
            ),
            actionButton(inputId = ns("confirm_action01"), label = "ADD",
                         class = "btn-primary", style = "width: 50%; height: 45px;") # Aumentado la altura

          )

        )
      )



    })
    observeEvent(input$confirm_action01, {


      if (is.null(the_list01_Dataset_internal()$"my_dataset")) {
        showNotification(
          "Please, select a dataset.",
          type = "warning"
        )
        return()
      }


      # All Ok...
      # 1) Show notification
      fn_show_notification_ok(the_message = "Dataset imported successfully.")


      buttons_controller$"reactive_obj"$"btn_dataset"$"class" <- "btn-success"

      # updateButtonClass(session, "btn_dataset", "btn-primary", "btn-success")
      # 3) Basics
      the_nrow <- nrow(the_list01_Dataset_internal()$"my_dataset")
      the_ncol <- ncol(the_list01_Dataset_internal()$"my_dataset")
      the_str_shape <- paste0(the_nrow, " Rows", " x ", the_ncol, " Cols")

      # 3) Put on stone
      the_list01_Dataset_stone$"source" <- the_list01_Dataset_internal()[["data_source"]]
      the_list01_Dataset_stone$"file"   <- the_list01_Dataset_internal()[["original_file_name"]]
      the_list01_Dataset_stone$"str_shape"  <- the_str_shape
      the_list01_Dataset_stone$"my_dataset" <- the_list01_Dataset_internal()$"my_dataset"
      the_list01_Dataset_stone$"info_status" <- "done"
      the_list01_Dataset_stone$"info_check_go_forward" <- TRUE
      the_list01_Dataset_stone$"info_color" <- my_color_green

      # 4) Remove Modal
      removeModal()



    })



    ###---------------------------------------------------------------------------



    # Stone 02 - the_list02_VarSelection
    output$"var_selection" <- renderUI({
      req(the_list01_Dataset_internal())

      amount_cols <- ncol(the_list01_Dataset_internal()$"my_dataset")
      amount_digits <- nchar(as.character(amount_cols))
      if(amount_digits == 1) amount_digits <- amount_digits + 1
      str_new <- paste0("%0", amount_digits, "d")
      vector_orden <- sprintf(str_new, 1:amount_cols)

      vector_colnames <- colnames(the_list01_Dataset_internal()$"my_dataset")
      #vector_colnames <- paste0(vector_orden, " - ", vector_colnames, " - ", openxlsx::int2col(1:length(vector_colnames)))
      vector_output_names <- paste0("Var ", vector_orden, " - Column ", openxlsx::int2col(1:length(vector_colnames)), " - ", vector_colnames)
      names(vector_colnames) <- vector_output_names
      vector_colnames <- c("Select a variable..." = "", vector_colnames)

      vector_alpha <- c("0.10 (10%)" = "0.10",
                        "0.05 (5%)" = "0.05",
                        "0.01 (1%)" = "0.01")
      div(
        selectInput(inputId = ns("var_name_rv"), label = "Response Variable (RV)", choices = vector_colnames),
        selectInput(inputId = ns("var_name_factor"), label = "Factor", choices = vector_colnames),
        selectInput(inputId = ns("alpha_value"), label = "Alpha value", choices = vector_alpha, selected = vector_alpha[2])

      )
    })
    observeEvent(input$"btn_var_selector", {

      # Validate before continue...
      if (!fn_internal_validate_Dataset()) return()






      # 2. Mostramos el modal con el contenido del módulo ya inicializado
      # Usando tamaño "xl" (extra large)
      showModal(
        modalDialog(
          # title = "Seleccionar Base de Datos",
          size = "xl", # Mantenemos "xl" como base
          easyClose = TRUE,

          # Aplicamos estilos personalizados para hacer el modal más grande y posicionarlo más arriba
          tags$div(
            tags$style(HTML("
        /* Hacer que el modal sea más grande que xl - ancho y alto */
        .modal-xl {
          max-width: 95% !important; /* Aumentamos el ancho a 95% de la ventana */
          width: 95%;
        }

        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-top: 20px !important; /* Reducimos el margen superior (valor por defecto es 1.75rem ~28px) */
        }

        /* Hacer que el contenido del modal ocupe más espacio vertical */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }

        /* Ajustar el cuerpo del modal para que ocupe el espacio disponible */
        .modal-body {
          flex: 1;
          overflow: hidden; /* Evita scroll doble */
          padding: 0; /* Quitamos padding para maximizar espacio */
        }

        /* Asegurar que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* O el tamaño máximo que prefieras */
          }
        }
      ")),
          ),

          # Contenedor para el módulo de importación - ahora ocupa todo el espacio disponible
          div(
            style = "height: 100%; overflow-y: auto; padding: 15px;",
            uiOutput(ns("var_selection"))
            # Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
          ),

          footer = tags$div(
            style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
            # Botón Cancelar de ancho completo
            tags$button(
              id = ns("btn_cancel02"),
              type = "button",
              class = "btn btn-default",
              style = "width: 50%; height: 45px;", # Aumentado la altura
              "data-bs-dismiss" = "modal",
              "CANCEL"
            ),
            actionButton(inputId = ns("confirm_action02"), label = "ADD",
                         class = "btn-primary", style = "width: 50%; height: 45px;") # Aumentado la altura

          )

        )
      )



    })
    observeEvent(input$confirm_action02, {

      ns <- session$ns

      # # # Hace falta modificar la funcion de importacion
      # para que tenga un objeto como "check_output" con T o F, y que ese
      # valor se resetee cada vez que hay un cambio de selecion de datos.
      # Creo que debo crear como sif uera un "internal_DATA".

      # req(the_list01_Dataset_internal())
      # 1) Hacer validaciones sobre la importacion realizada.
      #    Si todo esta bien...
      # 2) Asignar nuevos valores a "valores_internos".
      # 3) Cerrar el modal
      # Verificar que se haya seleccionado un dataset primero
      # print(the_list01_Dataset_internal())
      if (is.null(the_list01_Dataset_internal()$"my_dataset")) {
        # print(the_list01_Dataset_internal())
        showNotification(
          "Please, select a dataset.",
          type = "warning"
        )

        return()
      }




      # 1) Show notification
      fn_show_notification_ok(the_message = "Variable selection selected successfully.")

      # 2) Change color on botton
      buttons_controller$"reactive_obj"$"btn_var_selector"$"class" <- "btn-success"

      # shinyjs::removeClass(id = ns("btn_var_selector"), class = "btn-primary")
      # shinyjs::addClass(id = ns("btn_var_selector"),  class = "btn-success")

      # 3) Put on stone
      vector_var_names <- c(input$"var_name_rv", input$"var_name_factor")
      minidataset <- the_list01_Dataset_internal()$"my_dataset"[vector_var_names]
      minidataset[,input$"var_name_factor"] <- as.factor(as.character(minidataset[,input$"var_name_factor"]))

      the_list02_VarSelection_stone$"var_name_factor" <- input$"var_name_factor"
      the_list02_VarSelection_stone$"var_name_rv" <- input$"var_name_rv"
      the_list02_VarSelection_stone$"alpha_value" <- input$"alpha_value"
      the_list02_VarSelection_stone$"vector_var_names" <- vector_var_names
      the_list02_VarSelection_stone$"minidataset" <- minidataset
      the_list02_VarSelection_stone$"ncol" <- ncol(minidataset)
      the_list02_VarSelection_stone$"nrow" <- nrow(minidataset)
      the_list02_VarSelection_stone$"str_shape" <- paste0(nrow(minidataset), " Rows x ", ncol(minidataset), " Cols")
      the_list02_VarSelection_stone$"info_status" <- "done"
      the_list02_VarSelection_stone$"info_check_go_forward" <- TRUE
      the_list02_VarSelection_stone$"info_color" <- my_color_green
      # 4) Remove Modal
      removeModal()

    })
    ###---------------------------------------------------------------------------


    # Stone 03 - the_list03_SpecialSettigns
    output$settings_selection <- renderUI({
      req(the_list02_VarSelection_stone$"minidataset")

      minidataset <- the_list02_VarSelection_stone$"minidataset"
      var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"

      # 1. Obtener los niveles del factor
      vector_levels <- levels(minidataset[, var_name_factor])
      num_levels <- length(vector_levels)

      if (num_levels == 0) {
        return(p("No se encontraron niveles en la variable factor seleccionada."))
      }

      # 2. Definir una paleta de colores por defecto (hasta 8 colores distintos)
      # Si hay más de 8 niveles, puedes usar una paleta más grande o 'viridis'/'rainbow'
      # default_colors <- setNames(
      #   RColorBrewer::brewer.pal(min(num_levels, 8), "Dark2"),
      #   vector_levels[1:min(num_levels, 8)]
      # )
      default_colors <- setNames(
        rainbow(num_levels),
        vector_levels[1:num_levels]
      )
      # 3. Preparar las opciones de orden (del 1 al N)
      order_choices <- 1:num_levels

      # 4. Generar la lista de inputs dinámicos usando lapply
      # Cada elemento de la lista será un div conteniendo el selector de orden y el selector de color.
      level_inputs <- lapply(seq_along(vector_levels), function(i) {
        level <- vector_levels[i]
        default_color <- default_colors[i] #if (i <= 8) default_colors[i] else "#CCCCCC"

        # Usamos fluidRow para que los inputs se muestren uno al lado del otro
        fluidRow(
          id = paste0("config_row_", level),

          # Selector de Orden (el usuario asigna la posición deseada)
          column(4,
                 selectInput(
                   inputId = ns(paste0("order_", level)),
                   label = paste("Level:", level),
                   choices = order_choices,
                   selected = i # Orden inicial por defecto es la posición actual
                 )
          ),

          # Selector de Color
          column(4,
                 colourpicker::colourInput(
                   inputId = ns(paste0("color_", level)),
                   label = paste("Color:", level),
                   value = default_color,
                   showColour = "background"
                 )
          )
        )
      })

      # 5. Devolver todos los elementos generados
      tagList(
        # h3(icon("sliders-h"), "Configuración de Niveles"),
        # p("Defina el orden de visualización y el color para cada categoría:"),
        level_inputs
      )
    })
    the_list03_SpecialSettigns_internal <- reactive({
      # req(the_list01_Dataset_stone)
      # req(the_list02_VarSelection_stone)
      req(the_list02_VarSelection_stone$"minidataset")
      req(the_list02_VarSelection_stone$"var_name_factor")
      vector_levels <- levels(the_list02_VarSelection_stone$"minidataset"[, the_list02_VarSelection_stone$"var_name_factor"])
      req(vector_levels)
      # 1. Crear un data.frame para almacenar las configuraciones
      settings_df <- data.frame(
        level = vector_levels,
        order = rep(NA, length(vector_levels)), #NA_integer_,
        color = rep(NA, length(vector_levels)), #NA_character_,
        stringsAsFactors = FALSE
      )

      # 2. Iterar y capturar los valores de input
      for (level in vector_levels) {
        # Captura el valor del input de orden
        order_val <- input[[paste0("order_", level)]]

        # Captura el valor del input de color
        color_val <- input[[paste0("color_", level)]]

        # Asigna los valores al data.frame
        idx <- which(settings_df$level == level)
        if (!is.null(order_val)) {
          settings_df$order[idx] <- as.integer(order_val)
        }
        if (!is.null(color_val)) {
          settings_df$color[idx] <- color_val
        }
      }

      # 3. Ordenar el data.frame según la elección del usuario y devolverlo
      # Esto te dará el orden final de los niveles.
      df_order <- settings_df[order(settings_df$order), ]
      vector_ordered_levels <- df_order$level
      vector_ordered_colors <- df_order$color

      output_list <- list()
      output_list$"df_order" <- df_order
      output_list$"vector_ordered_levels" <- vector_ordered_levels
      output_list$"vector_ordered_colors" <- vector_ordered_colors

      output_list

    })

    output$settings_table_display <- DT::renderDT({
      # Requiere la función reactiva que has definido
      req(the_list03_SpecialSettigns_internal())

      settings_df <- the_list03_SpecialSettigns_internal()$"df_order"

      # 2. Renombrar columnas para la visualización
      settings_df <- settings_df %>%
        dplyr::select(
          Level = level,
          Order = order,
          ColorCode = color
        )

      # 3. Crear una columna HTML para mostrar el color
      # Esta columna contendrá un pequeño div con el color de fondo.
      settings_df$ColorSwatch <- paste0(
        '<div style="width: 100%; height: 20px; background-color:',
        settings_df$ColorCode,
        '; border: 1px solid #000; border-radius: 3px;"></div>'
      )

      # 4. Seleccionar y ordenar las columnas para el display final
      final_display_df <- settings_df %>%
        dplyr::select(
          "Level" = Level,
          "Order" = Order,
          "Color" = ColorSwatch,
          "Hex Code" = ColorCode
        )

      # 5. Renderizar la tabla con DT, indicando que la columna 'Color' es HTML
      DT::datatable(
        final_display_df,
        escape = c("Level", "Order", "Hex Code"), # Solo escapa (trata como texto) estas columnas
        options = list(
          dom = 't', # Muestra solo la tabla (t) sin búsqueda, info, etc.
          paging = FALSE,
          ordering = FALSE
        ),
        rownames = FALSE # Oculta los números de fila
      )
    })
    output$settings_table_display02 <- DT::renderDT({
      # Requiere la función reactiva que has definido
      req(the_list03_SpecialSettigns_stone$"df_order")

      settings_df <- the_list03_SpecialSettigns_stone$"df_order"

      # 2. Renombrar columnas para la visualización
      settings_df <- settings_df %>%
        dplyr::select(
          Level = level,
          Order = order,
          ColorCode = color
        )

      # 3. Crear una columna HTML para mostrar el color
      # Esta columna contendrá un pequeño div con el color de fondo.
      settings_df$ColorSwatch <- paste0(
        '<div style="width: 100%; height: 20px; background-color:',
        settings_df$ColorCode,
        '; border: 1px solid #000; border-radius: 3px;"></div>'
      )

      # 4. Seleccionar y ordenar las columnas para el display final
      final_display_df <- settings_df %>%
        dplyr::select(
          "Nivel" = Level,
          "Orden" = Order,
          "Color" = ColorSwatch,
          "Hex Cod" = ColorCode
        )

      # 5. Renderizar la tabla con DT, indicando que la columna 'Color' es HTML
      DT::datatable(
        final_display_df,
        escape = c("Nivel", "Orden", "Hex Cod"), # Solo escapa (trata como texto) estas columnas
        options = list(
          dom = 't', # Muestra solo la tabla (t) sin búsqueda, info, etc.
          paging = FALSE,
          ordering = FALSE
        ),
        rownames = FALSE # Oculta los números de fila
      )
    })
    observeEvent(input$"btn_settings", {

      # Validate before continue...
      if (!fn_internal_validate_Dataset()) return()
      if (!fn_internal_validate_VarSelection()) return()

      # 2. Mostramos el modal con el contenido del módulo ya inicializado
      # Usando tamaño "xl" (extra large)
      showModal(
        modalDialog(
          # title = "Seleccionar Base de Datos",
          size = "xl", # Mantenemos "xl" como base
          easyClose = TRUE,

          # Aplicamos estilos personalizados para hacer el modal más grande y posicionarlo más arriba
          tags$div(
            tags$style(HTML("
        /* Hacer que el modal sea más grande que xl - ancho y alto */
        .modal-xl {
          max-width: 95% !important; /* Aumentamos el ancho a 95% de la ventana */
          width: 95%;
        }

        /* Aumentar la altura del modal y posicionarlo más cerca del borde superior */
        .modal-dialog {
          height: 90vh !important; /* 90% de la altura de la ventana */
          max-height: 90vh !important;
          margin-top: 20px !important; /* Reducimos el margen superior (valor por defecto es 1.75rem ~28px) */
        }

        /* Hacer que el contenido del modal ocupe más espacio vertical */
        .modal-content {
          height: 100% !important;
          display: flex;
          flex-direction: column;
        }

        /* Ajustar el cuerpo del modal para que ocupe el espacio disponible */
        .modal-body {
          flex: 1;
          overflow: hidden; /* Evita scroll doble */
          padding: 0; /* Quitamos padding para maximizar espacio */
        }

        /* Asegurar que en pantallas muy grandes se mantenga un tamaño razonable */
        @media (min-width: 1400px) {
          .modal-xl {
            max-width: 1800px !important; /* O el tamaño máximo que prefieras */
          }
        }
      ")),
          ),

          # Contenedor para el módulo de importación - ahora ocupa todo el espacio disponible
          div(
            style = "height: 100%; overflow-y: auto; padding: 15px;",
            tagList(
              h3(icon("sliders-h"), "Configuración de Niveles"),
              p("Defina el orden de visualización y el color para cada categoría:"),
              # level_inputs
            ),
            fluidRow(
              column(6, uiOutput(ns("settings_selection"))),
              column(6, DT::DTOutput(ns("settings_table_display")))
            )
            # Rscience.import::MASTER_module_import_ui(id = ns("MASTER_import"))
          ),

          footer = tags$div(
            style = "display: flex; justify-content: center; width: 100%; gap: 10px;",
            # Botón Cancelar de ancho completo
            tags$button(
              id = ns("btn_cancel03"),
              type = "button",
              class = "btn btn-default",
              style = "width: 50%; height: 45px;", # Aumentado la altura
              "data-bs-dismiss" = "modal",
              "CANCEL"
            ),
            actionButton(inputId = ns("confirm_action03"), label = "ADD",
                         class = "btn-primary", style = "width: 50%; height: 45px;") # Aumentado la altura

          )

        )
      )



    })
    observeEvent(input$confirm_action03, {


      if (is.null(the_list01_Dataset_internal()$"my_dataset")) {
        # print(the_list01_Dataset_internal())
        showNotification(
          "Please, select a dataset.",
          type = "warning"
        )

        return()
      }




      # 1) Show notification
      fn_show_notification_ok(the_message = "Variable selection selected successfully.")

      # 2) Change color on botton
      # shinyjs::removeClass(id = "btn_settings", class = "btn-primary")
      # shinyjs::addClass(id = "btn_settings",  class = "btn-success")
      buttons_controller$"reactive_obj"$"btn_settings"$"class" <- "btn-success"


      # 3) Put on stone
      vector_ordered_levels <- the_list03_SpecialSettigns_internal()$"vector_ordered_levels"
      vector_ordered_colors <- the_list03_SpecialSettigns_internal()$"vector_ordered_colors"
      minidaset_without_change <- the_list02_VarSelection_stone$"minidataset"
      var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"

      minidaset_with_change <- minidaset_without_change
      minidaset_with_change[,var_name_factor] <- factor(
        x = minidaset_without_change[,var_name_factor],       # La variable original de factor
        levels = vector_ordered_levels  # El orden de los niveles que calculamos en el Paso 2
      )

      the_list03_SpecialSettigns_stone$"df_order" <-  the_list03_SpecialSettigns_internal()$"df_order"
      the_list03_SpecialSettigns_stone$"vector_ordered_levels" <- the_list03_SpecialSettigns_internal()$"vector_ordered_levels"
      the_list03_SpecialSettigns_stone$"vector_ordered_colors" <- the_list03_SpecialSettigns_internal()$"vector_ordered_colors"
      the_list03_SpecialSettigns_stone$"minidataset" <- minidaset_with_change
      the_list03_SpecialSettigns_stone$"nrow" <- nrow(minidaset_with_change)
      the_list03_SpecialSettigns_stone$"ncol" <- ncol(minidaset_with_change)
      the_list03_SpecialSettigns_stone$"info_status" <- "done"
      the_list03_SpecialSettigns_stone$"info_check_go_forward" <- TRUE
      the_list03_SpecialSettigns_stone$"info_color" <- my_color_green
      the_list03_SpecialSettigns_stone$"shiny_obj_name" <- ns("control03_plotly")#"settings_table_display02"

      # 4) Remove Modal
      removeModal()

    })

    ###---------------------------------------------------------------------------
    # Stone 04 - the_list03_SpecialSettigns
    observeEvent(input$"btn_play_front", {

      # Validate before continue...
      if (!fn_internal_validate_Dataset()) return()
      if (!fn_internal_validate_VarSelection()) return()
      if (!fn_internal_validate_SpecialSettigns()) return()



      the_list04_Play_stone$"run" <- TRUE
      the_list04_Play_stone$"status_info" = "All OK!"

      # buttons_controller$"reactive_obj"$"btn_play_font"$"class" <- "btn-success"
      buttons_controller$reactive_obj$btn_play_front$class <- "btn-success"




    # USO:

    })

    ###---------------------------------------------------------------------------
    output$"df_control01" <- DT::renderDataTable({
      # El código de configuración y cálculo
      req(the_list03_SpecialSettigns_stone$"minidataset")
      nrow_dataset <- nrow(the_list01_Dataset_internal()$"my_dataset")
      ncol_dataset <- ncol(the_list01_Dataset_internal()$"my_dataset")
      nrow_minidataset <- the_list03_SpecialSettigns_stone$"nrow"
      ncol_minidataset <- the_list03_SpecialSettigns_stone$"ncol"

      df_output <- data.frame(
        "source" = c("dataset", "minidataset"),
        "ncol" = c(ncol_dataset, ncol_minidataset),
        "nrow" = c(nrow_dataset, nrow_minidataset)
      )

      # 4. Seleccionar y ordenar las columnas para el display final
      final_display_df <- df_output %>%
        dplyr::select(
          "Source" = source,
          "Number of cols" = ncol,
          "Number of rows" = nrow
        )

      # 5. Renderizar la tabla con DT
      DT::datatable(
        final_display_df,
        # Usamos autoWidth para que ocupe el espacio mínimo
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          autoWidth = TRUE, # Ayuda a que la tabla no ocupe todo el ancho

          # ********* CLAVE DEL CENTRADO *********
          columnDefs = list(list(className = 'dt-center', targets = '_all'))
          # **************************************
        ),
        rownames = FALSE
      )
    })

    output$"df_control02" <- DT::renderDataTable({
      # El código de configuración y cálculo
      req(the_list03_SpecialSettigns_stone$"minidataset")
      # ... (Cálculos y preparación de final_display_df) ...

      minidataset <- the_list03_SpecialSettigns_stone$"minidataset"
      var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"
      var_name_rv <- the_list02_VarSelection_stone$"var_name_rv"

      # 2. Conversión y resumen
      minidataset[,var_name_factor] <- as.factor(minidataset[,var_name_factor])

      tabla_resumen <- minidataset %>%
        dplyr::group_by(across(all_of(var_name_factor))) %>%
        dplyr::summarise(
          n = n(),
          min = min(dplyr::across(all_of(var_name_rv))),
          max = max(dplyr::across(all_of(var_name_rv)))
        )

      settings_df <- the_list03_SpecialSettigns_stone$"df_order"

      # 2. Renombrar columnas para la visualización
      settings_df <- settings_df %>%
        dplyr::select(
          Level = level,
          Order = order,
          ColorCode = color
        )

      # 3. Crear una columna HTML para mostrar el color
      settings_df$ColorSwatch <- paste0(
        '<div style="width: 100%; height: 20px; background-color:',
        settings_df$ColorCode,
        '; border: 1px solid #000; border-radius: 3px;"></div>'
      )

      settings_df <- cbind.data.frame(settings_df, tabla_resumen)

      # 4. Seleccionar y ordenar las columnas para el display final
      final_display_df <- settings_df %>%
        dplyr::select(
          "Order" = Order,
          "Level" = Level,
          "n",
          "Min" = min,
          "Max" = max,
          "Color" = ColorSwatch,
          "Hex Cod" = ColorCode
        )

      # 5. Renderizar la tabla con DT
      DT::datatable(
        final_display_df,
        # ¡Asegúrate de marcar la columna 'Color' con I() o usa escape = FALSE si la tabla es simple!
        # El escape es correcto aquí: las columnas listadas se escapan (texto), Color no se escapa (HTML).
        escape = c("Order", "Level", "n", "Min", "Max", "Hex Cod"),
        options = list(
          # CLAVE: Indica a DataTables que intente ajustar el ancho de las columnas
          autoWidth = TRUE,
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE,
          # AÑADIR/MANTENER ESTO PARA CENTRAR TODAS LAS COLUMNAS
          columnDefs = list(list(className = 'dt-center', targets = '_all'))
        ),
        rownames = FALSE
      )

    }) # <-- ELIMINAMOS LA SECCIÓN DE OPCIONES EXTERNA

    output$"control03_plotly" <- plotly::renderPlotly({

      # Asegurarse de que los datos requeridos existen
      req(the_list03_SpecialSettigns_stone$"minidataset")
      minidataset <- the_list03_SpecialSettigns_stone$"minidataset"
      var_name_factor <- the_list02_VarSelection_stone$"var_name_factor"
      var_name_rv <- the_list02_VarSelection_stone$"var_name_rv"
      settings_df <- the_list03_SpecialSettigns_stone$"df_order"
      vector_ordered_levels <- the_list03_SpecialSettigns_stone$"vector_ordered_levels"
      vector_ordered_colors <- the_list03_SpecialSettigns_stone$"vector_ordered_colors"

      #################################
      df_rv_position_levels <- data.frame(
        "order_level"  = 1:nlevels(minidataset[,var_name_factor]),
        "level" = levels(minidataset[,var_name_factor]),
        "n"            = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], length),
        "variable"     = rep(var_name_rv, nlevels(minidataset[,var_name_factor])),
        "min"          = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], min),
        "mean"         = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], mean),
        "Q1"           = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], quantile, 0.25),
        "median"       = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], median),
        "Q3"           = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], quantile, 0.75),
        "max"          = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], max),
        "color" = vector_ordered_colors,
        stringsAsFactors = FALSE
      )
      df_rv_position_levels[,"level"] <- factor(
        x = df_rv_position_levels[,"level"],       # La variable original de factor
        levels = df_rv_position_levels[,"level"]  # El orden de los niveles que calculamos en el Paso 2
      )
      rownames(df_rv_position_levels) <- NULL

      df_table_factor_plot004 <- df_rv_position_levels
      ########################################################
      # --- CÓDIGO CLAVE DE LA SOLUCIÓN ---
      # 1. Crear el vector de números de fila secuenciales (1, 2, 3, ...)
      row_sequence <- 1:nrow(minidataset)

      # 2. Formatear el texto para el cursor (Ej: "Row: 1", "Row: 2", ...)
      hover_text <- paste0("Row: ", row_sequence)
      # ------------------------------------

      # # # New plotly...
      plot004_factor <- plotly::plot_ly()

      # # # Boxplot and info...
      plot004_factor <- plotly::add_trace(p = plot004_factor,
                                          type = "box",
                                          x = df_table_factor_plot004$level ,
                                          color = df_table_factor_plot004$level,
                                          colors = df_table_factor_plot004$color,
                                          lowerfence = df_table_factor_plot004$min,
                                          q1 = df_table_factor_plot004$Q1,
                                          median = df_table_factor_plot004$median,
                                          q3 = df_table_factor_plot004$Q3,
                                          upperfence = df_table_factor_plot004$max,
                                          boxmean = TRUE,
                                          boxpoints = FALSE,
                                          line = list(color = "black", width = 3)
      )

      # # # Title and settings...
      # plot004_factor <- plotly::layout(p = plot004_factor,
      #                                  title = "Plot 004 - Boxplot and means",
      #                                  font = list(size = 20),
      #                                  margin = list(t = 100))


      # # # Without zerolines...
      plot004_factor <- plotly::layout(p = plot004_factor,
                                       xaxis = list(zeroline = FALSE,
                                                    title = var_name_factor),
                                       yaxis = list(zeroline = FALSE,
                                                    title = var_name_rv),
                                       font = list(size = 20))

      # # # Output plot004_anova...
      plot004_factor

      # # Crear un nuevo plot
      # plot001_factor <- plotly::plot_ly()
      #
      # # Scatter plot
      # plot001_factor <- plotly::add_trace(p = plot001_factor,
      #                                     type = "scatter",
      #                                     mode = "markers",
      #                                     x = minidataset[,var_name_factor],
      #                                     y = minidataset[,var_name_rv],
      #                                     color = minidataset[,var_name_factor],
      #                                     colors = settings_df$color,
      #
      #                                     # *********************************
      #                                     # 1. ASIGNAR EL TEXTO DEL CURSOR:
      #                                     # Usamos el nuevo vector secuencial y formateado
      #                                     text = hover_text,
      #
      #                                     # 2. CONFIGURAR EL CONTENIDO DEL CURSOR:
      #                                     # Mantenemos 'text+x+y+name' para incluir el nuevo texto
      #                                     hoverinfo = 'text+x+y+name',
      #                                     # *********************************
      #
      #                                     marker = list(size = 15, opacity = 0.7))
      #
      # # Título y settings
      # # plot001_factor <- plotly::layout(p = plot001_factor,
      # #                                  title = "Scatterplot",
      # #                                  font = list(size = 20),
      # #                                  margin = list(t = 100))
      #
      # # Sin zerolines
      # plot001_factor <- plotly::layout(p = plot001_factor,
      #                                  xaxis = list(zeroline = FALSE, title = var_name_factor),
      #                                  yaxis = list(zeroline = FALSE, title = var_name_rv),
      #                                  font = list(size = 20))
      #
      # # El bloque renderPlotly debe devolver el objeto Plotly al final
      # plot001_factor
    })


    ###-------------------------------------------------------------------------

    observeEvent(input$"btn_refresh", {


      buttons_controller$reset()

      fn_app_reset_stone(rv = the_list01_Dataset_stone,         list_default = the_list01_Dataset_R_default)
      fn_app_reset_stone(rv = the_list02_VarSelection_stone,    list_default = the_list02_VarSelection_R_default)
      fn_app_reset_stone(rv = the_list03_SpecialSettigns_stone, list_default = the_list03_SpecialSettigns_R_default)
      fn_app_reset_stone(rv = the_list04_Play_stone,            list_default = the_list04_Play_default)


    })

    all_data_reactive <- reactive({
      list(
        dataset = reactiveValuesToList(the_list01_Dataset_stone),
        var_selection = reactiveValuesToList(the_list02_VarSelection_stone),
        special_settings = reactiveValuesToList(the_list03_SpecialSettigns_stone),
        play = reactiveValuesToList(the_list04_Play_stone)
      )
    })

    return(
      all_data_reactive
    )





    ###-------------------------------------------------------------------------



  })

}
