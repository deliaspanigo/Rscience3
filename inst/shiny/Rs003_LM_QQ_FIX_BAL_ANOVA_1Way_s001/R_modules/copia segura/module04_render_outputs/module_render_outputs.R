#' Theory Module UI
module_render_outputs_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("main_render"))
}


module_render_outputs_server <- function(id, app_state) {

  # Pasar información a la función interna
  moduleServer(id, function(input, output, session) {


    # 1. Session & Config --------------------------------------------------------
    ns <- session$ns

    library(promises)
    library(future)
    # plan(multisession)

    str_output_folder <- "zzz_zzz_USER_OUPUT_FOLDER"

    # 2. Libreries (mantener aquí) ---------------------------------------------
    library("dplyr")
    library("shinyjs")
    library("stringr")
    # library("here")

    # Set default
    set_default_quiet <- TRUE

    # 3. Hardcoded & Source Fns (mantener aquí) ---------------------------------
    my_color_blue <- "#0d6efd" # Blue - Bootstrap primary
    my_color_green <- "#198754" # Green - Bootstrap success
    my_color_orange <- "#fd7e14" # Orange - Bootstrap warning
    my_color_red  <- "#dc3545" # Red - Bootstrap danger

    # Loading my fns
    str_fn_app_inputs <- "fn_app_inputs.R" #here("fn_app_inputs.R")
    str_fn_app_general <- "fn_app_general.R" #here("fn_app_general.R")
    source(str_fn_app_inputs)
    source(str_fn_app_general)

    # Uso:
    MY_PACKAGE_NAME <- "Rscience3" #fn_app_MY_PACKAGE_NAME()
    str_central_folder_path <-getwd() #here::here()
    package_path <- fn_app_cut_path_to_package(full_path = str_central_folder_path, package_name = MY_PACKAGE_NAME)


    # ==========================================================================
    # 🔑 INICIO DE DEFINICIONES REACTIVAS CLAVE (MOVIDAS AL PRINCIPIO)
    # ==========================================================================
    app_state_render <- reactiveValues(is_running = FALSE) # DEFINICIÓN AQUÍ

    # Estado general de la ejecución (UI/Progress)
    state <- reactiveValues(
      current_step = 0,
      message = "Ready to start",
      progress = 0,
      data = NULL,
      max_step = 9
    )

    # Mochila de datos entre pasos
    list_bag_steps <- reactiveValues(
      sys_time = NULL,
      timestamp_format = NULL,
      temp_work_folder_path = NULL,
      temp_output_folder_path = NULL
    )

    # Tiempos
    the_timer <- reactiveValues(
      init_time =NULL,
      end_time = NULL,
      time_seg = NULL)

    # Trigger de avance del pipeline
    advance_step_trigger <- reactiveVal(0)

    # Indicador de cierre
    the_closing <- reactiveVal(FALSE)

    # Definición de la estructura del pipeline
    my_pipeline_steps <- reactiveValues(
      steps = list(
        "step01" = list(
          id = "step01",
          info_step = "Sys time and work output folder...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 01: Inicializando...\n")
            Sys.sleep(1) # Simula trabajo
            return(TRUE)
          }
        ),
        "step02" = list(
          id = "step02",
          info_step = "Coping files...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 02: Create modals...\n")
            Sys.sleep(2)
            return(TRUE)
          }
        ),
        "step03" = list(
          id = "step03",
          info_step = "Proccesing R script...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 03: Open Modals...\n")
            Sys.sleep(1)
            return(TRUE)
          }
        ),
        "step04" = list(
          id = "step04",
          info_step = "Preparing R code as .R file and PNG files...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 04: Open Modals...\n")
            Sys.sleep(1)
            return(TRUE)
          }
        ),
        "step05" = list(
          id = "step05",
          info_step = "Rendering PDF report...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 05: Open Modals...\n")
            Sys.sleep(1)
            return(TRUE)
          }
        ),
        "step06" = list(
          id = "step06",
          info_step = "Rendering Docx report...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 06: Open Modals...\n")
            Sys.sleep(1)
            return(TRUE)
          }
        ),
        "step07" = list(
          id = "step07",
          info_step = "Rendering xlsx report...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 07: Open Modals...\n")
            Sys.sleep(1)
            return(TRUE)
          }
        ),
        "step08" = list(
          id = "step08",
          info_step = "Rendering Revealjs Presentation...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 08: Open Modals...\n")
            Sys.sleep(1)
            return(TRUE)
          }
        ),
        "step09" = list(
          id = "step09",
          info_step = "Rendering Rscience Report HTML...",
          check = FALSE,
          status = "waiting...",
          func = function() {
            cat("Paso 09: Open Modals...\n")
            Sys.sleep(1)
            return(TRUE)
          }
        )
      )
    )
    # ==========================================================================
    # FIN DE DEFINICIONES REACTIVAS CLAVE
    # ==========================================================================


    output$"main_render" <- renderUI({

      bslib::navset_card_tab(
        full_screen = TRUE,
        # 1. Definimos el alto aquí. Puede ser "90vh" o un valor en píxeles "800px"
        height = "87vh",

        title = tags$div(
          style = "min-height: 10px; padding: 0px;",
          tags$h4("Proccessing")
        ),

        bslib::nav_panel(
          title = "user_selection",
          # 2. fillable = TRUE hace que el contenido interno intente expandirse
          bslib::card_body(
            fillable = TRUE,
            uiOutput(ns("steps_visualization"))
          )
        )
      )
    })

    ### SERVER -----------------------------------------------------------------

    is_running_reactive <- reactive({
      app_state$is_running
    })
    # Observar cambios en absolute_play$value
    observe({
      if (isTRUE(app_state$is_running)) {
        cat("Executing module logic\n")
        print(is_running_reactive())
        # Tu código aquí...
      }
    })

    # [NUEVO] Observador para actualizar el estado central basado en el trigger
    # Priority = -1 asegura que este se ejecute al final del ciclo reactivo.
    observeEvent(advance_step_trigger(), {
      req(advance_step_trigger() > 0)

      # Solo actualizar si es un avance real (previene loops innecesarios)
      if (advance_step_trigger() > state$current_step) {
        state$current_step <- advance_step_trigger()
      }
    }, priority = -1) # Prioridad baja


    # 6. Observer for main button
    observeEvent(is_running_reactive(), {
      req(is_running_reactive())
      ns <- NS(id)
      # Show initial modal
      if(check_modal) showModal(create_modal(ns))


      # El uso de shinyjs::delay ya no es necesario ni recomendado aquí.
      state$current_step <- 0 # Resetear
      state$message <- "Starting processing..."
      state$progress <- 0
      # })

      # Activate step 1 de forma asíncrona
      # Usamos advance_step_trigger para forzar el inicio del pipeline
      # y evitar que este observeEvent fije directamente state$current_step.
      advance_step_trigger(1) # Inicia la cadena reactiva


    })


    #### SERVER
    # steps_list_non_reactive <- my_pipeline_steps$steps # Esto devuelve la lista estática
    # max_steps_count <- length(steps_list_non_reactive)
    # 1. Define reactiveValues to control state


    # 2. Observe current state to display in UI
    output$current_status <- renderText({
      paste(
        "Step:", state$current_step, "\n",
        "Message:", state$message, "\n",
        "Progress:", state$progress * 100, "%"
      )
    })

    output$results <- renderText({
      if (is.null(state$data)) {
        "No data yet"
      } else {
        paste("Processed data:", length(state$data), "elements")
      }
    })

    # 3. Modal that uses reactive outputs
    check_modal <- TRUE

    if(check_modal){
      create_modal <- function(ns) { # Ensure 'ns' is passed if needed inside the modal
        modalDialog(
          title = ,

          # Inject the custom CSS styles here
          tags$style(
            HTML("
        /* 1. Adjust Modal Position (External Margin - Top/Bottom) */
        .modal-dialog {
          /* Reduces the space between the top of the modal and the browser window */
          margin-top: 30px !important;
          margin-bottom: 30px !important;
        }

        /* 2. Adjust Modal Internal Padding (The space inside the border) */
        .modal-body {
          /* Padding (Top, Right, Bottom, Left) */
          /* Note: modal-body controls the area below the title and above the footer. */
          padding-top: 5px !important; /* Reduce Top Padding */
          padding-right: 15px !important;
          padding-bottom: 5px !important; /* Reduce Bottom Padding */
          padding-left: 15px !important;
        }

        /* 3. Adjust Title Padding (Optional, but often needed) */
        .modal-header {
          padding-top: 10px !important;
          padding-bottom: 10px !important;
        }
      ")
          ),

          # Note: We move your main content below the title/header,
          # which is automatically wrapped in .modal-body by modalDialog.
          tagList(
            fluidRow(
              column(6,
                     div(style = "display: flex; align-items: center;",
                         h4("Current Step:", style = "margin: 0; margin-right: 10px;"),
                         textOutput(ns("modal_step"), inline = TRUE)
                     )
              ),
              column(6, uiOutput(ns("modal_progress_bar")))
            ),
            br(),
            fluidRow(
              column(6,
                     div(style = "display: flex; align-items: center;",
                         h4("Status:", style = "margin: 0; margin-right: 10px;"),
                         textOutput(ns("modal_message"), inline = TRUE)
                     )
              ),
              column(6,
                     div(style = "text-align: center;",
                         icon("spinner", class = "fa-spin fa-2x"))
              )
            ),

            uiOutput(ns("steps_visualization"))
          ),

          footer = NULL,
          easyClose = FALSE,
          size = "xl"
        )
      }
    }
    # 4. Outputs for modal that update automatically
    output$modal_title <- renderText({
      check_value <- state$current_step != "0"
      text01 <- "Processing..."
      text02 <- "Preparing enviroment..."
      ifelse(test = check_value, yes = text01, no = text02)
    })

    output$modal_step <- renderText({
      check_value <- state$current_step != "0"
      text01 <- paste("Step", state$current_step, "of ", state$max_step)
      text02 <- "---"
      ifelse(test = check_value, yes = text01, no = text02)
    })

    output$modal_message <- renderText({
      check_value <- state$message != ""
      text01 <- state$message
      text02 <- "---"
      ifelse(test = check_value, yes = text01, no = text02)
    })

    # 5. REACTIVE Progress bar
    output$modal_progress_bar <- renderUI({
      div(class = "progress",
          div(class = "progress-bar",
              style = paste0("width:", state$progress * 100, "%;"),
              paste0(round(state$progress * 100), "%")))
    })

    # En tu server.R
    output$steps_visualization <- renderUI({

      force_render <- reactiveValuesToList(my_pipeline_steps)
      current_step <- state$current_step

      # Obtener steps del pipeline
      steps_list <- reactiveValuesToList(my_pipeline_steps)$steps

      # Determinar el step actual en ejecución
      current_step_num <- state$current_step

      # Crear UI para cada step
      steps_ui <- lapply(names(steps_list), function(step_id) {

        # Extraer información del step
        step_info <- steps_list[[step_id]]
        step_number <- as.numeric(gsub("step", "", step_id))

        # Determinar estado y icono
        if (step_info$check) {
          # Step completado - icono verde
          icon_name <- "check-circle"
          icon_color <- "green"
          badge_color <- "success"
          badge_text <- "Completed"

        } else if (current_step_num == step_number) {
          # Step en ejecución - icono giratorio naranja
          icon_name <- "cog"
          icon_spin <- "fa-spin"
          icon_color <- "orange"
          badge_color <- "warning"
          badge_text <- "Running..."

        } else if (current_step_num > step_number) {
          # Step ya pasó pero no completó - error rojo
          icon_name <- "exclamation-triangle"
          icon_color <- "red"
          badge_color <- "danger"
          badge_text <- "Failed"

        } else {
          # Step pendiente - icono azul
          icon_name <- "play-circle"
          icon_color <- "blue"
          badge_color <- "secondary"
          badge_text <- "Pending"
        }

        # Construir UI del step
        div(
          class = "step-item",
          style = paste(
            "display: flex;",
            "align-items: center;",
            "padding: 10px;",
            "margin: 5px 0;",
            "border-radius: 5px;",
            "background-color: #f8f9fa;",
            "border-left: 4px solid",
            if (step_info$check) "#28a745;" # Verde para completado
            else if (current_step_num == step_number) "#ffc107;" # Amarillo para ejecución
            else "#6c757d;" # Gris para pendiente
          ),

          # Número del step con badge
          div(
            style = "width: 60px; text-align: center;",
            span(
              class = paste0("badge bg-", badge_color),
              style = "font-size: 1em; padding: 6px 12px;",
              step_number
            )
          ),

          # Icono de estado
          div(
            style = paste0(
              "width: 40px;",
              "text-align: center;",
              "font-size: 1.5em;",
              "color: ", icon_color, ";"
            ),
            if (exists("icon_spin")) {
              icon(icon_name, class = icon_spin)
            } else {
              icon(icon_name)
            }
          ),

          # Información del step
          div(
            style = "flex: 1; padding: 0 15px;",

            # Título del step
            div(
              style = paste(
                "font-weight: bold;",
                "font-size: 1.1em;",
                "margin-bottom: 3px;"
              ),
              paste("Step", step_number, ":", step_info$info_step)
            ),

            # Estado y detalles
            div(
              style = paste(
                "font-size: 0.9em;",
                "color: #666;"
              ),
              paste("Status: ", badge_text, " | ", step_info$status)
            )
          ),

          # Tiempo estimado si existe
          if (!is.null(step_info$estimated_time)) {
            div(
              style = paste(
                "font-size: 0.8em;",
                "color: #888;",
                "padding: 2px 8px;",
                "background-color: #e9ecef;",
                "border-radius: 3px;"
              ),
              paste("~", step_info$estimated_time, "sec")
            )
          }
        )
      })

      # Retornar todos los steps en un contenedor
      div(
        id = "steps-container",
        # style = "max-height: 500px; overflow-y: auto; padding: 10px;",
        steps_ui
      )
    })

    # Cierre (MOVIDO AL FINAL DEL SCRIPT)
    observeEvent(state$current_step,{
      # 1. Requeriments
      req(state$current_step > state$max_step)
      # req(!my_pipeline_steps$steps[[state$current_step]]$"check")
      print("Closing - Init")

      # 2. State info
      state$message   <- "Returning to RSciecne..."
      state$progress  <- 1

      # La eliminación de shinyjs::delay en el cierre debe hacerse con future/promise si es pesada,
      # pero como solo son actualizaciones reactivas y removeModal(), se puede dejar síncrono.
      the_closing(TRUE)
      if(check_modal) removeModal()

      print("Closing - End")
      ##########################
      print("Return - Init")

      # 2. State info
      state$message   <- "Returning to RSciecne..."
      state$progress  <- 1


      # removeModal()

      print("Return - End")


      # Aseguramos la lectura segura de los reactiveValues al final del proceso
      list_values <- reactiveValuesToList(list_bag_steps)

      app_state_render$is_running <- TRUE
      app_state_render$is_done  <- TRUE
      app_state_render$sys_time <- list_values$sys_time
      app_state_render$timestamp_format <- list_values$timestamp_format
      app_state_render$str_temp_work_folder_path <- list_values$temp_work_folder_path
      app_state_render$str_temp_output_folder_path <- list_values$temp_output_folder_path


    })

    # =====================================================================
    # LLAMADA A LAS FUNCIONES DEL PIPELINE (PASOS ASÍNCRONOS)
    # =====================================================================
    # Define the path to the steps folder
    # str_steps_folder_path is assumed to be defined by 'here()' or similar,
    # hence no need to redefine 'here()' inside the source loop.
    str_steps_folder_path <- file.path("R_modules", "module04_render_outputs", "pipeline_proc_steps")

    # --- Automatic Block to Source Pipeline Steps ---
    # 1. Get the list of files matching the naming pattern.
    #    - pattern = "^fn_proc_step.*\\.R$": Ensures only .R files starting with "fn_proc_step" are included.
    #    - full.names = TRUE: Returns the complete path, necessary for source().
    list_steps_files <- list.files(
      path = str_steps_folder_path,
      pattern = "^fn_proc_step.*\\.R$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = FALSE # Only searches in the main steps folder
    )

    # 2. Sort the files to ensure that Step01, Step02, etc., are loaded in the correct order.
    #    This step is crucial for maintaining the pipeline's intended sequence.
    list_steps_files <- sort(list_steps_files)

    # 3. Use lapply to apply the source function to every file path in the list.
    #    The return value of lapply (a list of NULLs from source) is typically ignored.
    lapply(list_steps_files, source)

    # ----------------------------------------------------
    # Your subsequent code continues here...

    #### Esto se puede mejorar en el futuro

    fn_proc_step01_temp_work_folder_and_time(state, my_pipeline_steps, list_bag_steps, the_timer, advance_step_trigger)
    fn_proc_step02_copy_files(state, my_pipeline_steps, list_bag_steps, session, advance_step_trigger)
    fn_proc_step03_quarto_report01(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger)
    fn_proc_step04_quarto_report02(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger)
    fn_proc_step05_quarto_report03(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger)
    fn_proc_step06_quarto_report04(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger)
    fn_proc_step07_quarto_report05(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger)
    fn_proc_step08_quarto_report06(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger)
    fn_proc_step09_quarto_report99(state, my_pipeline_steps, list_bag_steps, set_default_quiet, advance_step_trigger, the_timer)




    # Step Closing (El observer de cierre se dejó en la sección de observadores principales)

    # return -------------------------------------------------------------
    return(app_state_render)

  }) # End module server

}
