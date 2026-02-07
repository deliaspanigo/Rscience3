
# Loading modules for R Datasets and xlsx

mod_import_hub_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Bloque de Estilos: Controla la visibilidad de la barra de navegación
    tags$head(
      tags$style(HTML(paste0("
        /* Oculta la barra de pestañas completa cuando el wrapper tiene la clase hide-tabs */
        #", ns("wrapper"), ".hide-tabs ul.nav-tabs {
          display: none !important;
        }

        /* Elimina el borde superior que bslib suele dejar incluso sin pestañas */
        #", ns("wrapper"), ".hide-tabs .navset-tab {
          border-top: none !important;
        }

        /* Opcional: Ajuste estético para que el contenido empiece más arriba si no hay pestañas */
        #", ns("wrapper"), ".hide-tabs .tab-content {
          padding-top: 0px !important;
        }
      ")))
    ),

    # CONTENEDOR PRINCIPAL: Aquí es donde shinyjs añade/quita la clase 'hide-tabs'
    div(
      id = ns("wrapper"),
      class = "hide-tabs", # Estado inicial: oculto

      bslib::navset_tab(
        id = ns("import_workflow_steps"),
        selected = "step03_action", # Pestaña por defecto

        # --- PASOS DE DEPURACIÓN (01-02) ---
        bslib::nav_panel(
          title = "01. Check External",
          value = "step01_check_external",
          shiny::uiOutput(ns("SO_step01_check_external"))
        ),

        bslib::nav_panel(
          title = "02. Check Pre",
          value = "step02_check_pre",
          shiny::uiOutput(ns("SO_step02_check_pre"))
        ),

        # --- PESTAÑA PRINCIPAL DE USUARIO (03) ---
        bslib::nav_panel(
          title = "03. Action: import",
          value = "step03_action",
          br(),
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              card_header("Import Settings"),
              div(id = ns("import_controls_wrapper"),
                  selectizeInput(ns("import_method"), "Source dataset:",
                                 choices = "",
                                 options = list(
                                   placeholder = 'Select a source...',
                                   onInitialize = I('function() { this.setValue(""); }'),
                                   dropdownParent = "body"
                                 )),
                  shiny::uiOutput(ns("render_ui_step03_action_submodule"))
              ),
              hr(),
              bslib::layout_column_wrap(
                width = 1/3,
                actionButton(ns("btn_accept"), "Accept", icon = icon("check"), class = "btn-success"),
                actionButton(ns("btn_edit"), "Edit", icon = icon("pen"), class = "btn-warning"),
                actionButton(ns("btn_reset"), "Reset", icon = icon("trash"), class = "btn-danger")
              )
            ),
            bslib::card(
              card_header("Standardized Data Preview"),
              tableOutput(ns("preview_table")),
              full_screen = TRUE,
              card_footer(textOutput(ns("data_info")))
            )
          )
        ),

        # --- PASOS DE DEPURACIÓN Y MONITOREO (04-08) ---
        bslib::nav_panel(
          title = "04. Live: import",
          value = "step04_live",
          uiOutput(ns("render_ui_step04_ALL"))
        ),

        bslib::nav_panel(
          title = "05. View Post",
          value = "step05_view_post",
          br(),
          "Visual for accepted content.",
          bslib::card(
            card_header("Workflow Metadata"),
            verbatimTextOutput(ns("render_ui_step05_view_post_A"))
          ),
          bslib::card(
            card_header("Verbatim Structure"),
            verbatimTextOutput(ns("render_ui_step05_view_post_B"))
          )
        ),

        bslib::nav_panel(
          title = "06. Control Post",
          value = "step06_check_post",
          uiOutput(ns("SO_step06_check_post"))
        ),

        bslib::nav_panel(
          title = "07. Check General",
          value = "step07_check_general",
          uiOutput(ns("SO_step07_check_general"))
        ),

        bslib::nav_panel(
          title = "08. Raw Output",
          value = "step08_raw_output",
          bslib::card(
            card_header("Pure Bundle Export (Verbatim)"),
            verbatimTextOutput(ns("render_ui_step08_RAW"))
          )
        )
      ) # Fin navset_tab
    ) # Fin div wrapper
  ) # Fin tagList
}

mod_import_hub_server <- function(id, check_external, debug_toggle = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(debug_toggle(), {
      if (isTRUE(debug_toggle())) {
        # Si activas Engineer Mode, QUITAMOS la clase que oculta
        shinyjs::removeClass(id = "wrapper", class = "hide-tabs")
      } else {
        # Si desactivas, PONEMOS la clase que oculta
        shinyjs::addClass(id = "wrapper", class = "hide-tabs")
        # Volvemos a la pestaña segura
        bslib::nav_select("import_workflow_steps", "step03_action")
      }
    }, ignoreInit = FALSE)

    # Active specific
    bslib::nav_select("import_workflow_steps", "step03_action")




    # --- 01. Logic Check External --- -----------------------------------------
    RO_step01_check_external <- reactive({
      internal_check_external <- check_external()
      req(internal_check_external)

      the_time <- Sys.time()
      status <- list(
        ready = !is.null(internal_check_external),
        title = "Check External",
        description = "Default. Nothing to check really. This is the first module.",
        message = paste0("Check External: ", as.character(internal_check_external)),
        time = the_time,
        time_format = format(the_time, "%H:%M:%S")
      )
      return(status)
    })

    output$SO_step01_check_external <- renderUI({
      res <- RO_step01_check_external()
      color <- if(res$ready) "success" else "danger"
      icon_name <- if(res$ready) "check-circle" else "exclamation-triangle"

      tagList(
        res$description,
        bslib::value_box(
          title = res$title, value = res$message,
          showcase = icon(icon_name), theme = color,
          p(paste("Last check:", res$time_format))
        )
      )
    })

    # --- 02. Logic Check PRE --- ----------------------------------------------
    RO_step02_check_pre <- reactive({
      internal_check_pre <- TRUE
      req(internal_check_pre)

      the_time <- Sys.time()
      status <- list(
        ready = !is.null(internal_check_pre),
        title = "Check Pre",
        description = "Default. Nothing to check really. There is nothing to check previous load a dataset.",
        message = paste0("Check Pre: ", as.character(internal_check_pre)),
        time = the_time,
        time_format = format(the_time, "%H:%M:%S")
      )
      return(status)
    })

    output$SO_step02_check_pre <- renderUI({
      res <- RO_step02_check_pre()
      color <- if(res$ready) "success" else "danger"
      icon_name <- if(res$ready) "check-circle" else "exclamation-triangle"

      tagList(
        res$description,
        bslib::value_box(
          title = res$title, value = res$message,
          showcase = icon(icon_name), theme = color,
          p(paste("Last check:", res$time_format))
        )

      )
    })

    # --- GESTIÓN DE SUB-MÓDULOS (DINÁMICA) --- --------------------------------
    current_import_bundle <- reactiveVal(NULL)
    initialized_servers   <- new.env() # Entorno para cachear servidores

    # 1. UNICO PUNTO DE INICIALIZACIÓN (Solución al error)


    # --- 03. Logic Action
    observe({
      vector_choices <- stats::setNames(names(LIST_mod_import_options),
                      sapply(LIST_mod_import_options, `[[`, "label"))
      updateSelectizeInput(session, "import_method", choices = c("Select category..." = "", vector_choices))
      current_import_bundle(NULL)
      shinyjs::enable("import_controls_wrapper")
      shinyjs::enable("btn_accept")
      shinyjs::disable("btn_edit")
    })

    observeEvent(input$import_method, {



      req(input$import_method)
      method <- input$import_method
      id_inst <- paste0(method, "_inst")

      # Si el servidor para este método NO existe, lo creamos UNA SOLA VEZ
      if (!exists(method, envir = initialized_servers)) {
        initialized_servers[[method]] <- LIST_mod_import_options[[method]]$server(id_inst)
      }

      # Reset del bundle al cambiar de método

    })
    the_module_output <- reactive({
      req(input$import_method)
      # Buscamos la función reactiva ya inicializada en el entorno
      req(exists(input$import_method, envir = initialized_servers))
      initialized_servers[[input$import_method]]()
    })

    output$render_ui_step03_action_submodule <- renderUI({
      req(input$import_method)
      id_inst <- paste0(input$import_method, "_inst")
      LIST_mod_import_options[[input$import_method]]$ui_step03_action(ns(id_inst))
    })


    # --- 04. Logic Live


    observeEvent(input$btn_accept, {
      bundle_raw <- the_module_output()
      if (is.null(bundle_raw)) {
        showNotification("Complete the module steps first.", type = "warning")
        return(NULL)
      }

      bundle <- bundle_raw
      bundle$import_method_external <- LIST_mod_import_options[[input$import_method]]$label
      df_temp <- janitor::clean_names(as.data.frame(bundle$my_dataset))

      bundle$metrics <- list(
        row_count = nrow(df_temp), col_count = ncol(df_temp),
        empty_cells = sum(is.na(df_temp))
      )


      bundle$my_dataset <- df_temp
      bundle$ready <- TRUE

      current_import_bundle(bundle)



      shinyjs::disable("import_controls_wrapper")
      if (input$import_method == "excel_file") {
        #
        # shinyjs::disable(ns("excel_file_inst-file_upload"))
        # shinyjs::disable(ns("excel_file_inst-file_upload"))

      }
      shinyjs::disable("btn_accept")
      shinyjs::enable("btn_edit")
    })

    observeEvent(input$btn_edit, {
      current_import_bundle(NULL)
      shinyjs::enable("import_controls_wrapper")
      shinyjs::enable("btn_accept")
      shinyjs::disable("btn_edit")
    })

    observeEvent(input$btn_reset, {
      current_import_bundle(NULL)
      vector_choices <- stats::setNames(names(LIST_mod_import_options),
                                        sapply(LIST_mod_import_options, `[[`, "label"))
      updateSelectizeInput(session, "import_method", choices = c("Select category..." = "", vector_choices))
      # current_import_bundle(NULL)
      shinyjs::enable("import_controls_wrapper")
      shinyjs::enable("btn_accept")
      shinyjs::disable("btn_edit")

      # shinyjs::refresh()
    })

    # --- OUTPUTS DE VISTA & DEBUG ---
    output$preview_table <- # --- Vista Previa de la Tabla ---
      output$preview_table <- renderTable({
        bundle <- current_import_bundle()

        if (is.null(bundle)) {
          # Retornamos un DF simple con el mensaje de espera
          return(data.frame(
            Status = "Pending Confirmation",
            Message = "Please select a dataset and click 'Accept' to see the preview."
          ))
        }

        # Si hay bundle, mostramos las primeras 10 filas
        head(bundle$my_dataset, 10)
      }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # --- Texto informativo del pie de la tarjeta ---
    output$data_info <- renderText({
      bundle <- current_import_bundle()

      if (is.null(bundle)) {
        return("System Status: Waiting for user action in Step 03...")
      }

      paste0("Source: ", bundle$dataset_name_long, " | Method: ", bundle$import_method_external)
    })
    output$data_info     <- renderText({ req(current_import_bundle()); paste0("Source: ", current_import_bundle()$dataset_name_long) })


    output$render_ui_step04_live_A <- renderPrint({
      # Creamos una lista rápida para ver el valor y el tipo
      monitor <- list(
        selected_method = input$import_method,
        internal_id     = paste0(input$import_method, "_inst"),
        status          = if(is.null(current_import_bundle())) "UNCOMMITTED" else "ACCEPTED",
        timestamp = Sys.time()
      )

      cat("--- HUB SELECTOR MONITOR ---\n")
      utils::str(monitor)
    })

    output$render_ui_step04_live_submodule <- renderUI({
      req(input$import_method)
      id_inst <- paste0(input$import_method, "_inst")
      LIST_mod_import_options[[input$import_method]]$ui_step04_live(ns(id_inst))
    })

    output$render_ui_step04_live_B <- renderPrint({
      # Capturamos los valores de los botones
      # Si no se han presionado, valen 0
      button_states <- list(
        accept = input$btn_accept,
        edit   = input$btn_edit,
        reset  = input$btn_reset
      )

      monitor <- list(
        selected_method = input$import_method,
        buttons_counters = button_states,
        last_event = if(is.null(current_import_bundle())) "IDLE / EDITING" else "DATA_ACCEPTED",
        timestamp = Sys.time()
      )

      cat("--- HUB INTERFACE MONITOR ---\n")
      utils::str(monitor)
    })

    output$render_ui_step04_ALL <- renderUI({


      tagList(
        "Visualización del contenido en tiempo real. No importa si fue aceptado o no.",
        span("Current Hub State:", style = "font-weight: bold; color: #2c3e50;"),
        verbatimTextOutput(ns("render_ui_step04_live_A")),
        shiny::uiOutput(ns("render_ui_step04_live_submodule")),
        verbatimTextOutput(ns("render_ui_step04_live_B"))
      )
    })
    # 3. ACCESO AL OUTPUT DEL SUB-MÓDULO


    # --- 05. Logic View

    output$render_ui_step05_view_post_A <- renderPrint({
      # 1. Chequeo de estado sin detener la ejecución
      bundle <- current_import_bundle()
      is_confirmed <- !is.null(bundle)

      # 2. Encabezado de Workflow (Siempre visible)
      conf_status <- if(is_confirmed) "CONFIRMED [Button Accept Clicked]" else "PENDING [Waiting for Accept]"

      cat("--- WORKFLOW STATUS ---\n")
      cat("Confirmation Status :", conf_status, "\n")
      cat("Timestamp          :", format(Sys.time(), "%H:%M:%S"), "\n")

      # 3. Lógica condicional para el contenido
      if (!is_confirmed) {
        cat("\n--- NOTICE ---\n")
        cat("Waiting for user to confirm the dataset selection in Step 03.\n")
        cat("Please click the 'Accept' button to generate metadata and metrics.\n")
      } else {
        # Si hay bundle, mostramos todo el detalle
        cat("\n--- DATA SOURCE ---\n")
        cat("Method Selection    :", bundle$import_method_external, "\n")
        cat("Dataset Name (Short):", bundle$dataset_name_short, "\n")
        cat("Dataset Name (Long) :", bundle$dataset_name_long, "\n")

        cat("\n--- DATASET METRICS ---\n")
        cat("Dimensions          :", bundle$metrics$row_count, "rows x", bundle$metrics$col_count, "columns\n")
        cat("Missing Values (NA) :", bundle$metrics$empty_cells, "\n")

        cat("\n--- EXTRA METADATA ---\n")
        utils::str(bundle$extra_info)

        cat("\n--- BUNDLE FULL STRUCTURE ---\n")
        utils::str(bundle, max.level = 1)
      }
    })

    output$render_ui_step05_view_post_B <- renderPrint({ req(current_import_bundle()); utils::str(current_import_bundle()$my_dataset) })


    # --- 06. Logic Check  Post --- -----------------------------------------

    RO_step06_check_post <- reactive({
      # 1. Capturamos el bundle final
      the_output <- current_import_bundle()

      # 2. Validación de Integridad (El "Check" real)
      # Verificamos que las piezas fundamentales existan
      has_data    <- !is.null(the_output$my_dataset)
      has_metrics <- !is.null(the_output$metrics)
      has_name    <- !is.null(the_output$dataset_name_short)

      # Es exitoso solo si tiene todo lo anterior
      is_integrity_ok <- has_data && has_metrics && has_name

      # 3. Construcción del mensaje
      the_time <- Sys.time()

      # Definimos un mensaje dinámico basado en el éxito
      msg <- if (is_integrity_ok) {
        paste0("Success: Dataset '", the_output$dataset_name_short, "' validated and ready.")
      } else if (!is.null(the_output)) {
        "Warning: Bundle received but some metadata is missing."
      } else {
        "Pending: No data has been confirmed yet."
      }

      status <- list(
        ready = is_integrity_ok,
        title = "Check Post: Integrity Validation",
        description = paste(
          "Validation results:",
          if(has_data) "✅ Dataset found" else "❌ No data", "|",
          if(has_metrics) "✅ Metrics generated" else "❌ No metrics"
        ),
        message = msg,
        time = the_time,
        time_format = format(the_time, "%H:%M:%S")
      )

      return(status)
    })

    # El Output UI (SO) para este paso
    # El Output UI (SO) para este paso
    output$SO_step06_check_post <- renderUI({
      res <- RO_step06_check_post()

      # 1. Definimos el color
      # Si no hay bundle aún, usamos un color neutro (secondary)
      color <- if(res$ready) {
        "success"
      } else if (is.null(current_import_bundle())) {
        "secondary"
      } else {
        "warning"
      }

      # 2. Definimos el icono (Nombres estándar de FontAwesome)
      # 'shield-halved' o 'exclamation-triangle' son muy compatibles
      icon_name <- if(res$ready) "check-circle" else "exclamation-triangle"

      # 3. Construimos el Value Box
      bslib::value_box(
        title = res$title,
        value = res$message,
        showcase = shiny::icon(icon_name),
        theme = color,
        p(paste("Validation Time:", res$time_format)),
        # Agregamos la descripción como texto adicional en el box o debajo
        p(res$description, style = "font-size: 0.8rem; opacity: 0.8;")
      )
    })


    # 07. Check general
    # --- MASTER CHECK: Consolidación Final --- --------------------------------
    # --- MASTER CHECK: Consolidación Final --- --------------------------------
    RO_step07_master_status <- reactive({
      # Consumimos los 3 estados reactivos
      s1 <- RO_step01_check_external()
      s2 <- RO_step02_check_pre()
      s3 <- RO_step06_check_post()

      # Verificación de seguridad por si los reactivos devuelven NULL
      s1_ok <- isTRUE(s1$ready)
      s2_ok <- isTRUE(s2$ready)
      s3_ok <- isTRUE(s3$ready)

      is_system_ready <- s1_ok && s2_ok && s3_ok

      list(
        ready = is_system_ready,
        count_ok = sum(c(s1_ok, s2_ok, s3_ok)),
        total = 3,
        details = list(external = s1_ok, pre = s2_ok, post = s3_ok)
      )
    })

    output$SO_step07_check_general <- renderUI({
      ms <- RO_step07_master_status()

      # 1. Definimos el estilo dinámico
      color_status <- if(ms$ready) "success" else "warning"
      pct_val      <- round((ms$count_ok / ms$total) * 100)

      # 2. Usamos shiny::icon (FontAwesome) que ya probamos que funciona
      # 'check-double' para éxito, 'tasks' para proceso
      main_icon <- if(ms$ready) shiny::icon("check-double") else shiny::icon("tasks")

      # 3. Pre-calculamos los strings de la lista de chequeo
      # Esto evita el error de "Possible missing comma" en el div()
      s1_icon <- if(ms$details$external) "✅" else "❌"
      s2_icon <- if(ms$details$pre)      "✅" else "❌"
      s3_icon <- if(ms$details$post)     "✅" else "❌"

      label_s1 <- paste("Step 1 (External):",  s1_icon)
      label_s2 <- paste("Step 2 (Pre-load):",  s2_icon)
      label_s3 <- paste("Step 6 (Post-load):", s3_icon)

      # 4. Construcción de la UI
      bslib::card(
        bslib::card_header("Import Workflow Master Status"),
        bslib::layout_column_wrap(
          width = 1/2,
          # Visual de Progreso
          bslib::value_box(
            title = "Workflow Completion",
            value = paste0(pct_val, "%"),
            showcase = main_icon,
            theme = color_status,
            p(paste(ms$count_ok, "of", ms$total, "checkpoints passed"))
          ),
          # Lista de verificación rápida
          div(
            class = "p-2",
            p(label_s1),
            p(label_s2),
            p(label_s3)
          )
        )
      )
    })


    # --- 08. Raw Output (Puro y Crudo) --- ------------------------------------
    output$render_ui_step08_RAW <- renderPrint({
      # Solo req() para que no imprima nada si está vacío
      # o puedes quitarlo si quieres ver el NULL literal
      bundle <- current_import_bundle()

      # Imprime la estructura completa sin límites de nivel
      # Incluye los datos, métricas y metadatos tal cual existen en memoria
      print(bundle)
    })

    ### RETURN
    return(current_import_bundle)
  })
}
