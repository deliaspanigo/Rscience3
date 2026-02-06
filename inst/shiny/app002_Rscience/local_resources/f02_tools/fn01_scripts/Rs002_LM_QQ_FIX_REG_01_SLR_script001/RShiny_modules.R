# MÓDULO: Estadística Descriptiva Simple

module_ui_menu <- function(id) {
  ns <- NS(id)
  navset_pill_list(
    id = ns("menu_lateral"),
    well = FALSE,
    nav_panel("DEBUG conexion", value = "tab_conexion_DEBUG"),
    nav_panel("03. My Dataset", value = "tab_my_dataset"),
    nav_panel("3. Config", value = "tab_config"),
    nav_panel("3.2 Post Config DEBUG", value = "tab_config_post_DEBUG"),
    nav_panel("4. Resultados", value = "tab_results"),
    nav_panel("", value = "clean")
  )
}

module_ui_body <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_conexion_DEBUG'", ns("menu_lateral")),
      card(
        card_header("Conexion DEBUG"),
        uiOutput(ns("debug_conexion_dashboard"))
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_config'", ns("menu_lateral")),
      card(
        card_header("Configuración"),
        # Dejamos que el server lo renderice por completo
        uiOutput(ns("var_selector_ui"))
      )
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_config_post_DEBUG'", ns("menu_lateral")),
      card(
        card_header("Configuración POST DEBUG"),
        # Dejamos que el server lo renderice por completo
        uiOutput(ns("debug_status_POST_dashboard"))
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_results'", ns("menu_lateral")),
      card(
        card_header("Histograma"),
        plotOutput(ns("plot_simple"))
      ),
      card(
        card_header("Resumen"),
        verbatimTextOutput(ns("resumen_simple"))
      )
    )
  )
}



module_server <- function(id, OR_01_import_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. FUENTE DE DATOS ---
    ORI_raw_data_source <- reactive({
      req(OR_01_import_dataset())
      OR_01_import_dataset()
    })

    # Master01_import
    ## PRE Validation
    ## DEBUG PRE
    ## Action: import dataset
    ## DEBUG POST
    ## Check General
    ## output
    ## View master03: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output



    # Master02_tools
    ## PRE Validation
    ## DEBUG PRE
    ## Action: tool selection
    ## DEBUG POST
    ## Check General
    ## output
    ## View master03: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output


    # Ancestral 01 (check de que se cumple Master01 y 02 simulateos)
    ## PRE Validation
    ## Action: tool selection
    ## Check General
    ## output
    ## View ancentral01: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output

    # Master03_dataset
    ## PRE Validation
    ## DEBUG PRE
    ## Action: show dataset
    ## DEBUG POST
    ## Check General
    ## output
    ## View master03: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output


    # Master04_theory
    ## PRE Validation
    ## DEBUG PRE
    ## Action: show theory
    ## DEBUG POST
    ## Check General
    ## output
    ## View master04: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output


    # Master05_var_selection
    ## PRE Validation
    ## DEBUG PRE
    ## Action: seleccion de variables, y valor alfa.
    ## DEBUG POST
    ## Check General
    ## output
    ## View master05: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output


    # Master05_settings
    ## PRE Validation
    ## DEBUG PRE
    ## Action: colores, orden y otros detalles.
    ## DEBUG POST
    ## Check General
    ## output
    ## View master05: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output

    # Master06_control01
    ## PRE Validation
    ## DEBUG PRE
    ## Action: visualizacion de minimos, maximos y algun plot.
    ## DEBUG POST
    ## Check General
    ## output
    ## View master06: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output

    # Master07_control_special
    ## PRE Validation
    ## DEBUG PRE
    ## Action: control especifico de la herramienta.
    ## DEBUG POST
    ## Check General
    ## output
    ## View master07: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output


    # Ancestral 02 (check de que se cumple Master03 y 07 simulateos)
    ## PRE Validation
    ## Action: tool selection
    ## Check General
    ## output
    ## View ancentral02: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output


    # Master08_control_special
    ## PRE Validation
    ## DEBUG PRE
    ## Action: control especifico de la herramienta.
    ## DEBUG POST
    ## Check General
    ## output
    ## View master08: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output

    # Master09_render01
    ## PRE Validation
    ## DEBUG PRE
    ## Action: render01.
    ## DEBUG POST
    ## Check General
    ## output
    ## View master09: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output

    # Master10_render02
    ## PRE Validation
    ## DEBUG PRE
    ## Action: render02.
    ## DEBUG POST
    ## Check General
    ## output
    ## View master10: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output

    # Ancestral 03 (check de que se cumple lso master de los render)
    ## PRE Validation
    ## Action: tool selection
    ## Check General
    ## output
    ## View ancentral03: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output


    # Stone_download
    ## PRE Validation
    ## DEBUG PRE
    ## Action: permitir la descarga o ejecucion de todos los render.
    ## DEBUG POST
    ## Check General
    ## output
    ## View Stone: Check-General - Prevalidation - DebugPre - Action - Debug Post - Output

    # SuperView
    # Tal vez la visualizacion de todos los elementso anteriores de alguna manera


    # --- 2. VALIDADOR (Corrección del error length-one character) ---
    ORI_DEBUG_validator_conexion <- reactive({
      res <- ORI_raw_data_source()
      ds <- res$dataset$my_dataset

      checks_list <- list(
        list(id = "Process Status", criteria = "is_done == TRUE",  passed = !is.null(res$is_done) && res$is_done),
        list(id = "Existence",      criteria = "Object exists",    passed = !is.null(ds)),
        list(id = "Object Type",    criteria = "Is data.frame",    passed = is.data.frame(ds)),
        list(id = "Row Integrity",  criteria = "Rows > 0",         passed = if(is.data.frame(ds)) nrow(ds) > 0 else FALSE),
        list(id = "Col Integrity",  criteria = "Cols > 0",         passed = if(is.data.frame(ds)) ncol(ds) > 0 else FALSE),
        list(id = "Memory Check",   criteria = "Valid attributes", passed = !is.null(attributes(ds)))
      )

      quality_report_df <- do.call(rbind, lapply(checks_list, as.data.frame))
      failed_ids <- quality_report_df$id[quality_report_df$passed == FALSE]

      # CORRECCIÓN AQUÍ: Usar NULL en lugar de list()
      error_logs <- if(length(failed_ids) > 0) paste("Critical failure at:", paste(failed_ids, collapse = ", ")) else NULL
      is_system_valid <- all(quality_report_df$passed)

      list(
        valid = is_system_valid,
        report = quality_report_df,
        messages = error_logs,
        timestamp = Sys.time(),
        stats = list(
          row_count = if(is_system_valid) nrow(ds) else 0,
          col_count = if(is_system_valid) ncol(ds) else 0,
          mem_usage = if(is_system_valid) format(object.size(ds), units = "auto") else "0 Mb"
        )
      )
    })
    ui_debug_conexion_view <- function(v) {
      # Definimos el tema basado en la validez total
      status_theme <- if (v$valid) "#198754" else "#d63384"
      status_bg    <- if (v$valid) "#d1e7dd" else "#f8d7da"
      status_text  <- if (v$valid) "#0a3622" else "#842029"

      tagList(
        # --- 1. BANNER DE ESTADO INICIAL ---
        div(style = paste0("padding: 15px; border-radius: 8px; margin-bottom: 15px; text-align: center; border: 2px solid ", status_theme, "; ",
                           "background: ", status_bg, "; color: ", status_text, ";"),
            tags$h4(style = "margin: 0; display: flex; align-items: center; justify-content: center; gap: 10px;",
                    if(v$valid) shiny::icon("check-circle") else shiny::icon("times-circle"),
                    if(v$valid) "DATOS VALIDADOS" else "ERROR EN ORIGEN DE DATOS"
            ),
            div(style = "margin-top: 5px; font-size: 0.9rem; opacity: 0.8;",
                if(v$valid) "El dataset se ha cargado correctamente y cumple los requisitos mínimos."
                else "El dataset no puede ser procesado. Verifique el archivo de origen.")
        ),

        # --- 2. TARJETA DE DETALLES ---
        div(class = "card mb-3 border-0 shadow-sm",
            # Header con Título y TIMESTAMP
            div(style = paste0("background:", status_theme, "; color:white; padding:12px; border-radius: 8px 8px 0 0;
                           display:flex; justify-content:space-between; align-items:center;"),
                span(tags$b(shiny::icon("database"), " ESTADO DE DATOS")),
                span(style="font-size: 0.8rem; opacity: 0.9; font-family: monospace;",
                     shiny::icon("clock"), format(v$timestamp, "%H:%M:%S"))
            ),

            div(class = "card-body border",
                # Estadísticas rápidas
                div(style = "display: flex; justify-content: space-around; background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                    span(tags$b("Filas: "), tags$code(v$stats$row_count)),
                    span(tags$b("Columnas: "), tags$code(v$stats$col_count)),
                    span(tags$b("Memoria: "), tags$code(v$stats$mem_usage))
                ),

                # Tabla de Reporte con iconos y colores
                renderTable({
                  res <- v$report
                  res$passed <- ifelse(res$passed,
                                       "<span style='color: #198754; font-weight: bold;'>✔ PASSED</span>",
                                       "<span style='color: #dc3545; font-weight: bold;'>✘ FAILED</span>")
                  res
                },
                striped = TRUE,
                width = "100%",
                align = 'c',
                sanitize.text.function = function(x) x)
            )
        ),

        # --- 3. MENSAJES DE ERROR ADICIONALES ---
        if(!is.null(v$messages)) {
          div(class="alert alert-danger border-0 shadow-sm",
              style="border-left: 5px solid #842029;",
              tags$b(shiny::icon("exclamation-triangle"), " Detalles del error: "),
              br(),
              v$messages)
        }
      )
    }

    output$debug_conexion_dashboard <- renderUI({
      # 1. Obtener la lógica (Data)
      v <- ORI_DEBUG_validator_conexion()

      # 2. Generar la interfaz (UI) usando la función independiente
      ui_debug_conexion_view(v)
    })

    # --- 3. DATAFRAME FILTRADO ---
    my_df <- reactive({
      v <- ORI_validator_conexion()
      req(v$valid)
      ORI_raw_data_source()$dataset$my_dataset
    })

    my_vector_col_opt <- reactive({

      data <- my_df()
      cols <- names(data)
      vector_pos <- 1:length(cols)
      names(cols) <- paste0("Var ", vector_pos, " ", cols)
      cols
    })
    # --- 4. CONTROL SELECTOR (UI) ---
    output$var_selector_ui <- renderUI({

      cols <- my_vector_col_opt()

      req(cols)

      current_selection <- isolate(input$selected_var)
      selected_value <- if (!is.null(current_selection) && current_selection %in% cols) current_selection else ""

      selectizeInput(
        ns("selected_var"),
        label = "Seleccione una columna:",
        choices = c("Seleccione..." = "", cols),
        selected = selected_value,
        multiple = FALSE,
        options = list(placeholder = 'Buscando variable...', dropdownParent = 'body')
      )
    })

    # 02. Validator POST SETTINGS
    # --- 02.A Validador: Selección de Variable ---
    the_validator_POST_SETTINGS_01_var_selection <- reactive({
      df <- my_df()
      cols_opt <- my_vector_col_opt() # Aquí están tus labels
      selected <- input$selected_var

      safe_selection <- if(is.null(selected) || length(selected) == 0) "" else selected

      # --- Lógica para recuperar el Label ---
      # Buscamos en el vector de opciones qué nombre (Label) corresponde al valor seleccionado
      user_label <- if(safe_selection != "" && safe_selection %in% cols_opt) {
        names(cols_opt)[cols_opt == safe_selection]
      } else {
        "No selection"
      }

      check_selection <- safe_selection != ""
      check_existence <- if(check_selection) safe_selection %in% names(df) else FALSE
      check_numeric   <- if(check_existence) is.numeric(df[[safe_selection]]) else FALSE

      quality_report_df <- data.frame(
        id = c("Selection", "Existence", "Data Type"),
        criteria = c("Not Empty", "In Dataframe", "Is Numeric (Plot)"),
        passed = c(check_selection, check_existence, check_numeric),
        stringsAsFactors = FALSE
      )

      list(
        id = "Variable Selection",
        valid = check_selection && check_existence,
        valid_plot = check_selection && check_existence && check_numeric,
        report = quality_report_df,
        selected_var = safe_selection, # Nombre técnico de la columna
        user_label = user_label        # Nombre amigable (Var 1...)
      )
    })

    # --- 02.B (Espacio para más validadores: _02_filtros, _03_parametros, etc.) ---

    # --- 02.Z El Centralizador (MASTER VALIDATOR) ---
    the_validator_POST_SETTINGS_99 <- reactive({
      v1 <- the_validator_POST_SETTINGS_01_var_selection()

      all_validators <- list(v1)

      # Determinamos el estado maestro
      # TRUE si todos son válidos para su función principal (plot)
      master_ready <- all(sapply(all_validators, function(x) x$valid_plot))

      list(
        valid = master_ready,
        all_reports = all_validators,
        timestamp = Sys.time()
      )
    })
    # Visualizador para la Selección de Variable
    ui_debug_var_selection <- function(v1, df_current) {
      tagList(
        br(),

        # --- 1. BANNER DE ESTADO MAESTRO DEL CONTROLADOR ---
        div(style = paste0("padding: 15px; border-radius: 8px; margin-bottom: 20px; text-align: center; border: 2px solid; ",
                           if(v1$valid_plot) "background: #d1e7dd; color: #0a3622; border-color: #a3cfbb;"
                           else "background: #f8d7da; color: #842029; border-color: #f5c2c7;"),
            tags$h4(style = "margin: 0; display: flex; align-items: center; justify-content: center; gap: 10px;",
                    if(v1$valid_plot) shiny::icon("check-circle") else shiny::icon("times-circle"),
                    if(v1$valid_plot) "CONTROLADOR SEGURO" else "CONTROLADOR BLOQUEADO"
            ),
            tags$small(if(v1$valid_plot) "La variable es apta para análisis y visualización."
                       else "Existen problemas de validación que impiden el uso de esta variable.")
        ),

        # --- 2. FICHAS DE IDENTIDAD (TUS CARDS ACTUALES) ---
        div(style = "display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 20px;",
            div(style = "flex: 1; min-width: 150px; background: #f0f7ff; border: 1px solid #cfe2ff; padding: 10px; border-radius: 8px;",
                div(style = "font-size: 0.65rem; color: #0d6efd; font-weight: bold; text-transform: uppercase;", "User Label"),
                div(style = "font-size: 0.9rem; font-weight: bold; color: #084298;", v1$user_label)
            ),
            div(style = "flex: 1; min-width: 150px; background: #f8f9fa; border: 1px solid #e9ecef; padding: 10px; border-radius: 8px;",
                div(style = "font-size: 0.65rem; color: #6c757d; font-weight: bold; text-transform: uppercase;", "Technical Name"),
                div(style = "font-size: 0.9rem; font-family: monospace; font-weight: bold;",
                    if(v1$selected_var == "") "---" else v1$selected_var)
            ),
            div(style = "flex: 1; min-width: 120px; background: #fff3cd; border: 1px solid #ffe69c; padding: 10px; border-radius: 8px;",
                div(style = "font-size: 0.65rem; color: #856404; font-weight: bold; text-transform: uppercase;", "Data Type"),
                div(style = "font-size: 0.9rem; font-weight: bold; color: #856404;",
                    if(v1$selected_var != "" && v1$selected_var %in% names(df_current))
                      class(df_current[[v1$selected_var]])[1] else "N/A")
            )
        ),

        # --- 3. TABLA CON ICONOS COLOREADOS ---
        renderTable({
          res <- v1$report
          # Inyectamos HTML para los colores
          res$passed <- ifelse(res$passed,
                               "<span style='color: #198754; font-weight: bold;'>✔ PASSED</span>",
                               "<span style='color: #dc3545; font-weight: bold;'>✘ FAILED</span>")

          names(res) <- c("Metric", "Requirement", "Status")
          res
        },
        striped = TRUE,
        bordered = TRUE,
        width = "100%",
        align = 'c',
        sanitize.text.function = function(x) x # IMPORTANTE: permite leer el HTML de los colores
        )
      )
    }

    # Visualizador para la Visión General (Contenido de la primera pestaña)
    ui_debug_general_view <- function(v_master) {
      tagList(
        br(),
        div(style = "display: flex; flex-direction: column; gap: 12px;",
            lapply(v_master$all_reports, function(report) {

              # Lógica de colores estricta:
              # Verde si pasa el check de PLOT (el más exigente)
              # Rojo para cualquier otra situación (Empty, No existe, o No numérico)
              is_perfect <- report$valid_plot

              row_bg    <- if(is_perfect) "#d1e7dd" else "#f8d7da" # Verde claro vs Rojo claro
              row_border <- if(is_perfect) "#a3cfbb" else "#f5c2c7" # Bordes
              text_color <- if(is_perfect) "#0a3622" else "#842029" # Texto oscuro

              div(style = paste0("background:", row_bg, ";
                              border: 1px solid ", row_border, ";
                              color: ", text_color, ";
                              padding: 12px 15px;
                              border-radius: 8px;
                              display: flex;
                              justify-content: space-between;
                              align-items: center;"),

                  # Lado Izquierdo: Nombre e ID
                  span(
                    tags$b(style = "font-size: 1rem;", report$id),
                    tags$i(style = "font-size: 0.8rem; margin-left: 10px; opacity: 0.7;",
                           paste0("(", report$selected_var, ")"))
                  ),

                  # Lado Derecho: Badge de Estado
                  span(
                    if(is_perfect) {
                      tags$span(class="badge bg-success", style="padding: 6px 10px;",
                                shiny::icon("check-circle"), " READY")
                    } else {
                      tags$span(class="badge bg-danger", style="padding: 6px 10px;",
                                shiny::icon("times-circle"), " FAILED")
                    }
                  )
              )
            })
        ),

        # Cartel de aviso dinámico (Solo aparece si algo falló)
        if(!v_master$valid) {
          div(class = "alert alert-warning mt-3 border-0 shadow-sm",
              style = "border-left: 5px solid #ffc107; background: #fff3cd;",
              shiny::icon("triangle-exclamation"),
              tags$b(" Atención:"), " Hay controladores que no cumplen los requisitos técnicos para el gráfico.
          Revise la pestaña de detalle.")
        }
      )
    }

    output$debug_status_POST_dashboard <- renderUI({
      # 1. Recuperar datos y validadores
      v_master <- the_validator_POST_SETTINGS_99()
      v1       <- the_validator_POST_SETTINGS_01_var_selection()
      df       <- my_df()

      # 2. Color dinámico
      status_theme <- if (v_master$valid) "#198754" else "#d63384"

      # 3. Construcción del Tabset
      tagList(
        div(class = "card border-0 shadow-sm mb-4",
            # Header Maestro
            div(style = paste0("background:", status_theme, "; color:white; padding:12px; border-radius: 8px 8px 0 0;"),
                div(style = "display:flex; justify-content:space-between; align-items:center;",
                    span(tags$b(shiny::icon("microchip"), " MASTER CONFIGURATION AUDIT")),
                    span(style="font-size:0.8rem; opacity:0.8;", format(v_master$timestamp, "%H:%M:%S"))
                )
            ),

            # Cuerpo con navegación
            div(class = "card-body bg-white",
                tabsetPanel(
                  id = ns("debug_tabs"),

                  # PESTAÑA GENERAL (Usa el visualizador general)
                  tabPanel(title = "General View", icon = icon("eye"),
                           ui_debug_general_view(v_master)
                  ),

                  # PESTAÑA DETALLE VAR (Usa el visualizador de variable)
                  tabPanel(title = "Var Selection", icon = icon("sliders"),
                           ui_debug_var_selection(v1, df)
                  )

                  # Aquí podrías añadir:
                  # tabPanel("Filtros", ui_debug_filtros(v2))
                )
            )
        )
      )
    })


    # --- 5. LÓGICA DE VALIDACIÓN PARA GRÁFICO ---

    ready_col_data <- reactive({
      req(input$selected_var)
      df <- my_df()
      req(input$selected_var %in% names(df))

      col_data <- df[[input$selected_var]]

      if (!is.numeric(col_data)) {
        showNotification("Variable no numérica.", type = "warning", id = ns("warn_num"))
        return(NULL)
      }
      col_data
    })

    # --- 6. OUTPUTS: GRÁFICO Y RESUMEN ---
    output$plot_simple <- renderPlot({
      val <- req(ready_col_data())
      hist(val, col = "steelblue", border = "white",
           main = paste("Histograma:", input$selected_var))
    })

    output$resumen_simple <- renderPrint({
      req(input$selected_var)
      df <- my_df()
      req(input$selected_var %in% names(df))
      summary(df[[input$selected_var]])
    })


  })
}
