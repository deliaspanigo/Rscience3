# --- Inside app_software_2026.R ---

# Define the global.R path relative to the installed package
path_global <- system.file("shiny/app003_software/global.R", package = "Rscience3")

if (path_global != "") {
  message("--> Loading configuration from global.R...")
  source(path_global)
} else {
  # Error handling if global.R is critical
  stop("CRITICAL ERROR: 'global.R' missing in 'app003_software' folder.", call. = FALSE)
}

SHOW_DEBUG <- FALSE
addResourcePath("mis_estilos", system.file("shiny/app003_software/www", package = "Rscience3"))

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience - Centralized Preview",

  # --- TECHNICAL BLOCK: CSS TO HIDE CONTROL TABS ---
  # Vincular el archivo externo
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mis_estilos/custom_styles.css")
    ),

  sidebar = sidebar(
    # MENU 1: Master (Orchestrator)
    navset_pill_list(
      id = "menu_fixed",
      well = FALSE,
      nav_panel("1. Dataset", value = "tab_import"),
      if(SHOW_DEBUG) nav_panel("1.1. Debug Dataset", value = "tab_import_DEBUG"),
      nav_panel("2. Tools & Scripts", value = "tab_tools"),
      if(SHOW_DEBUG) nav_panel("2.1. Tools Debug", value = "tab_tools_DEBUG"),
      if(SHOW_DEBUG) nav_panel("3.1. Is Done All", value = "tab_is_done_all_DEBUG"),

      if(SHOW_DEBUG) nav_panel("4.1. Temporal FF Debug", value = "tab_temporal_FF_DEBUG"),
      if(SHOW_DEBUG) nav_panel("5.1. Loading FF Debug", value = "tab_loading_FF_DEBUG"),
      if(SHOW_DEBUG) nav_panel("6.1. General Debug", value = "tab_99_DEBUG"),

      # Logical tab: CSS makes it invisible
      nav_panel("HIDDEN", value = "tab_execute_tool")
    ),

    hr(),

    # MENU 2: Dynamic (Loaded from the module)
    uiOutput("render_tool_menu")
  ),

  # --- UNIFIED MAIN AREA ---

  # Group A: Master Configuration and Debug Panels
  # Only visible if the focus is NOT on the executing tool
  conditionalPanel(
    condition = "input.menu_fixed != 'tab_execute_tool'",

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_import'",
      module_orchestrator_01_import_dataset_ui("master_import")
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_import_DEBUG'",
      bslib::navset_card_tab(
        title = "Step 01 - Import Diagnostics",

        # Pestaña 1: La Ficha Técnica Estética que diseñamos
        bslib::nav_panel(
          title = "Visual Summary",
          shiny::icon("table"),
          shiny::uiOutput("debug_status_01_dashboard")
        ),

        # Pestaña 2: El Verbatim Crudo (str) para inspección profunda
        bslib::nav_panel(
          title = "Raw Structure",
          shiny::icon("code"),
          shiny::verbatimTextOutput("debug_verbatim_01_import_DEBUG")
        )
      )
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_tools'",
      module_tool_selector_ui("master_tools")
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_tools_DEBUG'",
      bslib::navset_card_tab(
        title = "Step 02 - Tools Diagnostics",

        # Pestaña 1: La Ficha Técnica Estética del Paso 02
        bslib::nav_panel(
          title = "Visual Summary",
          shiny::icon("wrench"),
          shiny::uiOutput("debug_status_02_dashboard")
        ),

        # Pestaña 2: El Verbatim Crudo para inspección de toda la lista YML
        bslib::nav_panel(
          title = "Raw Structure",
          shiny::icon("code"),
          shiny::verbatimTextOutput("debug_verbatim_02_tools_DEBUG")
        )
      )
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_is_done_all_DEBUG'",
      bslib::navset_card_tab(
        title = "Step 03 - Is done all - Debug",
        # Pestaña Visual (Badges)
        bslib::nav_panel(
          title = "Visual Status",
          shiny::htmlOutput("debug_status_03_dashboard")
        ),
        # Pestaña Técnica (Verbatim)
        bslib::nav_panel(
          title = "Technical Raw",
          shiny::verbatimTextOutput("debug_verbatim_03_is_done_all_DEBUG")
        )
      )
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_temporal_FF_DEBUG'",
      bslib::navset_card_tab(
        title = "Step 04 - Temporal FF - Debug",

        # Pestaña 1: Mapeo Visual de Archivos
        bslib::nav_panel(
          title = "File Mapping",
          shiny::htmlOutput("debug_status_04_dashboard")
        ),

        # Pestaña 2: Estructura de la Lista (Raw)
        bslib::nav_panel(
          title = "Technical Raw",
          shiny::verbatimTextOutput("debug_verbatim_04_temporal_FF_DEBUG")
        )
      )
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_loading_FF_DEBUG'",
      bslib::navset_card_tab(
        title = "Step 05 - Module Dispatcher - Debug",

        # Pestaña 1: Verificación de Funciones (La parte visual que creamos)
        bslib::nav_panel(
          title = "Function Check",
          shiny::htmlOutput("debug_status_05_dashboard")
        ),

        # Pestaña 2: Raw Data (Para ver el entorno y errores de source)
        bslib::nav_panel(
          title = "Technical Raw",
          shiny::verbatimTextOutput("debug_verbatim_05_module_loading")
        )
      )
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_99_DEBUG'",
      bslib::navset_card_tab(
        title = "System Master Control - General View",

        # Pestaña 1: El Roadmap visual con barra de progreso
        bslib::nav_panel(
          title = "Roadmap Status",
          shiny::htmlOutput("debug_status_99_dashboard")
        ),

        # Pestaña 2: La tabla técnica original
        bslib::nav_panel(
          title = "Raw Data Frame",
          shiny::verbatimTextOutput("debug_verbatim_99_super_summary")
        )
      )
    )
  ),

  # Group B: Tool Execution Panel
  # Only visible when the Master jumps to 'tab_execute_tool'
  conditionalPanel(
    condition = "input.menu_fixed == 'tab_execute_tool'",
    uiOutput("render_tool_body")
  )
)

server <- function(input, output, session) {

  # Basics
  default_output_list <- list(is_done = FALSE)


  # --- MENU SYNC LOGIC (Alternation) ---
  # A. If the user interacts with the Master Menu
  observeEvent(input$menu_fixed, {
    # If a configuration/debug tab is selected (steps 1 to 4)
    if(input$menu_fixed != "tab_execute_tool") {
      nav_select("active_tool-menu_lateral", selected = "clean")
    }
  }, ignoreInit = TRUE)

  # B. If the user interacts with the Dynamic Tool Menu
  observeEvent(input[["active_tool-menu_lateral"]], {
    req(input[["active_tool-menu_lateral"]])
    # If a real tool tab is selected (steps 5 onwards)
    if(input[["active_tool-menu_lateral"]] != "clean") {
      # Move the Master Menu to the invisible execution tab
      nav_select("menu_fixed", selected = "tab_execute_tool")
    }
  }, ignoreInit = TRUE)


  # --- PHASE 01: IMPORTATION ---  ---------------------------------------------
  OR_01_import_dataset <- module_orchestrator_01_import_dataset_server("master_import")
  output$debug_verbatim_01_import_DEBUG <- renderPrint({
    str(OR_01_import_dataset())
  })
  output$debug_status_01_dashboard <- renderUI({
    data <- OR_01_import_dataset()

    # 1. Validación de seguridad
    if (is.null(data) || !is.list(data)) {
      return(shiny::tags$p(shiny::tags$em("Waiting for data import...")))
    }

    # --- Lógica de Estado ---
    overall_ok <- base::isTRUE(data$is_done)

    # Función para cajas de métricas
    metric_box <- function(label, value, icon_name) {
      shiny::tags$div(
        style = "flex: 1; background: #ffffff; padding: 12px; border-radius: 8px; border: 1px solid #e2e8f0; text-align: center;",
        shiny::tags$div(style = "color: #64748b; font-size: 0.7em; font-weight: bold; text-transform: uppercase;", label),
        shiny::tags$div(style = "font-size: 1.1em; font-weight: bold; color: #1e293b; margin-top: 4px;",
                        shiny::icon(icon_name, style = "color: #94a3b8; margin-right: 5px;"), value)
      )
    }

    shiny::tagList(
      # --- HEADER: STATUS ---
      shiny::tags$div(
        style = "background: #f8fafc; border: 1px solid #e2e8f0; padding: 15px; border-radius: 10px; margin-bottom: 20px; display: flex; align-items: center;",
        shiny::tags$div(
          style = base::paste0("font-size: 1.5em; margin-right: 15px; color: ", base::ifelse(overall_ok, "#10b981", "#ef4444"), ";"),
          shiny::icon(base::ifelse(overall_ok, "check-circle", "exclamation-circle"))
        ),
        shiny::tags$div(
          shiny::tags$h4(style = "margin: 0; color: #0f172a; font-weight: 800; font-size: 1.1em;",
                         base::ifelse(overall_ok, "IMPORT SEQUENCE COMPLETE", "IMPORT SEQUENCE PENDING")),
          shiny::tags$span(style = "color: #64748b; font-size: 0.85em;", data$description_short)
        )
      ),

      # --- DATA SOURCE SELECTION (Uno abajo del otro) ---
      shiny::tags$div(
        style = "margin-bottom: 20px; padding: 15px; background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px;",
        shiny::tags$div(style = "color: #1e40af; font-size: 0.75em; font-weight: bold; text-transform: uppercase; margin-bottom: 10px;",
                        shiny::icon("database"), " Data Source Details"),

        # Label Externo
        shiny::tags$div(style = "margin-bottom: 8px;",
                        shiny::tags$span(style = "color: #1e40af; font-size: 0.8em; font-weight: bold;", "External Name: "),
                        shiny::tags$div(style = "font-size: 1.05em; color: #1e3a8a; font-weight: 600;", data$orquestator_import$name_external)
        ),

        # Label Interno
        shiny::tags$div(
          shiny::tags$span(style = "color: #1e40af; font-size: 0.8em; font-weight: bold;", "Internal ID: "),
          shiny::tags$div(style = "font-family: monospace; font-size: 0.9em; color: #1e40af; background: #dbeafe; padding: 4px 8px; border-radius: 4px; display: inline-block; margin-top: 2px;",
                          data$orquestator_import$name_internal)
        )
      ),

      # --- GRID DE MÉTRICAS ---
      shiny::tags$div(
        style = "display: flex; gap: 12px; margin-bottom: 20px;",
        metric_box("Observations", data$dataset$rows, "list-ol"),
        metric_box("Variables", data$dataset$cols, "columns"),
        metric_box("Format", "Data Frame", "table")
      ),

      # --- TECHNICAL FILE (Inlcuye Label File Name) ---
      shiny::tags$div(
        style = "background: white; border: 1px solid #e2e8f0; border-radius: 8px; overflow: hidden;",
        shiny::tags$div(style = "background: #f1f5f9; padding: 10px 15px; font-weight: bold; font-size: 0.8em; color: #475569; border-bottom: 1px solid #e2e8f0;",
                        "OBJECT ARCHITECTURE"),
        shiny::tags$div(
          style = "padding: 15px; font-size: 0.85em; color: #334155;",

          # File Name & Label
          shiny::tags$div(style = "margin-bottom: 10px; display: flex; gap: 20px;",
                          shiny::tags$div(
                            shiny::tags$strong("File Name: "),
                            shiny::tags$span(style = "color: #0284c7;", data$dataset$file_name)
                          ),
                          shiny::tags$div(
                            shiny::tags$strong("Label Name: "),
                            shiny::tags$span(style = "color: #0284c7;", data$dataset$label_file_name)
                          )
          ),

          # Ruta
          shiny::tags$div(style = "margin-bottom: 10px;",
                          shiny::tags$strong("Internal Reference: "),
                          shiny::tags$code(style = "font-size: 0.9em; background: #f8fafc; border: 1px solid #f1f5f9;", data$dataset$file_path_internal)),

          # Info
          shiny::tags$div(
            style = "padding: 10px; background: #f8fafc; border-left: 3px solid #cbd5e1; font-style: italic; color: #64748b;",
            data$orquestator_import$info
          )
        )
      ),

      # --- PERFORMANCE FOOTER (Tiempos uno abajo del otro) ---
      shiny::tags$div(
        style = "margin-top: 15px; padding: 12px; border-radius: 8px; background: #f8fafc; border: 1px solid #e2e8f0; font-family: monospace; font-size: 0.8em; color: #64748b;",

        shiny::tags$div(style = "margin-bottom: 4px;",
                        shiny::tags$strong("Start: "),
                        if(!is.null(data$init_time)) base::format(data$init_time, "%H:%M:%OS3") else "--:--:--"
        ),

        shiny::tags$div(style = "margin-bottom: 4px;",
                        shiny::tags$strong("End:   "),
                        if(!is.null(data$end_time)) base::format(data$end_time, "%H:%M:%OS3") else "--:--:--"
        ),

        shiny::tags$div(style = "padding-top: 4px; border-top: 1px dashed #cbd5e1; color: #1e293b; font-weight: bold;",
                        shiny::tags$strong("Time:  "),
                        if(is.numeric(data$dataset$time_secs)) base::round(data$dataset$time_secs, 6) else "0.000000",
                        " seconds"
        )
      )
    )
  })

  # --- PHASE 02: TOOL SELECTION ---  ------------------------------------------
  OR_02_tools <- module_tool_selector_server("master_tools", "tools_config_PROD.yml")
  output$debug_verbatim_02_tools_DEBUG <- renderPrint({
    OR_02_tools()
  })
  output$debug_status_02_dashboard <- renderUI({
    data <- OR_02_tools()

    # 1. Validación de seguridad
    if (is.null(data) || !is.list(data)) {
      return(shiny::tags$p(shiny::tags$em("Waiting for tool selection...")))
    }

    # --- Lógica de Estado ---
    overall_ok <- base::isTRUE(data$is_done)

    # Función para cajas de métricas (Estética Neutra)
    metric_box <- function(label, value, icon_name) {
      shiny::tags$div(
        style = "flex: 1; background: #ffffff; padding: 12px; border-radius: 8px; border: 1px solid #e2e8f0; text-align: center;",
        shiny::tags$div(style = "color: #64748b; font-size: 0.7em; font-weight: bold; text-transform: uppercase;", label),
        shiny::tags$div(style = "font-size: 1.05em; font-weight: bold; color: #1e293b; margin-top: 4px;",
                        shiny::icon(icon_name, style = "color: #94a3b8; margin-right: 5px;"), value)
      )
    }

    shiny::tagList(
      # --- HEADER: STATUS ---
      shiny::tags$div(
        style = "background: #f8fafc; border: 1px solid #e2e8f0; padding: 15px; border-radius: 10px; margin-bottom: 20px; display: flex; align-items: center;",
        shiny::tags$div(
          style = base::paste0("font-size: 1.5em; margin-right: 15px; color: ", base::ifelse(overall_ok, "#10b981", "#ef4444"), ";"),
          shiny::icon(base::ifelse(overall_ok, "check-circle", "exclamation-circle"))
        ),
        shiny::tags$div(
          shiny::tags$h4(style = "margin: 0; color: #0f172a; font-weight: 800; font-size: 1.1em;",
                         base::ifelse(overall_ok, "TOOL SELECTION ACTIVE", "TOOL SELECTION PENDING")),
          shiny::tags$span(style = "color: #64748b; font-size: 0.85em;", data$description_short)
        )
      ),

      # --- TOOL & CATEGORY DETAILS (Vertical) ---
      shiny::tags$div(
        style = "margin-bottom: 20px; padding: 15px; background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px;",
        shiny::tags$div(style = "color: #1e40af; font-size: 0.75em; font-weight: bold; text-transform: uppercase; margin-bottom: 10px;",
                        shiny::icon("wrench"), " Selected Configuration"),

        # Categoría
        shiny::tags$div(style = "margin-bottom: 10px;",
                        shiny::tags$span(style = "color: #1e40af; font-size: 0.8em; font-weight: bold;", "Analysis Category: "),
                        shiny::tags$div(style = "font-size: 1.05em; color: #1e3a8a; font-weight: 600;", data$category$external),
                        shiny::tags$div(style = "font-family: monospace; font-size: 0.8em; color: #1e40af; opacity: 0.7;",
                                        base::paste("Code:", data$category$internal))
        ),

        # Herramienta
        shiny::tags$div(
          shiny::tags$span(style = "color: #1e40af; font-size: 0.8em; font-weight: bold;", "Active Tool: "),
          shiny::tags$div(style = "font-size: 1.05em; color: #1e3a8a; font-weight: 600;", data$tool$external),
          shiny::tags$div(style = "font-family: monospace; font-size: 0.8em; color: #1e40af; opacity: 0.7;",
                          base::paste("Code:", data$tool$internal))
        )
      ),

      # --- GRID DE MÉTRICAS RÁPIDAS (Script, Subcategory, Group) ---
      shiny::tags$div(
        style = "display: flex; gap: 12px; margin-bottom: 20px;",
        metric_box("Script", data$script$internal, "terminal"),
        metric_box("Sub-Cat", data$yml_node$subcategory, "folder-tree"),
        metric_box("Group", data$yml_node$statistic_group, "layer-group")
      ),

      # --- TECHNICAL FILE (Paths & YML) ---
      shiny::tags$div(
        style = "background: white; border: 1px solid #e2e8f0; border-radius: 8px; overflow: hidden;",
        shiny::tags$div(style = "background: #f1f5f9; padding: 10px 15px; font-weight: bold; font-size: 0.8em; color: #475569; border-bottom: 1px solid #e2e8f0;",
                        "MODULE ARCHITECTURE"),
        shiny::tags$div(
          style = "padding: 15px; font-size: 0.85em; color: #334155;",

          # Paths
          shiny::tags$div(style = "margin-bottom: 8px;",
                          shiny::tags$strong("Folder Path: "),
                          shiny::tags$span(style = "color: #0284c7; font-family: monospace;", data$paths$folder)),

          shiny::tags$div(style = "margin-bottom: 10px;",
                          shiny::tags$strong("Module File: "),
                          shiny::tags$code(style = "font-size: 0.9em; background: #f8fafc; border: 1px solid #f1f5f9;", data$paths$module)),

          # Info del YML
          shiny::tags$div(
            style = "padding: 10px; background: #f8fafc; border-left: 3px solid #cbd5e1; font-size: 0.9em; color: #64748b;",
            shiny::tags$strong(style = "color: #475569;", "Description: "),
            data$yml_node$description_short
          )
        )
      ),

      # --- PERFORMANCE FOOTER (Tiempos Verticales) ---
      shiny::tags$div(
        style = "margin-top: 15px; padding: 12px; border-radius: 8px; background: #f8fafc; border: 1px solid #e2e8f0; font-family: monospace; font-size: 0.8em; color: #64748b;",

        shiny::tags$div(style = "margin-bottom: 4px;",
                        shiny::tags$strong("Start: "),
                        if(!is.null(data$init_time)) base::format(base::as.POSIXct(data$init_time), "%H:%M:%OS3") else "--:--:--"
        ),

        shiny::tags$div(style = "margin-bottom: 4px;",
                        shiny::tags$strong("End:   "),
                        if(!is.null(data$end_time)) base::format(base::as.POSIXct(data$end_time), "%H:%M:%OS3") else "--:--:--"
        ),

        shiny::tags$div(style = "padding-top: 4px; border-top: 1px dashed #cbd5e1; color: #1e293b; font-weight: bold;",
                        shiny::tags$strong("Time:  "),
                        if(is.numeric(data$diff_secs)) base::round(data$diff_secs, 6) else "0.000000",
                        " seconds"
        )
      )
    )
  })

  # --- PHASE 03: STATE CENTRALIZER (Gatekeeper) --- ---------------------------
  OR_03_CENTRAL_is_done_import_and_tools <- reactive({

    init_time <- lubridate::now()

    is_done_import <- isTRUE(OR_01_import_dataset()$is_done)
    is_done_tools  <- isTRUE(OR_02_tools()$is_done)
    is_done_all    <- all(is_done_import, is_done_tools)

    end_time <- lubridate::now()
    diff_time <- end_time - init_time
    diff_secs <- base::as.numeric(diff_time, units = "secs")

    list_output <- list(
      "is_done" = is_done_all,
      "description_short" = "Central Point.",
      "init_time" = init_time,
      "end_time" = end_time,
      "diff_secs" = diff_secs,
      previous_steps = list(
        "is_done_import" = is_done_import,
        "is_done_tools" = is_done_tools
      )
    )
    return(list_output)
  })
  output$debug_verbatim_03_is_done_all_DEBUG <- renderPrint({
    OR_03_CENTRAL_is_done_import_and_tools()
  })
  output$debug_status_03_dashboard <- renderUI({
    # 1. Validación de entrada
    data <- OR_03_CENTRAL_is_done_import_and_tools()
    if (is.null(data) || !is.list(data)) {
      return(shiny::tags$p(shiny::tags$em("Waiting for reactive data...")))
    }

    # 2. Funciones auxiliares para filas (Status)
    status_row <- function(label, status) {
      is_ok <- base::isTRUE(status)
      icon_name <- base::ifelse(is_ok, "check-circle", "times-circle")
      icon_color <- base::ifelse(is_ok, "#28a745", "#dc3545")

      shiny::tags$div(
        style = "display: flex; align-items: center; margin-bottom: 8px; padding: 8px; border-bottom: 1px solid #f0f0f0;",
        shiny::span(style = base::paste0("color:", icon_color, "; margin-right: 15px; font-size: 1.2em;"),
                    shiny::icon(icon_name)),
        shiny::tags$strong(style = "width: 100px;", label),
        shiny::tags$span(base::ifelse(is_ok, "Passed", "Pending/Failed"))
      )
    }

    # 3. Lógica del Banner
    overall_ok   <- base::isTRUE(data$is_done)
    banner_bg    <- base::ifelse(overall_ok, "#d4edda", "#f8d7da")
    banner_color <- base::ifelse(overall_ok, "#155724", "#721c24")
    banner_text  <- base::ifelse(overall_ok, "✅ STEP 03: GATE OPEN", "🔒 STEP 03: GATE LOCKED")

    # 4. Construcción del UI
    shiny::tagList(
      # BANNER GLOBAL
      shiny::tags$div(
        style = base::paste0("background-color: ", banner_bg, "; color: ", banner_color,
                             "; padding: 15px; border-radius: 8px; margin-bottom: 20px; text-align: center; border: 1px solid;"),
        shiny::tags$h4(style = "margin: 0; font-weight: bold;", banner_text)
      ),

      shiny::tags$h5("Prerequisites Checklist:"),

      # FILAS DE CADA PASO
      shiny::tags$div(
        style = "background: white; border: 1px solid #ddd; border-radius: 5px; padding: 10px;",
        status_row("IMPORT", data$previous_steps$is_done_import),
        status_row("TOOLS", data$previous_steps$is_done_tools)
      ),

      # --- SECCIÓN DE TIEMPOS (PERFORMANCE) ---
      shiny::tags$div(
        style = "margin-top: 20px; padding: 12px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef; font-family: monospace; font-size: 0.85em;",
        shiny::tags$div(style = "color: #495057; font-weight: bold; margin-bottom: 5px; font-family: sans-serif;", "⏱ Execution Metadata:"),
        shiny::tags$div(base::paste("Start:   ", base::format(data$init_time, "%H:%M:%OS3"))),
        shiny::tags$div(base::paste("End:     ", base::format(data$end_time, "%H:%M:%OS3"))),
        shiny::tags$div(
          style = "margin-top: 5px; padding-top: 5px; border-top: 1px dashed #ced4da; color: #0d6efd; font-weight: bold;",
          base::paste("Duration:", base::round(data$diff_secs, 4), "seconds")
        )
      ),

      # NOTA INFORMATIVA
      shiny::tags$p(
        style = "margin-top: 15px; font-size: 0.85em; color: #6c757d; font-style: italic;",
        "Note: Both prerequisites must be TRUE for the Central Gate to open."
      )
    )
  })


  # --- PHASE 04: TEMPORAL FF and ENV (Files and Folders) --- --------------------------
  OR_04_temporal_FF <- reactive({

    init_time <- lubridate::now()

    # 1. Capture reactive snapshots
    internal_OR_02_tools   <- OR_02_tools()
    internal_OR_03_all_ok  <- OR_03_CENTRAL_is_done_import_and_tools()

    is_previous_done <- internal_OR_03_all_ok$is_done

    if (!isTRUE(is_previous_done)) {
      return(default_output_list)
    }

    # 01. Basics
    str_tool_folder_INFO <- internal_OR_02_tools$paths$folder
    str_tool_module_file_INFO <- internal_OR_02_tools$paths$module
    current_time <- lubridate::now()
    formatted_time_lubridate <- base::format(current_time, "%Y_%m_%d_%H_%M_%S")
    new_subfolder_Rscience <- paste0("Rscience_", formatted_time_lubridate)
    tool_execution_env <- new.env(parent = .GlobalEnv)

    # 02. Local Files and Folders (local_FF)
    fn3_step02_local  <- function(){
      str_tool_folder_LOCAL      <- system.file("shiny", str_tool_folder_INFO,      package = "Rscience3")
      str_tool_module_file_LOCAL <- system.file("shiny", str_tool_module_file_INFO, package = "Rscience3")
      check_folder_exists_LOCAL      <- dir.exists(str_tool_folder_LOCAL)
      check_module_file_exists_LOCAL <- file.exists(str_tool_module_file_LOCAL)

      is_done <- base::isTRUE(check_folder_exists_LOCAL) && base::isTRUE(check_module_file_exists_LOCAL)

      the_list <- list(
        is_done = is_done,
        description_short = "Temporal FF.",
        folder = list(
          str_path = str_tool_folder_LOCAL,
          check_exists = check_folder_exists_LOCAL
        ),
        module_file = list(
          str_path = str_tool_module_file_LOCAL,
          check_exists = check_module_file_exists_LOCAL
        )
      )

      return(the_list)
    }
    list_output_step02_local <- fn3_step02_local()

    # 03. Temporal basics
    fn3_step03_temp_Rscience <- function(){
      library("fs")
      general_temp_folder_path   <- base::tempdir()
      Rscience_temp_folder_path  <- base::file.path(general_temp_folder_path, new_subfolder_Rscience)
      str_tool_folder_TEMPORAL   <- Rscience_temp_folder_path

      # 2. Create the final directory
      # 'fs::dir_create' is better than 'dir.create' because it handles
      # nested paths and doesn't throw an error if it already exists.
      fs::dir_create(str_tool_folder_TEMPORAL)

      # 3. Verification
      check_folder <- fs::dir_exists(str_tool_folder_TEMPORAL)
      if (check_folder) {
        base::print(base::paste("Success: Folder created at", str_tool_folder_TEMPORAL))
      }

      # is done
      is_done <- check_folder

      the_list <- list(
        is_done = is_done,
        description = "Step03 - New Rscience temporal folder.",
        folder = list(
          str_path = str_tool_folder_TEMPORAL,
          check_exists = check_folder
        )
      )

      return(the_list)
    }
    list_output_step03_temp_basics <- fn3_step03_temp_Rscience()


    # 04. Copying folder LOCAL to TEMP
    # 04. Copying folder LOCAL to TEMP
    copy_directory_safely <- function(source_folder_path, target_folder_path, overwrite_flag = FALSE) {

      # 1. Check if source exists
      if (!fs::dir_exists(source_folder_path)) {
        base::stop("Source directory does not exist.")
      }

      # 2. FIX: Get only the NAME of the source folder (e.g., "my_tool")
      # base::basename("path/to/my_tool") -> "my_tool"
      folder_name <- base::basename(source_folder_path)

      # 3. FIX: Construct the final path: "temp_dir/my_tool"
      final_destination <- base::file.path(target_folder_path, folder_name)

      # 4. Check if destination exists
      if (fs::dir_exists(final_destination) && !overwrite_flag) {
        base::message("Destination already exists.")
        return(FALSE)
      }

      # 5. Execute copy
      # fs::dir_copy will create 'final_destination' and put everything inside
      fs::dir_copy(
        path = source_folder_path,
        new_path = final_destination,
        overwrite = overwrite_flag
      )

      base::print(base::paste("Folder copied to:", final_destination))
      return(TRUE)
    }



    fn3_step04_copying_FF <- function(){

      # Info basics
      str_subfolder <- str_tool_folder_INFO
      str_module_file <- str_tool_module_file_INFO

      # Local basics
      str_folder_script_LOCAL <- list_output_step02_local$"folder"$"str_path"

      # Temp basics
      temp_Rscience_folder_path <- list_output_step03_temp_basics$"folder"$"str_path"


      is_copied <- copy_directory_safely(source_folder_path = str_folder_script_LOCAL,
                            target_folder_path = temp_Rscience_folder_path,
                            overwrite_flag = FALSE)

      # Temp work folder PROC
      temp_script_folder_path <- file.path(temp_Rscience_folder_path, str_subfolder)
      check_folder <- dir.exists(temp_script_folder_path)

      # Temp module file path PROC
      temp_script_module_file_path <- file.path(temp_Rscience_folder_path, str_module_file)
      check_module_file <-file.exists(temp_script_module_file_path)

      is_done <- base::isTRUE(is_copied) &&
                  base::isTRUE(check_folder) &&
                    base::isTRUE(check_module_file)


      the_list <- list(
        is_done = is_done,
        description = "Step03 - New Rscience temporal folder.",
        temp_copy = list(
          "from" = str_folder_script_LOCAL,
          "to" = temp_Rscience_folder_path,
          "is_copied" = is_copied
        ),
        temp_script_folder = list(
          "str_path" = temp_script_folder_path,
          "check_exists" = check_folder
        ),
        temp_module_file = list(
          "str_path" = temp_script_module_file_path,
          "check_exists" = check_module_file
        )
      )

      return(the_list)


    }
    list_output_step04_copying_FF <- fn3_step04_copying_FF()



    # Final
    is_done <- TRUE

    end_time <- lubridate::now()
    diff_time <- end_time - init_time
    diff_secs <- base::as.numeric(diff_time, units = "secs")

    list_output <- list(
      is_done = TRUE,
      description_short = "Temporal FF.",
      init_time = init_time,
      end_time = end_time,
      diff_secs = diff_secs,
      tool_execution_env = tool_execution_env,
      local = list_output_step02_local,
      temp_basics = list_output_step03_temp_basics,
      temp_copying_FF = list_output_step04_copying_FF
    )

    return(list_output)
  })
  output$debug_verbatim_04_temporal_FF_DEBUG <- renderPrint({
    OR_04_temporal_FF()
  })
  output$debug_status_04_dashboard <- renderUI({
    # --- 1. VALIDACIÓN DE ENTRADA (GUARDRAIL) ---
    data <- OR_04_temporal_FF()

    # Si los datos no están listos o falta el cálculo de tiempo, mostramos espera
    if (is.null(data) || is.null(data$diff_secs)) {
      return(shiny::tags$p(shiny::tags$em("Waiting for Step 04 process to initialize...")))
    }

    # --- 2. FUNCIONES AUXILIARES ---
    file_row <- function(label, path, exists) {
      exists_safe <- base::isTRUE(exists)
      icon_name   <- base::ifelse(exists_safe, "check-circle", "times-circle")
      icon_color  <- base::ifelse(exists_safe, "#28a745", "#dc3545")

      shiny::tags$div(
        style = "margin-bottom: 5px; border-bottom: 1px solid #eee; padding: 5px;",
        shiny::span(style = base::paste0("color:", icon_color), shiny::icon(icon_name)),
        shiny::tags$strong(base::paste0(" ", label, ": ")),
        shiny::tags$code(style = "font-size: 0.85em; background: #f8f9fa; color: #333;",
                         base::ifelse(is.null(path) || path == "", "Pending...", path))
      )
    }

    # --- 3. CONSTRUCCIÓN DEL DASHBOARD ---
    overall_ok <- base::isTRUE(data$is_done)
    banner_bg    <- base::ifelse(overall_ok, "#d4edda", "#f8d7da")
    banner_color <- base::ifelse(overall_ok, "#155724", "#721c24")
    banner_text  <- base::ifelse(overall_ok, "✅ STEP 04: ALL FILES READY", "❌ STEP 04: SYNCHRONIZATION ERROR")

    shiny::tagList(
      # BANNER DE ESTADO GLOBAL
      shiny::tags$div(
        style = base::paste0("background-color: ", banner_bg, "; color: ", banner_color,
                             "; padding: 15px; border-radius: 8px; margin-bottom: 20px; text-align: center; border: 1px solid;"),
        shiny::tags$h4(style = "margin: 0; font-weight: bold;", banner_text)
      ),

      shiny::tags$h5("Detailed File Inspection"),

      # Sección LOCAL
      shiny::tags$div(
        style = "background: #e9ecef; padding: 10px; border-left: 5px solid #0d6efd; margin-bottom: 10px;",
        shiny::tags$h6("Source (Package Internal)"),
        file_row("Local Folder", data$local$folder$str_path, data$local$folder$check_exists),
        file_row("Module File", data$local$module_file$str_path, data$local$module_file$check_exists)
      ),

      # Sección TEMPORAL
      shiny::tags$div(
        style = "background: #fff3cd; padding: 10px; border-left: 5px solid #ffc107;",
        shiny::tags$h6("Destination (Temp System)"),
        file_row("Temp Root", data$temp_basics$folder$str_path, data$temp_basics$folder$check_exists),
        file_row("Copied Folder", data$temp_copying_FF$temp_script_folder$str_path, data$temp_copying_FF$temp_script_folder$check_exists),
        file_row("Module File", data$temp_copying_FF$temp_module_file$str_path, data$temp_copying_FF$temp_module_file$check_exists)
      ),

      # --- SECCIÓN DE TIEMPOS (PERFORMANCE) ---
      shiny::tags$div(
        style = "margin-top: 20px; padding: 12px; background-color: #f1f3f5; border-radius: 8px; border: 1px solid #dee2e6; font-family: monospace; font-size: 0.85em;",
        shiny::tags$div(style = "color: #495057; font-weight: bold; margin-bottom: 5px; font-family: sans-serif;", "⏱ Execution Metadata:"),

        # Formateo seguro de timestamps
        shiny::tags$div(base::paste("Start:   ", base::format(data$init_time, "%H:%M:%OS3"))),
        shiny::tags$div(base::paste("End:     ", base::format(data$end_time, "%H:%M:%OS3"))),

        shiny::tags$div(
          style = "margin-top: 5px; padding-top: 5px; border-top: 1px dashed #ced4da; color: #198754; font-weight: bold;",
          # Aquí es donde fallaba: ahora solo se ejecuta si data$diff_secs existe
          base::paste("Total Copy Duration:", base::round(as.numeric(data$diff_secs), 4), "seconds")
        )
      ),

      shiny::tags$p(
        style = "margin-top: 15px; font-size: 0.85em; color: #6c757d; font-style: italic;",
        "Note: Files are mirrored to a temporary directory to allow local environment execution."
      )
    )
  })


  # --- PHASE 05: MODULE EXTRACTION (Agnostic Dispatcher) ---
  OR_05_module_loading <- reactive({

    init_time <- lubridate::now()

    internal_OR_04_temporal_FF <- OR_04_temporal_FF()
    if (!isTRUE(internal_OR_04_temporal_FF$is_done)) return(list(is_done = FALSE))

    # 01. Basics
    tool_execution_env <- internal_OR_04_temporal_FF$tool_execution_env
    str_tool_module_file_TEMPORAL <- internal_OR_04_temporal_FF$temp_copying_FF$temp_module_file$str_path
    source(str_tool_module_file_TEMPORAL, local = tool_execution_env)

    # Look for functions INSIDE the tool's private environment
    # env <- phase3$tool_env

    required <- c("module_ui_menu", "module_ui_body", "module_server")
    check_exists <- sapply(required, exists, envir = tool_execution_env)

    if(!all(check_exists)) {
      return(list(is_done = FALSE, error = "Functions not found in tool file"))
    }

    is_done <- check_exists

    end_time <- lubridate::now()
    diff_time <- end_time - init_time
    diff_secs <- base::as.numeric(diff_time, units = "secs")

    list(
      is_done = TRUE,
      description_short = "Loading selected module.",
      init_time = init_time,
      end_time = end_time,
      diff_secs = diff_secs,
      all_fn_exists = check_exists,
      menu    = get("module_ui_menu", envir = tool_execution_env),
      body    = get("module_ui_body", envir = tool_execution_env),
      server  = get("module_server",  envir = tool_execution_env)
    )
  })
  output$debug_verbatim_05_module_loading <- renderPrint({
    OR_05_module_loading()
  })
  output$debug_status_05_dashboard <- renderUI({
    # 1. Validación de entrada (Guardrail contra valores NULL o iniciales)
    data <- OR_05_module_loading()

    # Si no hay datos, o el proceso falló antes de calcular el tiempo, esperamos.
    if (is.null(data) || is.null(data$diff_secs)) {
      return(shiny::tags$p(shiny::tags$em("Waiting for Step 04 and Module Source...")))
    }

    # 2. Función para filas de objetos
    fn_row <- function(fn_name, exists) {
      is_ok <- base::isTRUE(exists)
      status_symbol <- base::ifelse(is_ok, "✅", "❌")

      shiny::tags$div(
        style = "display: flex; align-items: center; margin-bottom: 8px; padding: 10px; border-bottom: 1px solid #f0f0f0;",
        shiny::span(style = "margin-right: 15px; font-size: 1.2em;", status_symbol),
        shiny::tags$code(style = "width: 180px; font-weight: bold; color: #2c3e50; font-size: 1.1em; background: none;",
                         fn_name),
        shiny::tags$span(style = "color: #7f8c8d; font-style: italic;",
                         base::ifelse(is_ok, "Object found in Environment", "Object NOT found"))
      )
    }

    # 3. Lógica del Banner
    overall_ok   <- base::isTRUE(data$is_done)
    banner_bg    <- base::ifelse(overall_ok, "#ebf5fb", "#fef9e7")
    banner_color <- "#2c3e50"
    banner_border <- base::ifelse(overall_ok, "#aed6f1", "#f9e79f")

    # 4. Construcción del UI
    shiny::tagList(
      # BANNER GLOBAL
      shiny::tags$div(
        style = base::paste0("background-color: ", banner_bg, "; color: ", banner_color,
                             "; padding: 15px; border-radius: 8px; margin-bottom: 20px; text-align: center; border: 2px solid ", banner_border, ";"),
        shiny::tags$h4(style = "margin: 0; font-weight: bold;",
                       base::ifelse(overall_ok, "READY: Module Objects Loaded", "ATTENTION: Objects Missing"))
      ),

      shiny::tags$h5(style = "color: #34495e; margin-left: 5px;", "Environment Objects Inspection:"),

      # CONTENEDOR DE OBJETOS
      shiny::tags$div(
        style = "background: #ffffff; border: 1px solid #dcdde1; border-radius: 5px;",
        fn_row("module_ui_menu", data$all_fn_exists["module_ui_menu"]),
        fn_row("module_ui_body", data$all_fn_exists["module_ui_body"]),
        fn_row("module_server",  data$all_fn_exists["module_server"])
      ),

      # --- SECCIÓN DE TIEMPOS (PERFORMANCE) ---
      shiny::tags$div(
        style = "margin-top: 20px; padding: 12px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef; font-family: monospace; font-size: 0.85em;",
        shiny::tags$div(style = "color: #495057; font-weight: bold; margin-bottom: 5px; font-family: sans-serif;", "⏱ Source & Dispatch Performance:"),
        shiny::tags$div(base::paste("Start Source: ", base::format(data$init_time, "%H:%M:%OS3"))),
        shiny::tags$div(base::paste("End Dispatch:   ", base::format(data$end_time, "%H:%M:%OS3"))),
        shiny::tags$div(
          style = "margin-top: 5px; padding-top: 5px; border-top: 1px dashed #ced4da; color: #6f42c1; font-weight: bold;",
          base::paste("Total Loading Time:", base::round(as.numeric(data$diff_secs), 4), "seconds")
        )
      ),

      # MOSTRAR ERROR SI EXISTE
      if (!overall_ok && !base::is.null(data$error)) {
        shiny::tags$div(
          style = "margin-top: 15px; color: #a94442; background-color: #f2dede; padding: 12px; border-radius: 4px; border: 1px solid #ebccd1;",
          shiny::tags$strong("Dispatcher Error: "), data$error
        )
      }
    )
  })



  OR_99_super_summary <- shiny::reactive({

    # 1. Captura de snapshots reactivos
    steps_data <- list(
      OR_01_import_dataset(),
      OR_02_tools(),
      OR_03_CENTRAL_is_done_import_and_tools(),
      OR_04_temporal_FF(),
      OR_05_module_loading()
    )

    # 2. Creación del Data Frame detallado
    df_summary <- purrr::map_dfr(base::seq_along(steps_data), function(i) {
      item <- steps_data[[i]]

      # Validación de estado y descripción
      safe_status <- base::isTRUE(item$is_done)
      safe_desc   <- base::ifelse(base::is.null(item$description_short), "Step pending", item$description_short)

      # LÓGICA DE TIEMPO ROBUSTA:
      # Si existe diff_secs lo usa; si no, lo calcula restando end - init
      duration_val <- 0
      if (!base::is.null(item$diff_secs)) {
        duration_val <- item$diff_secs
      } else if (!base::is.null(item$init_time) && !base::is.null(item$end_time)) {
        duration_val <- base::as.numeric(item$end_time - item$init_time, units = "secs")
      }

      base::data.frame(
        step_num    = i,
        status_bool = safe_status,
        status_icon = base::ifelse(safe_status, "✅", "❌"),
        description = base::as.character(safe_desc),
        duration    = base::as.numeric(duration_val),
        stringsAsFactors = FALSE
      )
    })

    # 3. Cálculos Globales
    is_system_ready <- base::all(df_summary$status_bool)

    # Tiempos globales
    global_init <- steps_data[[1]]$init_time
    # Buscamos el último end_time disponible en la cadena
    global_end  <- steps_data[[5]]$end_time

    # Si el sistema no ha terminado, el end_time global es el del último paso completado
    if (is.null(global_end)) {
      completed_steps <- base::which(df_summary$status_bool)
      if (length(completed_steps) > 0) {
        global_end <- steps_data[[max(completed_steps)]]$end_time
      }
    }

    total_performance_secs <- base::sum(df_summary$duration, na.rm = TRUE)

    # 4. Retorno de Lista Maestra
    list(
      is_done        = is_system_ready,
      df             = df_summary,
      init_time      = global_init,
      end_time       = global_end,
      total_duration = total_performance_secs
    )
  })
  output$debug_verbatim_99_super_summary <- renderPrint({
    OR_99_super_summary()
  })
  output$debug_status_99_dashboard <- renderUI({
    master_data <- OR_99_super_summary()
    df_data     <- master_data$df

    if (base::is.null(df_data) || base::nrow(df_data) == 0) {
      return(shiny::tags$p("Initializing Roadmap..."))
    }

    # Barra de progreso
    ready_steps  <- base::sum(df_data$status_bool)
    progress_pct <- base::round((ready_steps / base::nrow(df_data)) * 100)

    shiny::tagList(
      # --- HEADER DE PERFORMANCE GLOBAL ---
      shiny::tags$div(
        style = "display: flex; gap: 15px; margin-bottom: 20px;",

        # Card 1: ESTADO
        shiny::tags$div(
          style = base::paste0("flex: 1; padding: 15px; border-radius: 10px; text-align: center; border: 2px solid; ",
                               base::ifelse(master_data$is_done,
                                            "background: #e6fffa; color: #234e52; border-color: #b2f5ea;",
                                            "background: #fff5f5; color: #822727; border-color: #feb2b2;")),
          shiny::tags$div(style = "font-size: 0.7em; text-transform: uppercase; letter-spacing: 1px; font-weight: bold;", "System Status"),
          shiny::tags$div(style = "font-size: 1.3em; font-weight: 900;",
                          base::ifelse(master_data$is_done, "FULLY READY", "INITIALIZING"))
        ),

        # Card 2: TIEMPO TOTAL
        shiny::tags$div(
          style = "flex: 1; padding: 15px; border-radius: 10px; text-align: center; background: #2c3e50; color: white; border: 2px solid #1a252f;",
          shiny::tags$div(style = "font-size: 0.7em; text-transform: uppercase; letter-spacing: 1px; opacity: 0.8;", "Total Execution"),
          shiny::tags$div(style = "font-size: 1.3em; font-weight: 900;",
                          base::paste0(base::round(master_data$total_duration, 4), " s"))
        )
      ),

      # Barra de progreso
      shiny::tags$div(
        style = "width: 100%; background: #edf2f7; height: 8px; border-radius: 4px; margin-bottom: 25px; overflow: hidden; border: 1px solid #e2e8f0;",
        shiny::tags$div(style = base::paste0("width: ", progress_pct, "%; background: #2c3e50; height: 100%; transition: width 0.6s cubic-bezier(0.4, 0, 0.2, 1);"))
      ),

      # --- ROADMAP ---
      shiny::tags$div(
        style = "border: 1px solid #dcdde1; border-radius: 8px; background: white; box-shadow: 0 4px 6px rgba(0,0,0,0.02);",
        base::lapply(base::seq_len(base::nrow(df_data)), function(i) {
          is_ok <- df_data$status_bool[i]
          shiny::tags$div(
            style = base::paste0("display: flex; align-items: center; padding: 12px 15px; border-bottom: 1px solid #edf2f7; ",
                                 base::ifelse(is_ok, "", "background-color: #fffaf0;")),

            shiny::tags$div(style = "width: 25px; font-weight: bold; color: #a0aec0;", base::paste0("0", i)),
            shiny::span(style = "margin-right: 12px; font-size: 1.1em;", df_data$status_icon[i]),
            shiny::tags$div(style = "flex-grow: 1; color: #2d3748; font-weight: 500;", df_data$description[i]),

            # Tiempo individual
            shiny::tags$span(style = "font-family: 'Courier New', monospace; font-size: 0.85em; background: #f7fafc; padding: 2px 6px; border-radius: 4px; border: 1px solid #e2e8f0; color: #4a5568;",
                             base::paste0(base::round(df_data$duration[i], 4), "s"))
          )
        })
      ),

      # --- FOOTER METADATA ---
      shiny::tags$div(
        style = "margin-top: 20px; padding: 10px; border-top: 2px solid #f7fafc; display: flex; justify-content: space-between; font-size: 0.75em; color: #a0aec0; font-family: monospace;",
        shiny::tags$span(base::paste("Launch:", base::format(master_data$init_time, "%H:%M:%OS3"))),
        shiny::tags$span(base::paste("Checkpoint:", base::format(master_data$end_time, "%H:%M:%OS3")))
      )
    )
  })
  # --- PHASE 06: DYNAMIC RENDERING ---

  # 1. Render Lateral Menu (Tool)
  output$render_tool_menu <- renderUI({
    res <- OR_05_module_loading()
    req(res$is_done)
    res$menu("active_tool")
  })

  # 2. Render Body (Activated via conditionalPanel in UI)
  output$render_tool_body <- renderUI({
    res <- OR_05_module_loading()
    if(!res$is_done) {
      return(card(card_header("System not ready"), "Complete steps 1 and 2."))
    }
    res$body("active_tool")
  })

  # 3. Execution of Module Server
  observe({
    res <- OR_05_module_loading()
    req(res$is_done)
    # Execute the loaded tool's server
    res$server("active_tool", OR_01_import_dataset = OR_01_import_dataset)
  })

}

shinyApp(ui, server)
