

# Source global
source(file = "global.R")

SHOW_DEBUG <- TRUE

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience - Centralized Preview",

  # --- BLOQUE TÉCNICO: CSS PARA OCULTAR PESTAÑAS DE CONTROL ---
  tags$head(
    tags$style(HTML("
      /* Oculta la pestaña puente del menú maestro */
      .nav-link[data-value='tab_execute_tool'],
      .nav-item:has(> .nav-link[data-value='tab_execute_tool']) {
        display: none !important;
      }

      /* Oculta la pestaña de limpieza del menú dinámico (ANOVA, etc) */
      .nav-link[data-value='clean'],
      .nav-item:has(> .nav-link[data-value='clean']) {
        display: none !important;
      }

      /* Ajuste estético para el hr entre menús */
      hr {
        margin: 1rem 0;
        opacity: 0.15;
      }
    "))
  ),

  sidebar = sidebar(
    # MENU 1: Maestro (Orquestador)
    navset_pill_list(
      id = "menu_fixed",
      well = FALSE,
      nav_panel("1. Dataset", value = "tab_import"),
      if(SHOW_DEBUG) nav_panel("1.1. Debug Dataset", value = "tab_import_DEBUG"),
      nav_panel("2. Tools", value = "tab_tools"),
      if(SHOW_DEBUG) nav_panel("2.1. Tools Debug", value = "tab_tools_DEBUG"),
      if(SHOW_DEBUG) nav_panel("3.1. Temporal FF Debug", value = "tab_temporal_FF_DEBUG"),
      if(SHOW_DEBUG) nav_panel("4.1. Loading FF", value = "tab_loading_FF_DEBUG"),

      # Pestaña lógica: El CSS la hace invisible
      nav_panel("HIDDEN", value = "tab_execute_tool")
    ),

    hr(),

    # MENU 2: Dinámico (Cargado desde el módulo)
    uiOutput("render_tool_menu")
  ),

  # --- ÁREA PRINCIPAL UNIFICADA (MAIN) ---

  # Grupo A: Paneles de Configuración y Debug del Maestro
  # Solo visibles si el foco NO está en la herramienta ejecutándose
  conditionalPanel(
    condition = "input.menu_fixed != 'tab_execute_tool'",

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_import'",
      module_orchestrator_01_import_dataset_ui("master_import")
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_import_DEBUG'",
      card(card_header("Import Debug"), verbatimTextOutput("debug_verbatim_01_import_DEBUG"))
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_tools'",
      module_tool_selector_ui("master_tools")
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_tools_DEBUG'",
      card(card_header("Tools Debug"), verbatimTextOutput("debug_verbatim_02_tools_DEBUG"))
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_temporal_FF_DEBUG'",
      card(card_header("Temporal FF Debug"), verbatimTextOutput("debug_verbatim_03_temporal_FF_DEBUG"))
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_loading_FF_DEBUG'",
      card(card_header("Loading FF Debug"), verbatimTextOutput("debug_verbatim_04_loading_FF_DEBUG"))
    )
  ),

  # Grupo B: Panel de Ejecución de la Herramienta
  # Solo visible cuando el Maestro salta a 'tab_execute_tool'
  conditionalPanel(
    condition = "input.menu_fixed == 'tab_execute_tool'",
    uiOutput("render_tool_body")
  )
)



server <- function(input, output, session) {

  # observe({
  #   nav_hide("menu_fixed", target = "tab_import_DEBUG")
  #   nav_hide("menu_fixed", target = "tab_tools_DEBUG")
  #   nav_hide("menu_fixed", target = "tab_temporal_FF_DEBUG")
  #   nav_hide("menu_fixed", target = "tab_loading_FF_DEBUG")
  # })

  # --- LOGICA DE SINCRONIZACIÓN DE MENÚS (Alternancia) ---

  # A. Si el usuario interactúa con el Menú Maestro
  observeEvent(input$menu_fixed, {
    # Si selecciona una pestaña de configuración/debug (pasos 1 al 4)
    if(input$menu_fixed != "tab_execute_tool") {
      # Usamos nav_select de bslib
      nav_select("active_tool-menu_lateral", selected = "clean")
    }
  }, ignoreInit = TRUE)

  # B. Si el usuario interactúa con el Menú de la Herramienta Dinámica
  observeEvent(input[["active_tool-menu_lateral"]], {
    req(input[["active_tool-menu_lateral"]])
    # Si selecciona una pestaña real de la herramienta (pasos 5 en adelante)
    if(input[["active_tool-menu_lateral"]] != "clean") {
      # Movemos el Menú Maestro a la pestaña invisible de ejecución
      nav_select("menu_fixed", selected = "tab_execute_tool")
    }
  }, ignoreInit = TRUE)


  # --- FASE 01: IMPORTACIÓN ---
  OR_01_import_dataset <- module_orchestrator_01_import_dataset_server("master_import")

  output$debug_verbatim_01_import_DEBUG <- renderPrint({
    str(OR_01_import_dataset())
  })


  # --- FASE 02: SELECCIÓN DE HERRAMIENTA ---
  OR_02_tools <- module_tool_selector_server("master_tools", "tools_config_PROD.yml")

  output$debug_verbatim_02_tools_DEBUG <- renderPrint({
    OR_02_tools()
  })


  # --- CENTRALIZADOR DE ESTADO (Gatekeeper) ---
  OR_CENTRAL_is_done_import_and_tools <- reactive({
    is_done_import <- isTRUE(OR_01_import_dataset()$is_done)
    is_done_tools  <- isTRUE(OR_02_tools()$is_done)
    all(is_done_import, is_done_tools)
  })


  # --- FASE 03: GESTIÓN TEMPORAL (Files & Folders) ---
  OR_03_temporal_FF <- reactive({
    is_ready <- OR_CENTRAL_is_done_import_and_tools()
    if (!is_ready) return(list(is_done = FALSE, message = "Esperando Fase 1 y 2..."))

    t_start     <- Sys.time()
    time_folder <- format(t_start, "%Y_%m_%d_%H_%M_%S")
    tool_data   <- OR_02_tools()
    base_path   <- ".."

    safe_tool_id <- gsub("[^a-zA-Z0-9._-]", "_", tool_data$tool)
    path_local_folder <- file.path(base_path, tool_data$folder)
    path_local_file   <- file.path(base_path, tool_data$special_path)

    exists_local_dir  <- dir.exists(path_local_folder)
    exists_local_file <- file.exists(path_local_file)

    temp_name      <- paste0("RscienceTemp_", safe_tool_id, "_", time_folder)
    full_temp_path <- file.path(tempdir(), temp_name)
    dest_file_path <- file.path(full_temp_path, basename(path_local_file))

    if(exists_local_dir) {
      dir.create(full_temp_path, showWarnings = FALSE, recursive = TRUE)
      if(exists_local_file) {
        file.copy(from = path_local_file, to = full_temp_path, overwrite = TRUE)
        # IMPORTANTE: Cargamos el código de la herramienta
        source(dest_file_path, local = FALSE)
      }
    }

    list(
      is_done = all(exists_local_dir, exists_local_file, dir.exists(full_temp_path), file.exists(dest_file_path)),
      local_assets = list(folder_exists = exists_local_dir, file_exists = exists_local_file),
      temporal_assets = list(folder_exists = dir.exists(full_temp_path), path = full_temp_path)
    )
  })

  output$debug_verbatim_03_temporal_FF_DEBUG <- renderPrint({
    OR_03_temporal_FF()
  })


  # --- FASE 04: CARGA DE MÓDULOS (Dispatcher) ---
  OR_04_module_loading <- reactive({
    phase3 <- OR_03_temporal_FF()
    if (!isTRUE(phase3$is_done)) return(list(is_done = FALSE))

    required <- c("module_ui_menu", "module_ui_body", "module_server")
    check_exists <- sapply(required, exists, envir = .GlobalEnv)

    if(!all(check_exists)) return(list(is_done = FALSE, error = "Módulos no hallados"))

    list(
      is_done = TRUE,
      menu    = get("module_ui_menu", envir = .GlobalEnv),
      body    = get("module_ui_body", envir = .GlobalEnv),
      server  = get("module_server",  envir = .GlobalEnv)
    )
  })

  output$debug_verbatim_04_loading_FF_DEBUG <- renderPrint({
    OR_04_module_loading()
  })


  # --- FASE 05: RENDERIZADO DINÁMICO ---

  # 1. Render Menu Lateral (Herramienta)
  output$render_tool_menu <- renderUI({
    res <- OR_04_module_loading()
    req(res$is_done)
    res$menu("active_tool")
  })

  # 2. Render Body (Se activa vía conditionalPanel en la UI)
  output$render_tool_body <- renderUI({
    res <- OR_04_module_loading()
    if(!res$is_done) {
      return(card(card_header("Sistema no listo"), "Complete los pasos 1 y 2."))
    }
    res$body("active_tool")
  })

  # 3. Ejecución del Servidor del Módulo
  observe({
    res <- OR_04_module_loading()
    req(res$is_done)
    # Ejecutamos el server de la herramienta cargada
    res$server("active_tool", OR_01_import_dataset = OR_01_import_dataset)
  })

}


shinyApp(ui, server)
