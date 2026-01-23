library(shiny)
library(bslib)
library(fs)

source(file = "fn.R")
source(file = "../app002_01_menu_tools/module_menu_export.R")
# ============================================================
# ORCHESTRATOR
# ============================================================
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience 1.0",
  tags$head(
    tags$style("
      .nav-item:has(a[data-value='clean']) { display: none !important; }
      .module-main-content .nav.nav-tabs { display: none !important; }
      .module-main-content .tab-content { border: none !important; }
    ")
  ),
  sidebar = sidebar(
    navset_pill_list(
      id = "menu_fixed", well = FALSE,
      nav_panel("1. Dataset", value = "fixed_data"),
      nav_panel("2. Tools", value = "fixed_tools"),
      nav_panel("", value = "clean")
    ),
    uiOutput("separator"),
    uiOutput("ui_menu_module")
  ),
  uiOutput("main_shared_body")
)

server <- function(input, output, session) {

  # --- 1. REGISTRY (Paths only) ---
  tools <- list(
    "anova" = list(
      label = "One-Way ANOVA",
      file = "../Rs002_LM_QQ_FIX_ANOVA_01_1Way_s002/RShiny_modules.R",
      start_tab = "tab_config"
    ),
    "desc" = list(
      label = "Descriptives",
      file = "../Rs001_DESCRIPTIVE_1C_s001/RShiny_modules.R",
      start_tab = "tab_config"
    )
  )

  # Reactive States
  nav_state <- reactiveValues(origin = "fixed", tab = "fixed_data")
  active_tool <- reactiveVal(NULL)

  # Objetos para guardar información de selección
  tool_selection_time <- reactiveValues()           # Objeto POSIXct
  tool_formatted_time <- reactiveValues()          # String YYYY_MM_DD_HH_MM_SS
  tool_combined_string <- reactiveValues()         # String combinado
  temp_folder_path <- reactiveValues()             # Ruta de la carpeta temporal

  data_r <- reactive({
    req(input$dataset_sel)
    get(input$dataset_sel, "package:datasets")
  })

  # --- 2. DYNAMIC LOADING LOGIC ---

  observeEvent(input$btn_load, {
    target_id <- input$tool_choice

    # Obtener hora actual
    current_time <- Sys.time()

    # 1. Guardar hora POSIXct
    tool_selection_time[[target_id]] <- current_time

    # 2. Crear string con formato YYYY_MM_DD_HH_MM_SS
    formatted_time <- format(current_time, "%Y_%m_%d_%H_%M_%S")
    tool_formatted_time[[target_id]] <- formatted_time

    # 3. Crear string combinado: nombre_tool + "_" + hora_formateada
    combined_string <- paste0(target_id, "_", formatted_time)
    tool_combined_string[[target_id]] <- combined_string

    # 4. CREAR ESTRUCTURA DE CARPETAS TEMPORALES
    # Ruta original del archivo
    original_file_path <- tools[[target_id]]$file

    # Obtener directorio de la herramienta original
    original_dir <- dirname(normalizePath(original_file_path))

    # Crear carpeta temporal principal (solo una vez por sesión)
    main_temp_dir <- file.path(tempdir(), "Rscience_temp")
    if (!dir.exists(main_temp_dir)) {
      dir.create(main_temp_dir, recursive = TRUE)
    }

    # Crear subcarpeta con nombre combinado
    tool_temp_dir <- file.path(main_temp_dir, combined_string)

    # Limpiar si ya existe (por si acaso)
    if (dir.exists(tool_temp_dir)) {
      unlink(tool_temp_dir, recursive = TRUE)
    }

    # COPIAR TODO EL CONTENIDO DE LA CARPETA ORIGINAL
    cat("\n=== COPIA DE ARCHIVOS ===\n")
    cat("Carpeta original:", original_dir, "\n")
    cat("Carpeta temporal:", tool_temp_dir, "\n")

    # Copiar todos los archivos y subcarpetas
    # file.copy(from = original_dir,
    #           to = dirname(tool_temp_dir),
    #           recursive = TRUE,
    #           overwrite = TRUE)
    copy_tool_folder(original_file_path, tool_temp_dir)

    # Renombrar la carpeta copiada al nombre combinado
    copied_dir <- file.path(dirname(tool_temp_dir), basename(original_dir))
    if (dir.exists(copied_dir) && copied_dir != tool_temp_dir) {
      file.rename(copied_dir, tool_temp_dir)
    }

    # Guardar ruta temporal
    temp_folder_path[[target_id]] <- tool_temp_dir

    # 5. CARGAR DESDE LA CARPETA TEMPORAL
    # Construir ruta al archivo en la carpeta temporal
    temp_module_file <- file.path(tool_temp_dir, "RShiny_modules.R")

    cat("Archivo a cargar:", temp_module_file, "\n")
    cat("¿Existe el archivo?", file.exists(temp_module_file), "\n")
    cat("================================\n\n")

    # Verificar que el archivo existe
    if (!file.exists(temp_module_file)) {
      showNotification("Error: No se pudo copiar el archivo del módulo",
                       type = "error")
      return()
    }

    # Cargar el archivo desde la carpeta temporal
    source(temp_module_file, local = TRUE)

    # Guardamos las funciones actuales
    current_tool_funcs <- list(
      menu = module_ui_menu,
      body = module_ui_body,
      server = module_server
    )

    # Actualizamos la lista tools
    tools[[target_id]]$funcs <<- current_tool_funcs

    # Inicializamos el servidor del módulo
    current_tool_funcs$server(target_id, data_r, nav_state)

    # Cambiamos estado
    active_tool(target_id)
    nav_state$origin <- "module"
    nav_state$tab <- tools[[target_id]]$start_tab
    nav_select("menu_fixed", "clean")

    # Mostrar información del proceso
    showNotification(
      paste("Módulo cargado desde carpeta temporal:", basename(tool_temp_dir)),
      type = "message"
    )
  })

  # --- 3. NAVIGATION & RENDERING ---

  # Sincronización Sidebar -> Body
  observe({
    req(active_tool())
    module_menu_id <- paste0(active_tool(), "-menu_lateral")
    val <- input[[module_menu_id]]

    req(val, val != "clean")
    nav_state$origin <- "module"
    nav_state$tab <- val
    nav_select("menu_fixed", "clean")
  })

  observeEvent(input$menu_fixed, {
    req(input$menu_fixed != "clean")
    nav_state$origin <- "fixed"
    nav_state$tab <- input$menu_fixed

    if(!is.null(active_tool())) nav_select(paste0(active_tool(), "-menu_lateral"), "clean")
  })

  output$separator <- renderUI({ if(!is.null(active_tool())) hr() })

  output$ui_menu_module <- renderUI({
    req(active_tool())
    tools[[active_tool()]]$funcs$menu(active_tool())
  })

  output$main_shared_body <- renderUI({
    if (nav_state$origin == "fixed") {
      if (nav_state$tab == "fixed_data") {
        card(selectInput("dataset_sel", "Dataset:", choices = c("iris", "mtcars")), tableOutput("preview"))
      } else {
        choices <- setNames(names(tools), sapply(tools, `[[`, "label"))

        # Convertir reactiveValues a listas para mostrar
        time_list <- reactiveValuesToList(tool_selection_time)
        formatted_list <- reactiveValuesToList(tool_formatted_time)
        combined_list <- reactiveValuesToList(tool_combined_string)
        temp_paths <- reactiveValuesToList(temp_folder_path)

        card(
          radioButtons("tool_choice", "Choose Tool:", choices = choices),
          actionButton("btn_load", "Load Module", class="btn-primary w-100"),

          # Mostrar información de selección (solo si hay datos)
          if(length(time_list) > 0) {
            card(
              card_header("Registro de Selecciones"),
              tags$ul(
                lapply(names(time_list), function(tool_name) {
                  tags$li(
                    tags$strong(tools[[tool_name]]$label, ":"),
                    tags$br(),
                    "• ID: ", combined_list[[tool_name]],
                    tags$br(),
                    "• Carpeta: ", if(!is.null(temp_paths[[tool_name]]))
                      basename(temp_paths[[tool_name]]) else "No creada"
                  )
                })
              )
            )
          }
        )
      }
    } else {
      req(active_tool())
      tools[[active_tool()]]$funcs$body(active_tool(), data_r())
    }
  })

  output$preview <- renderTable({ head(data_r(), 5) })

  # Observer para acceder a los datos desde otros lugares
  observe({
    tool <- active_tool()
    if(!is.null(tool)) {
      cat("\n--- DATOS ACTUALES ---\n")
      cat("Herramienta activa:", tool, "\n")
      cat("String combinado:", tool_combined_string[[tool]], "\n")
      cat("Carpeta temporal:", temp_folder_path[[tool]], "\n")
    }
  })

  # Limpiar carpetas temporales al cerrar la app
  session$onSessionEnded(function() {
    main_temp_dir <- file.path(tempdir(), "Rscience_temp")
    if (dir.exists(main_temp_dir)) {
      cat("\nLimpiando carpetas temporales...\n")
      unlink(main_temp_dir, recursive = TRUE)
    }
  })
}

shinyApp(ui, server)
