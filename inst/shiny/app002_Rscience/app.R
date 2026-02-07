# --- 1. CARGA DE RECURSOS ---
source(file = "global.R")

# --- 2. UI: APP PRINCIPAL ---
# --- 2. UI: APP PRINCIPAL ---
ui <- bslib::page_navbar(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50"
  ),
  title = "R-Science App",
  fillable = TRUE, # Recomendado para layouts modernos
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .nav-link[data-value='tab_HIDDEN_MENU01'] { display: none !important; }
        .not-allowed { cursor: not-allowed !important; }
      "))
    )
  ),

  # --- CAMBIO AQUÍ: Envuelve el layout en un nav_panel o usa el argumento 'sidebar' correctamente ---
  sidebar = bslib::sidebar(
    width = 250,
    div(
      id = "all_all_all",
      style = "padding: 10px; background: rgba(0,0,0,0.05); border-radius: 8px; margin-bottom: 20px;",
      shinyWidgets::materialSwitch(
        inputId = "debug_mode",
        label = tags$b("Engineer View"),
        value = FALSE, #TRUE,
        status = "danger"
      )
    ),
    bslib::navset_pill_list(
      id = "main_switcher",
      well = FALSE,
      widths = c(12, 12),
      bslib::nav_panel(title = "01. Data Import", value = "panel_import", icon = icon("database")),
      bslib::nav_panel(title = "02. Tools",       value = "panel_tools",  icon = icon("chart-line")),
      bslib::nav_panel("HIDDEN_MENU01", value = "tab_HIDDEN_MENU01"),
      bslib::nav_item(div(id = "debug_sep_1", tags$hr(style = "margin: 10px 0; border-top: 1px solid #000;"))),
      bslib::nav_item(
        span(id = "debug_title", "ENGINEERING DIAGNOSTICS",
             style = "font-size: 0.85rem; font-weight: bold; color: #dc3545; padding: 10px 15px; display: block; letter-spacing: 1px;")
      ),
      bslib::nav_panel(title = span(icon("bullseye"), "H01. Central", style = "color: #fd7e14; font-weight: bold;"), value = "panel_ORH01_central"),
      bslib::nav_panel(title = span(icon("bullseye"), "H02. Temporal FF", style = "color: #fd7e14; font-weight: bold;"), value = "panel_ORH02_temporal_FF"),
      bslib::nav_panel(title = span(icon("bullseye"), "H03. Module Loading", style = "color: #fd7e14; font-weight: bold;"), value = "panel_ORH03_module_loading"),
      bslib::nav_item(div(id = "debug_sep_2", tags$hr(style = "margin: 10px 0; border-top: 1px solid #000;")))
    ),
    uiOutput("render_auxiliar_tool_menu")
  ),

  # El cuerpo principal de la App
  bslib::nav_panel(
    title = "Main", # Este título no se verá si usas sidebar pero es necesario para la estructura
    div(
      id = "main_body_container",
      conditionalPanel("input.main_switcher == 'panel_import'", mod_import_hub_ui("main_import")),
      conditionalPanel("input.main_switcher == 'panel_tools'",  mod_tools_hub_ui("main_tools")),
      conditionalPanel("input.main_switcher == 'panel_ORH01_central'", uiOutput("ui_diagnostic_ORH01_central")),
      conditionalPanel("input.main_switcher == 'panel_ORH02_temporal_FF'", uiOutput("ui_diagnostic_ORH02_temporal_FF")),
      conditionalPanel("input.main_switcher == 'panel_ORH03_module_loading'", uiOutput("ui_diagnostic_ORH03_module_loading")),
      conditionalPanel("input.main_switcher == 'tab_HIDDEN_MENU01'", uiOutput("render_tool_body"))
    )
  )
)

# --- 3. SERVER: LÓGICA DE LA APP ---
server <- function(input, output, session) {

  observe({shinyjs::hide("all_all_all")})
  # --- LÓGICA DE VISIBILIDAD DEBUG (Engineer View) ---
  observeEvent(input$debug_mode, {


    # Lista de valores (values) de los nav_panels que quieres controlar
    debug_panels <- c(
      "panel_ORH01_central",
      "panel_ORH02_temporal_FF",
      "panel_ORH03_module_loading"
    )

    if (isTRUE(input$debug_mode)) {
      # Mostrar paneles y separadores
      lapply(debug_panels, function(x) shinyjs::show(selector = paste0(".nav-link[data-value='", x, "']")))
      # shinyjs::show("debug_sep_1")
      shinyjs::show("debug_sep_2")
      shinyjs::show("debug_title") # <--- AGREGAR ESTA LÍNEA
    } else {
      # Ocultar paneles y separadores
      lapply(debug_panels, function(x) shinyjs::hide(selector = paste0(".nav-link[data-value='", x, "']")))
      # shinyjs::hide("debug_sep_1")
      shinyjs::hide("debug_sep_2")
      shinyjs::hide("debug_title") # <--- AGREGAR ESTA LÍNEA

      # Si el usuario está parado en un panel de debug y lo apaga, lo pateamos a 'panel_import'
      if (input$main_switcher %in% debug_panels) {
        bslib::nav_select("main_switcher", selected = "panel_import")
      }
    }
  })

  # --- MENU SYNC LOGIC (Alternation) ---
  # A. Si el usuario usa el Menú Maestro (Import, Tools, Debug)
  observeEvent(input$main_switcher, {
    # Solo reseteamos el menú del módulo si NO estamos en la pestaña de ejecución
    if(input$main_switcher != "tab_HIDDEN_MENU01") {
      # Usamos el namespace "active_tool" para llegar al input del módulo
      bslib::nav_select("active_tool-menu_lateral", selected = "tab_HIDDEN_lateral")
    }
  }, ignoreInit = TRUE)

  # B. Si el usuario usa el Menú de la Herramienta (Módulo)
  observeEvent(input[["active_tool-menu_lateral"]], {
    val <- input[["active_tool-menu_lateral"]]
    req(val) # Evita disparos accidentales cuando el menú se está cargando

    # Si selecciona cualquier pestaña que NO sea la de reseteo ("tab_HIDDEN_lateral")
    if(val != "tab_HIDDEN_lateral") {
      # Forzamos al Menú Maestro a irse a la pestaña que renderiza el cuerpo del módulo
      if(input$main_switcher != "tab_HIDDEN_MENU01") {
        bslib::nav_select("main_switcher", selected = "tab_HIDDEN_MENU01")
      }
    }
  }, ignoreInit = TRUE)


  BigBang <- reactive(TRUE)
  status_toggle <- reactive({ isTRUE(input$debug_mode) })
  default_output_list <- list(ready = FALSE, description_short = "Waiting...", diff_secs = 0)
  # Módulos
  RO_01_import_dataset <- mod_import_hub_server(id = "main_import", check_external = BigBang, debug_toggle = status_toggle)
  RO_02_tools <- mod_tools_hub_server(id = "main_tools", config_path = "local_resources/f02_tools/fn02_menues/super_menu01.yml", check_external = BigBang, debug_toggle = status_toggle)

  # --- FASE 03: CENTRAL ---
  ORH_01_CENTRAL <- reactive({
    init_time <- lubridate::now()
    bundle_import <- RO_01_import_dataset()
    bundle_tools  <- RO_02_tools()
    is_ready_import <- is.list(bundle_import) && isTRUE(bundle_import$ready)
    is_ready_tools  <- is.list(bundle_tools) && isTRUE(bundle_tools$ready)
    is_ready_all    <- all(is_ready_import, is_ready_tools)
    end_time <- lubridate::now()

    list(
      "ready" = is_ready_all,
      "init_time" = init_time, "end_time" = end_time,
      "diff_secs" = as.numeric(end_time - init_time, units = "secs"),
      "previous_steps" = list("ready_import" = is_ready_import, "ready_tools" = is_ready_tools)
    )
  })

  output$ORH_01_CENTRAL_debug_dashboard <- renderUI({
    data <- ORH_01_CENTRAL()
    req(data)
    # (Tu lógica de status_row y banner se mantiene igual...)
    overall_ok <- isTRUE(data$ready)
    banner_bg  <- if(overall_ok) "#d1e7dd" else "#f8d7da"
    banner_txt <- if(overall_ok) "✅ GATE OPEN" else "🔒 GATE LOCKED"

    tagList(
      div(style = paste0("background-color:", banner_bg, "; padding: 20px; border-radius: 12px; text-align: center;"), tags$h3(banner_txt)),
      bslib::card(card_header("Prerequisites"), p("Import: ", as.character(data$previous_steps$ready_import)), p("Tools: ", as.character(data$previous_steps$ready_tools)))
    )
  })

  output$ui_diagnostic_ORH01_central <- renderUI({
    bslib::navset_card_tab(
      title = "Diagnostics: Central",
      bslib::nav_panel("Dashboard", uiOutput("ORH_01_CENTRAL_debug_dashboard")),
      bslib::nav_panel("Raw", verbatimTextOutput("ORH_01_CENTRAL_debug_verbatim"))
    )
  })
  output$ORH_01_CENTRAL_debug_verbatim <- renderPrint({ ORH_01_CENTRAL() })

  # --- FASE 04: TEMPORAL FF ---


  ORH_02_temporal_FF <- reactive({

    init_time <- lubridate::now()

    central_status <- ORH_01_CENTRAL()
    req(central_status)
    if (!isTRUE(central_status$ready)) return(default_output_list)
    print("A")

    internal_tools_bundle <- RO_02_tools()
    if (!isTRUE(internal_tools_bundle$ready)) return(default_output_list)
    print("B")

    # Construcción de rutas (simplificado para brevedad)
    str_ALL_scripts_root_INFO      <- "shiny/app002_Rscience/local_resources/f02_tools/fn01_scripts"
    str_SELECTED_script_subfolder  <- internal_tools_bundle$script
    str_MODULE_filename            <- "RShiny_modules.R"

    str_ALL_scripts_path_LOCAL <- system.file(str_ALL_scripts_root_INFO, package = "Rscience3")
    str_SELECTED_folder_path_LOCAL <- file.path(str_ALL_scripts_path_LOCAL, str_SELECTED_script_subfolder)
    str_SELECTED_module_path_LOCAL <- file.path(str_SELECTED_folder_path_LOCAL, str_MODULE_filename)

    check_script_folder <- dir.exists(str_SELECTED_folder_path_LOCAL)
    check_file_module <-  file.exists(str_SELECTED_module_path_LOCAL)

    local_ready <- check_script_folder && check_file_module
    print(check_script_folder)
    print(check_file_module)
    print(local_ready)
    tool_execution_env <- new.env(parent = .GlobalEnv)
    Rscience_temp_path <- file.path(tempdir(), paste0("Rscience_", format(now(), "%Y%m%d_%H%M%S")))

    is_copied <- FALSE
    if(local_ready) {
      fs::dir_create(Rscience_temp_path)
      fs::dir_copy(path = str_SELECTED_folder_path_LOCAL, new_path = file.path(Rscience_temp_path, basename(str_SELECTED_folder_path_LOCAL)), overwrite = TRUE)
      is_copied <- TRUE
    }

    end_time <- lubridate::now()
    list(
      ready = is_copied, init_time = init_time, end_time = end_time,
      diff_secs = as.numeric(end_time - init_time, units = "secs"),
      tool_execution_env = tool_execution_env,
      local = list(folder = list(str_path = str_SELECTED_folder_path_LOCAL, check_exists = local_ready),
                   module_file = list(str_path = str_SELECTED_module_path_LOCAL, check_exists = local_ready)),
      temp_basics = list(folder = list(str_path = Rscience_temp_path, check_exists = dir.exists(Rscience_temp_path))),
      temp_copying_FF = list(
        temp_script_folder = list(str_path = file.path(Rscience_temp_path, str_SELECTED_script_subfolder), check_exists = is_copied),
        temp_module_file = list(str_path = file.path(Rscience_temp_path, str_SELECTED_script_subfolder, str_MODULE_filename), check_exists = is_copied)
      )
    )
  })

  output$ui_diagnostic_ORH02_temporal_FF <- renderUI({
    bslib::navset_card_tab(
      title = "Temporal FF",
      bslib::nav_panel("Dashboard", uiOutput("ORH_02_temporal_FF_debug_dashboard")),
      bslib::nav_panel("Raw", verbatimTextOutput("ORH_02_temporal_FF_debug_verbatim"))
    )
  })
  # --- FASE 04: TEMPORAL FF (Actualización de Dashboard) ---

  output$ORH_02_temporal_FF_debug_dashboard <- renderUI({
    data <- ORH_02_temporal_FF()

    # Lógica de bloqueo si no hay datos o no está listo
    if (is.null(data) || !isTRUE(data$ready)) {
      return(
        div(style = "background-color: #f8d7da; padding: 40px; border-radius: 12px; text-align: center; border: 1px solid #f5c2c7;",
            tags$h1(icon("lock"), style = "font-size: 3rem; color: #842029;"),
            tags$h3("🔒 GATE LOCKED", style = "color: #842029; font-weight: bold;"),
            tags$p("Waiting for Phase 01 & 02 (Import & Tools Selection) to be completed.", style = "color: #842029;")
        )
      )
    }

    overall_ok <- isTRUE(data$ready)
    status_color <- if(overall_ok) "#198754" else "#dc3545"
    status_bg    <- if(overall_ok) "#eafaf1" else "#fdf2f2"

    file_info_row <- function(label, path, exists) {
      tags$div(style = "margin-bottom: 10px; padding: 8px; border-radius: 4px; background: white; border: 1px solid #eee;",
               tags$div(style = "display: flex; justify-content: space-between;",
                        tags$b(label),
                        tags$span(style = paste0("color:", if(exists) "#198754" else "#dc3545"),
                                  if(exists) icon("check-circle") else icon("times-circle"))
               ),
               tags$code(style = "display: block; font-size: 0.8em; color: #666; word-break: break-all;",
                         if(is.null(path) || path == "") "Not defined" else path)
      )
    }

    tagList(
      tags$div(style = paste0("background-color:", status_bg, "; color:", status_color,
                              "; padding: 15px; border-radius: 8px; border: 1px solid ", status_color, "; margin-bottom: 20px;"),
               tags$h4(style = "margin:0;", "✅ Files Synchronized"),
               tags$small(paste("Execution time:", round(data$diff_secs, 4), "seconds"))
      ),
      fluidRow(
        column(6, bslib::card(card_header("Source: Package Assets"),
                              file_info_row("Local Folder", data$local$folder$str_path, data$local$folder$check_exists),
                              file_info_row("Module R File", data$local$module_file$str_path, data$local$module_file$check_exists))),
        column(6, bslib::card(card_header("Destination: Temp Session"),
                              file_info_row("Session Root", data$temp_basics$folder$str_path, data$temp_basics$folder$check_exists),
                              file_info_row("Copied Script", data$temp_copying_FF$temp_module_file$str_path, data$temp_copying_FF$temp_module_file$check_exists)))
      )
    )
  })
  output$ORH_02_temporal_FF_debug_verbatim <- renderPrint({ ORH_02_temporal_FF() })

  # --- FASE 05: MODULE LOADING ---
  ORH_03_module_loading <- reactive({
    data_ff <- ORH_02_temporal_FF()
    if (!isTRUE(data_ff$ready)) {
      return(list(ready = FALSE, description = "Waiting for Phase 02 (Files)..."))
    }

    init_time <- lubridate::now()
    tool_env <- data_ff$tool_execution_env
    path_to_source <- data_ff$temp_copying_FF$temp_module_file$str_path

    # Intentar cargar el código
    load_status <- tryCatch({
      source(path_to_source, local = tool_env)
      list(success = TRUE, error = NULL)
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })

    if (!load_status$success) {
      return(list(
        ready = FALSE,
        error_msg = load_status$error,
        init_time = init_time,
        diff_secs = as.numeric(lubridate::now() - init_time)
      ))
    }

    # Verificar funciones requeridas
    required_fns <- c("module_ui_menu", "module_ui_body", "module_server")
    check_exists <- sapply(required_fns, function(x) exists(x, envir = tool_env, inherits = FALSE))

    list(
      ready = all(check_exists),
      init_time = init_time,
      diff_secs = as.numeric(lubridate::now() - init_time),
      all_fn_exists = check_exists,
      # Extraemos las funciones solo si existen
      menu_fn   = if(check_exists["module_ui_menu"]) get("module_ui_menu", envir = tool_env) else NULL,
      body_fn   = if(check_exists["module_ui_body"]) get("module_ui_body", envir = tool_env) else NULL,
      server_fn = if(check_exists["module_server"]) get("module_server", envir = tool_env) else NULL
    )
  })


  output$ORH_03_module_loading_dashboard <- renderUI({
    data <- ORH_03_module_loading()

    # Lógica de bloqueo si no está listo o hay error de espera
    if (is.null(data) || !isTRUE(data$ready)) {
      # Si hay un error de sintaxis real, lo mostramos; si no, mostramos el candado de "espera"
      if (!is.null(data$error_msg)) {
        return(bslib::card(card_header("R Source Error", class = "bg-danger text-white"),
                           tags$pre(style = "color: #d63384; padding: 10px;", data$error_msg)))
      }

      return(
        div(style = "background-color: #f8d7da; padding: 40px; border-radius: 12px; text-align: center; border: 1px solid #f5c2c7;",
            tags$h1(icon("lock"), style = "font-size: 3rem; color: #842029;"),
            tags$h3("🔒 GATE LOCKED", style = "color: #842029; font-weight: bold;"),
            tags$p("Waiting for Phase 04 (File Synchronization) to finish successfully.", style = "color: #842029;")
        )
      )
    }

    overall_ok <- isTRUE(data$ready)
    status_color <- if(overall_ok) "#0d6efd" else "#dc3545"
    status_bg    <- if(overall_ok) "#f0f7ff" else "#fff5f5"

    fn_row <- function(name, exists) {
      tags$div(style = "display: flex; align-items: center; justify-content: space-between; padding: 12px; border-bottom: 1px solid #eee;",
               tags$code(style = paste0("font-size: 1.1em; color:", if(exists) "#2c3e50" else "#ccc"), name),
               if(exists) tags$span(class = "badge bg-success", "LOADED") else tags$span(class = "badge bg-danger", "MISSING")
      )
    }

    tagList(
      tags$div(style = paste0("background-color:", status_bg, "; color:", status_color,
                              "; padding: 20px; border-radius: 10px; border: 2px solid ", status_color, "; margin-bottom: 20px;"),
               tags$h4(style = "margin:0; font-weight: bold;", "🚀 DISPATCHER: MODULE FULLY LOADED"),
               tags$small(paste("Source time:", round(data$diff_secs, 5), "seconds"))
      ),
      bslib::card(card_header("Function Map (Agnostic Interface)"),
                  fn_row("module_ui_menu",   data$all_fn_exists["module_ui_menu"]),
                  fn_row("module_ui_body",   data$all_fn_exists["module_ui_body"]),
                  fn_row("module_server",    data$all_fn_exists["module_server"]))
    )
  })
  output$ORH_03_module_loading_verbatim <- renderPrint({ ORH_03_module_loading() })
  output$ui_diagnostic_ORH03_module_loading <- renderUI({
    bslib::navset_card_tab(
      title = "Module Loading",
      bslib::nav_panel("Dashboard", uiOutput("ORH_03_module_loading_dashboard")),
      bslib::nav_panel("Raw", verbatimTextOutput("ORH_03_module_loading_verbatim"))
    )
  })
  # --- OUTPUTS DE HERRAMIENTA ---
  output$render_auxiliar_tool_menu <- renderUI({
    res <- ORH_03_module_loading()
    req(res$ready); res$menu_fn("active_tool")
  })

  output$render_tool_body <- renderUI({
    res <- ORH_03_module_loading()
    if(!isTRUE(res$ready)) return(card("Step 04/05 Pending"))
    res$body_fn("active_tool")
  })

  observe({
    res <- ORH_03_module_loading()
    req(res$ready)
    res$server_fn(id = "active_tool",
                  OR_01_import_dataset = RO_01_import_dataset,
                  ORH_02_temporal_FF = ORH_02_temporal_FF,
                  debug_toggle = status_toggle)
  })
}

shinyApp(ui, server)
