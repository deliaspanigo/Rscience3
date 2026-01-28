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

SHOW_DEBUG <- TRUE
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
      card(card_header("step 01 - Import - Debug"), verbatimTextOutput("debug_verbatim_01_import_DEBUG"))
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_tools'",
      module_tool_selector_ui("master_tools")
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_tools_DEBUG'",
      card(card_header("step 02 - Tools - Debug"), verbatimTextOutput("debug_verbatim_02_tools_DEBUG"))
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


  # --- PHASE 02: TOOL SELECTION ---  ------------------------------------------
  OR_02_tools <- module_tool_selector_server("master_tools", "tools_config_PROD.yml")
  output$debug_verbatim_02_tools_DEBUG <- renderPrint({
    OR_02_tools()
  })


  # --- PHASE 03: STATE CENTRALIZER (Gatekeeper) --- ---------------------------
  OR_03_CENTRAL_is_done_import_and_tools <- reactive({
    is_done_import <- isTRUE(OR_01_import_dataset()$is_done)
    is_done_tools  <- isTRUE(OR_02_tools()$is_done)
    is_done_all    <- all(is_done_import, is_done_tools)

    list_output <- list(
      "is_done" = is_done_all,
      "description_short" = "Central Point.",
      previous_steps = list(
        "is_done_import" = is_done_import,
        "is_done_tools" = is_done_tools)
      )
    return(list_output)
  })
  output$debug_verbatim_03_is_done_all_DEBUG <- renderPrint({
    OR_03_CENTRAL_is_done_import_and_tools()
  })
  output$debug_status_03_dashboard <- renderUI({
    # 1. Validación de entrada (Guardrail)
    data <- OR_03_CENTRAL_is_done_import_and_tools()
    if (is.null(data) || !is.list(data)) {
      return(shiny::tags$p(shiny::tags$em("Waiting for reactive data...")))
    }

    # 2. Funciones auxiliares para filas
    # Usamos una estructura de fila con icono y texto para mayor claridad
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

    # 3. Lógica del Banner Superior (Overall)
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

      # NOTA INFORMATIVA
      shiny::tags$p(
        style = "margin-top: 15px; font-size: 0.9em; color: #666;",
        shiny::tags$em("Note: Both prerequisites must be TRUE for the Central Gate to open.")
      )
    )
  })


  # --- PHASE 04: TEMPORAL FF and ENV (Files and Folders) --- --------------------------
  OR_04_temporal_FF <- reactive({

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

    list_output <- list(
      is_done = TRUE,
      description_short = "Temporal FF.",
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
    # --- 1. VALIDACIÓN DE ENTRADA ---
    data <- OR_04_temporal_FF()
    if (is.null(data) || !is.list(data) || length(data) == 0) {
      return(shiny::tags$p(shiny::tags$em("Waiting for Step 03 to complete...")))
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
    # Definimos el banner superior según el estado global
    overall_ok <- base::isTRUE(data$is_done)
    banner_bg    <- base::ifelse(overall_ok, "#d4edda", "#f8d7da")
    banner_color <- base::ifelse(overall_ok, "#155724", "#721c24")
    banner_text  <- base::ifelse(overall_ok, "✅ STEP 04: ALL FILES READY", "❌ STEP 04: SYNCHRONIZATION ERROR")

    shiny::tagList(
      # --- BANNER DE ESTADO GLOBAL (AL INICIO) ---
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
      )
    )
  })


  # --- PHASE 05: MODULE EXTRACTION (Agnostic Dispatcher) ---
  OR_05_module_loading <- reactive({
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
    list(
      is_done = TRUE,
      description_short = "Loading selected module.",
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
    # 1. Validación de entrada
    data <- OR_05_module_loading()
    if (is.null(data) || length(data) <= 1) {
      return(shiny::tags$p(shiny::tags$em("Waiting for Step 04 to complete...")))
    }

    # 2. Función para filas de objetos
    fn_row <- function(fn_name, exists) {
      is_ok <- base::isTRUE(exists)
      # Usamos Check Verde o X Roja directa
      status_symbol <- base::ifelse(is_ok, "✅", "❌")

      shiny::tags$div(
        style = "display: flex; align-items: center; margin-bottom: 8px; padding: 10px; border-bottom: 1px solid #f0f0f0;",
        # Símbolo de estado
        shiny::span(style = "margin-right: 15px; font-size: 1.2em;", status_symbol),

        # Nombre del objeto (Color Azul Acero Profesional)
        shiny::tags$code(style = "width: 180px; font-weight: bold; color: #2c3e50; font-size: 1.1em; background: none;",
                         fn_name),

        # Estado en texto (Gris neutro)
        shiny::tags$span(style = "color: #7f8c8d; font-style: italic;",
                         base::ifelse(is_ok, "Object found in Environment", "Object NOT found"))
      )
    }

    # 3. Lógica del Banner (Colores de fondo suaves, texto neutro)
    overall_ok   <- base::isTRUE(data$is_done)
    banner_bg    <- base::ifelse(overall_ok, "#ebf5fb", "#fef9e7") # Azul muy claro o Amarillo muy claro
    banner_color <- "#2c3e50" # Siempre el mismo color de texto oscuro para legibilidad
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

    list_is_done <- list()
    internal_OR_01_import_dataset <- OR_01_import_dataset()
    list_is_done[[1]] <- list(internal_OR_01_import_dataset$is_done,
                              internal_OR_01_import_dataset$description_short)


    internal_OR_02_tools <- OR_02_tools()
    list_is_done[[2]] <- list(internal_OR_02_tools$is_done,
                              internal_OR_02_tools$description_short)

    internal_OR_03_CENTRAL_is_done_import_and_tools <- OR_03_CENTRAL_is_done_import_and_tools()
    list_is_done[[3]] <- list(internal_OR_03_CENTRAL_is_done_import_and_tools$is_done,
                              internal_OR_03_CENTRAL_is_done_import_and_tools$description_short)

    internal_OR_04_temporal_FF <- OR_04_temporal_FF()
    list_is_done[[4]] <- list(internal_OR_04_temporal_FF$is_done,
                              internal_OR_04_temporal_FF$description_short)

    internal_OR_05_module_loading <- OR_05_module_loading()
    list_is_done[[5]] <- list(internal_OR_05_module_loading$is_done,
                              internal_OR_05_module_loading$description_short)


    df_summary <- purrr::map_dfr(base::seq_along(list_is_done), function(i) {
      item <- list_is_done[[i]]

      # FIX: Validamos que el contenido no sea NULL o vacío para evitar el error de "1, 0 rows"
      safe_status <- base::ifelse(base::is.null(item[[1]]), FALSE, item[[1]])
      safe_desc   <- base::ifelse(base::is.null(item[[2]]) || base::length(item[[2]]) == 0, "No desc", item[[2]])

      base::data.frame(
        step_num    = i,
        status_icon = base::ifelse(base::isTRUE(safe_status), "✅", "❌"),
        description = base::as.character(safe_desc),
        stringsAsFactors = FALSE
      )
    })
    return(df_summary)
  })
  output$debug_verbatim_99_super_summary <- renderPrint({
    OR_99_super_summary()
  })
  output$debug_status_99_dashboard <- renderUI({
    # 1. Obtener los datos del reactivo
    df_data <- OR_99_super_summary()

    # Validación de seguridad: si no hay datos o falla el reactivo
    if (base::is.null(df_data) || base::nrow(df_data) == 0) {
      return(shiny::tags$p(shiny::tags$em("Waiting for system initialization...")))
    }

    # 2. Función interna para construir cada fila del Roadmap
    summary_row <- function(num, icon, desc) {
      is_ok <- (icon == "✅")
      # Fondo ligeramente rojizo si el paso falló o está pendiente
      bg_color <- base::ifelse(is_ok, "#ffffff", "#fffcfc")

      shiny::tags$div(
        style = base::paste0("display: flex; align-items: center; padding: 12px; border-bottom: 1px solid #edf2f7; background-color: ", bg_color, ";"),

        # Círculo con el número de paso
        shiny::tags$div(
          style = "width: 28px; height: 28px; border-radius: 50%; background: #2c3e50; color: white;
                 display: flex; align-items: center; justify-content: center; font-size: 0.75em; margin-right: 15px; flex-shrink: 0;",
          num
        ),

        # Icono de estado (Emoji)
        shiny::tags$span(style = "margin-right: 15px; font-size: 1.2em;", icon),

        # Descripción del paso
        shiny::tags$div(
          style = "flex-grow: 1;",
          shiny::tags$span(style = "color: #2c3e50; font-weight: 500; font-size: 0.95em;", desc)
        ),

        # Etiqueta de texto (Badge)
        shiny::tags$span(
          style = base::paste0("font-size: 0.7em; padding: 3px 10px; border-radius: 10px; font-weight: bold; ",
                               base::ifelse(is_ok, "background: #e6fffa; color: #234e52; border: 1px solid #b2f5ea;",
                                            "background: #fff5f5; color: #822727; border: 1px solid #feb2b2;")),
          base::ifelse(is_ok, "READY", "WAITING")
        )
      )
    }

    # 3. Cálculo de progreso para la barra superior
    total_steps <- base::nrow(df_data)
    ready_steps <- base::sum(df_data$status_icon == "✅")
    progress_pct <- base::round((ready_steps / total_steps) * 100)

    # 4. Construcción final del UI
    shiny::tagList(
      # --- BARRA DE PROGRESO ---
      shiny::tags$div(
        style = "margin-bottom: 25px; background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
        shiny::tags$div(
          style = "display: flex; justify-content: space-between; margin-bottom: 8px; font-size: 0.9em; font-weight: bold; color: #2c3e50;",
          shiny::tags$span("System Activation Progress"),
          shiny::tags$span(base::paste0(progress_pct, "%"))
        ),
        shiny::tags$div(
          style = "width: 100%; background-color: #e9ecef; border-radius: 10px; height: 10px; overflow: hidden;",
          shiny::tags$div(
            style = base::paste0("width: ", progress_pct, "%; background-color: #2c3e50; height: 100%; transition: width 0.5s ease-in-out;")
          )
        )
      ),

      # --- ROADMAP CONTAINER ---
      shiny::tags$h5(style = "color: #2c3e50; margin-bottom: 12px; font-weight: bold; padding-left: 5px;",
                     "Execution Roadmap Details"),

      shiny::tags$div(
        style = "border: 1px solid #dcdde1; border-radius: 8px; overflow: hidden; background: white; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1);",
        # Aquí usamos seq_len para evitar el error de sintaxis del operador ':'
        base::lapply(base::seq_len(base::nrow(df_data)), function(i) {
          summary_row(
            num  = df_data$step_num[i],
            icon = df_data$status_icon[i],
            desc = df_data$description[i]
          )
        })
      ),

      # --- FOOTER ---
      shiny::tags$p(
        style = "margin-top: 15px; font-size: 0.8em; color: #a0aec0; text-align: right; font-style: italic;",
        base::paste("Last sync:", base::format(base::Sys.time(), "%H:%M:%S"))
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
