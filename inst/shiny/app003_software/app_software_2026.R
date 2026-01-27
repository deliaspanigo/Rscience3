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

      if(SHOW_DEBUG) nav_panel("3.1. Temporal FF Debug", value = "tab_temporal_FF_DEBUG"),
      if(SHOW_DEBUG) nav_panel("4.1. Loading FF Debug", value = "tab_loading_FF_DEBUG"),

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
      card(card_header("Step 03 - Is done all - Debug"), verbatimTextOutput("debug_verbatim_03_is_done_all_DEBUG"))
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_temporal_FF_DEBUG'",
      card(card_header("Step 04 - Temporal FF - Debug"), verbatimTextOutput("debug_verbatim_04_temporal_FF_DEBUG"))
    ),

    conditionalPanel(
      condition = "input.menu_fixed == 'tab_loading_FF_DEBUG'",
      card(card_header("Loading FF Debug"), verbatimTextOutput("debug_verbatim_04_loading_FF_DEBUG"))
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


  # --- PHASE 01: IMPORTATION ---
  OR_01_import_dataset <- module_orchestrator_01_import_dataset_server("master_import")

  output$debug_verbatim_01_import_DEBUG <- renderPrint({
    str(OR_01_import_dataset())
  })


  # --- PHASE 02: TOOL SELECTION ---
  OR_02_tools <- module_tool_selector_server("master_tools", "tools_config_PROD.yml")

  output$debug_verbatim_02_tools_DEBUG <- renderPrint({
    OR_02_tools()
  })


  # --- PHASE 03: STATE CENTRALIZER (Gatekeeper) ---
  OR_03_CENTRAL_is_done_import_and_tools <- reactive({
    is_done_import <- isTRUE(OR_01_import_dataset()$is_done)
    is_done_tools  <- isTRUE(OR_02_tools()$is_done)
    is_done_all    <- all(is_done_import, is_done_tools)

    list_output <- list(
      "is_done_import" = is_done_import,
      "is_done_tools" = is_done_tools,
      "is_done_all" = is_done_all
    )
    list_output
  })

  output$debug_verbatim_03_is_done_all_DEBUG <- renderPrint({
    OR_03_CENTRAL_is_done_import_and_tools()
  })

  # --- PHASE 04: TEMPORAL FF (Files and Folders) ---
  OR_04_temporal_FF <- reactive({
    # 1. Capture reactive snapshots at the very beginning
    # This "freezes" the values for this execution cycle
    previous_state_pack <- OR_03_CENTRAL_is_done_import_and_tools()
    selected_tool_data  <- OR_02_tools()

    # 2. Extract static values from the local objects
    is_previous_step_ready <- previous_state_pack$is_done_all
    # req(is_previous_step_ready)
    print(is_previous_step_ready)
    # 3. Guardrail: Exit early if dependencies are not met
    if (!isTRUE(is_previous_step_ready)) {
      return(default_output_list)
    }

    # 4. Preparation of file paths using local variables
    # Using file.path is safer for cross-platform compatibility
    base_directory <- ".."
    tool_script_path <- file.path(base_directory, selected_tool_data$special_path)

    # 5. Sandbox Creation: Initialize a clean environment
    # Setting parent = .GlobalEnv ensures access to loaded libraries
    tool_execution_env <- new.env(parent = .GlobalEnv)

    # 6. Tool Loading Execution
    tryCatch({
      # Source the external script into the isolated environment
      source(tool_script_path, local = tool_execution_env)

      # Return a success bundle
      list(
        is_done   = TRUE,
        tool_env  = tool_execution_env,
        timestamp = Sys.time()
      )

    }, error = function(e) {
      # User-facing notification for debugging
      showNotification(
        ui = HTML(paste("Step 04 <br>Critical Error loading tool logic:", e$message)),
        type = "error"
      )

      # Return a failure bundle
      return(default_output_list)
    })
  })

  output$debug_verbatim_04_temporal_FF_DEBUG <- renderPrint({
    OR_04_temporal_FF()
  })


  # --- PHASE 04: MODULE EXTRACTION (Agnostic Dispatcher) ---
  OR_04_module_loading <- reactive({
    phase3 <- OR_04_temporal_FF()
    if (!isTRUE(phase3$is_done)) return(list(is_done = FALSE))

    # Look for functions INSIDE the tool's private environment
    env <- phase3$tool_env

    required <- c("module_ui_menu", "module_ui_body", "module_server")
    check_exists <- sapply(required, exists, envir = env)

    if(!all(check_exists)) {
      return(list(is_done = FALSE, error = "Functions not found in tool file"))
    }

    list(
      is_done = TRUE,
      menu    = get("module_ui_menu", envir = env),
      body    = get("module_ui_body", envir = env),
      server  = get("module_server",  envir = env)
    )
  })

  output$debug_verbatim_04_loading_FF_DEBUG <- renderPrint({
    OR_04_module_loading()
  })


  # --- PHASE 05: DYNAMIC RENDERING ---

  # 1. Render Lateral Menu (Tool)
  output$render_tool_menu <- renderUI({
    res <- OR_04_module_loading()
    req(res$is_done)
    res$menu("active_tool")
  })

  # 2. Render Body (Activated via conditionalPanel in UI)
  output$render_tool_body <- renderUI({
    res <- OR_04_module_loading()
    if(!res$is_done) {
      return(card(card_header("System not ready"), "Complete steps 1 and 2."))
    }
    res$body("active_tool")
  })

  # 3. Execution of Module Server
  observe({
    res <- OR_04_module_loading()
    req(res$is_done)
    # Execute the loaded tool's server
    res$server("active_tool", OR_01_import_dataset = OR_01_import_dataset)
  })

}

shinyApp(ui, server)
