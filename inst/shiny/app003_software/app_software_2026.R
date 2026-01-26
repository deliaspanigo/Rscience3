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

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience - Centralized Preview",

  # --- TECHNICAL BLOCK: CSS TO HIDE CONTROL TABS ---
  tags$head(
    tags$style(HTML("
      /* Hide the bridge tab of the master menu */
      .nav-link[data-value='tab_execute_tool'],
      .nav-item:has(> .nav-link[data-value='tab_execute_tool']) {
        display: none !important;
      }

      /* Hide the clean tab of the dynamic menu (ANOVA, etc) */
      .nav-link[data-value='clean'],
      .nav-item:has(> .nav-link[data-value='clean']) {
        display: none !important;
      }

      /* Aesthetic adjustment for the hr between menus */
      hr {
        margin: 1rem 0;
        opacity: 0.15;
      }
    "))
  ),

  sidebar = sidebar(
    # MENU 1: Master (Orchestrator)
    navset_pill_list(
      id = "menu_fixed",
      well = FALSE,
      nav_panel("1. Dataset", value = "tab_import"),
      if(SHOW_DEBUG) nav_panel("1.1. Debug Dataset", value = "tab_import_DEBUG"),
      nav_panel("2. Tools", value = "tab_tools"),
      if(SHOW_DEBUG) nav_panel("2.1. Tools Debug", value = "tab_tools_DEBUG"),
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

  # Group B: Tool Execution Panel
  # Only visible when the Master jumps to 'tab_execute_tool'
  conditionalPanel(
    condition = "input.menu_fixed == 'tab_execute_tool'",
    uiOutput("render_tool_body")
  )
)

server <- function(input, output, session) {

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


  # --- STATE CENTRALIZER (Gatekeeper) ---
  OR_CENTRAL_is_done_import_and_tools <- reactive({
    is_done_import <- isTRUE(OR_01_import_dataset()$is_done)
    is_done_tools  <- isTRUE(OR_02_tools()$is_done)
    all(is_done_import, is_done_tools)
  })


  # --- PHASE 03: TEMPORAL FF (Encapsulated) ---
  OR_03_temporal_FF <- reactive({
    is_ready <- OR_CENTRAL_is_done_import_and_tools()
    if (!is_ready) return(list(is_done = FALSE))

    tool_data <- OR_02_tools()
    base_path <- ".."
    path_local_file <- file.path(base_path, tool_data$special_path)

    # 1. Create a dedicated Environment for this tool
    tool_env <- new.env(parent = .GlobalEnv)

    # 2. Source the tool into that specific environment
    tryCatch({
      source(path_local_file, local = tool_env)

      list(
        is_done = TRUE,
        tool_env = tool_env, # We pass the whole sandbox forward
        timestamp = Sys.time()
      )
    }, error = function(e) {
      showNotification(paste("Error loading tool logic:", e$message), type = "error")
      list(is_done = FALSE)
    })
  })

  output$debug_verbatim_03_temporal_FF_DEBUG <- renderPrint({
    OR_03_temporal_FF()
  })


  # --- PHASE 04: MODULE EXTRACTION (Agnostic Dispatcher) ---
  OR_04_module_loading <- reactive({
    phase3 <- OR_03_temporal_FF()
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
