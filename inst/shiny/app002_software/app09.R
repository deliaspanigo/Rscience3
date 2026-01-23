library(shiny)
library(bslib)

# ============================================================
# MODULE: ANOVA
# ============================================================
anova_ui_menu <- function(id) {
  ns <- NS(id)
  navset_pill_list(
    id = ns("menu"), well = FALSE,
    nav_panel("3. ANOVA Config", value = "anova_config"),
    nav_panel("4. ANOVA Results", value = "anova_res"),
    nav_panel("", value = "clean")
  )
}

anova_ui_body <- function(id, tab, data) {
  ns <- NS(id)
  if (tab == "anova_config") {
    card(
      card_header("ANOVA Configuration"),
      selectInput(ns("y"), "Response (Y):", choices = names(data)),
      selectInput(ns("x"), "Factor (X):", choices = names(data))
    )
  } else {
    card(card_header("Results"), verbatimTextOutput(ns("res")))
  }
}

anova_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    output$res <- renderPrint({
      req(input$y, input$x)
      summary(aov(as.formula(paste(input$y, "~", input$x)), data = data_r()))
    })
  })
}

# ============================================================
# MODULE: DESCRIPTIVES
# ============================================================
desc_ui_menu <- function(id) {
  ns <- NS(id)
  navset_pill_list(
    id = ns("menu"), well = FALSE,
    nav_panel("3. Variables", value = "desc_config"),
    nav_panel("4. Summary", value = "desc_res"),
    nav_panel("", value = "clean")
  )
}

desc_ui_body <- function(id, tab, data) {
  ns <- NS(id)
  if (tab == "desc_config") {
    card(checkboxGroupInput(ns("vars"), "Select Variables:", choices = names(data)))
  } else {
    card(tableOutput(ns("res")))
  }
}

desc_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    output$res <- renderTable({
      req(input$vars)
      sapply(data_r()[, input$vars, drop=F], function(x) if(is.numeric(x)) mean(x, na.rm=T) else NA)
    }, rownames = TRUE)
  })
}

# ============================================================
# ORCHESTRATOR
# ============================================================
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience 1.0",
  tags$head(tags$style(".nav-item:has(a[data-value='clean']) { display: none; }")),
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

  # --- 1. TOOL REGISTRY (THE SINGLE SOURCE OF TRUTH) ---
  # Add as many tools as you want here. The orchestrator handles the rest.
  tools <- list(
    "anova" = list(
      label = "One-Way ANOVA",
      menu = anova_ui_menu,
      body = anova_ui_body,
      server = anova_server,
      start_tab = "anova_config"  # <--- Automatic entry point
    ),
    "desc" = list(
      label = "Descriptives",
      menu = desc_ui_menu,
      body = desc_ui_body,
      server = desc_server,
      start_tab = "desc_config"   # <--- Automatic entry point
    )
  )

  nav_state <- reactiveValues(origin = "fixed", tab = "fixed_data")
  active_tool <- reactiveVal(NULL)

  data_r <- reactive({
    req(input$dataset_sel)
    get(input$dataset_sel, "package:datasets")
  })

  # --- 2. AUTOMATIC NAVIGATION CONTROLLER ---

  observeEvent(input$menu_fixed, {
    req(input$menu_fixed != "clean")
    nav_state$origin <- "fixed"
    nav_state$tab <- input$menu_fixed
    if(!is.null(active_tool())) nav_select(paste0(active_tool(), "-menu"), "clean")
  })

  observe({
    req(active_tool())
    module_menu_id <- paste0(active_tool(), "-menu")
    val <- input[[module_menu_id]]
    req(val, val != "clean")
    nav_state$origin <- "module"
    nav_state$tab <- val
    nav_select("menu_fixed", "clean")
  })

  # --- 3. DYNAMIC RENDERING ---

  output$separator <- renderUI({ if(!is.null(active_tool())) hr() })

  output$ui_menu_module <- renderUI({
    req(active_tool())
    tools[[active_tool()]]$menu(active_tool())
  })

  output$main_shared_body <- renderUI({
    if (nav_state$origin == "fixed") {
      if (nav_state$tab == "fixed_data") {
        card(selectInput("dataset_sel", "Dataset:", choices = c("iris", "mtcars")), tableOutput("preview"))
      } else {
        # Automatic choice generation from the list
        choices <- setNames(names(tools), sapply(tools, `[[`, "label"))
        card(radioButtons("tool_choice", "Choose Tool:", choices = choices),
             actionButton("btn_load", "Load Module", class="btn-primary w-100"))
      }
    } else {
      req(active_tool())
      tools[[active_tool()]]$body(active_tool(), nav_state$tab, data_r())
    }
  })

  # --- 4. AUTOMATIC LOADING ---
  observeEvent(input$btn_load, {
    target_id <- input$tool_choice
    active_tool(target_id)

    # Initialize server
    tools[[target_id]]$server(target_id, data_r)

    # Automatic navigation based on tool metadata
    nav_state$origin <- "module"
    nav_state$tab <- tools[[target_id]]$start_tab
    nav_select("menu_fixed", "clean")
  })

  output$preview <- renderTable({ head(data_r(), 5) })
}

shinyApp(ui, server)
