library(shiny)
library(bslib)

# ============================================================
# MODULE: ANOVA
# ============================================================
source(file = "RShiny_modules.R")

# ============================================================
# ORCHESTRATOR
# ============================================================

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience Orchestrator",
  tags$head(
    tags$style("
      /* 1. Esconder pestañas del sidebar que sirven para limpiar */
      .nav-item:has(a[data-value='clean']) { display: none !important; }

      /* 2. ESCONDER LAS PESTAÑAS DEL CUERPO CENTRAL (HARD RESET) */
      /* Esto elimina el header completo del navset_tab dentro del main */
      .module-main-content .nav.nav-tabs {
        display: none !important;
        visibility: hidden !important;
        height: 0 !important;
        overflow: hidden !important;
      }

      /* Limpiar bordes residuales */
      .module-main-content .tab-content {
        border: none !important;
      }
    ")
  ),
  sidebar = sidebar(
    navset_pill_list(
      id = "menu_fixed", well = FALSE,
      nav_panel("1. Dataset", value = "fixed_data"),
      nav_panel("2. Tools", value = "fixed_tools"),
      nav_panel("", value = "clean")
    ),
    uiOutput("ui_separator"),
    uiOutput("ui_menu_module")
  ),
  uiOutput("main_shared_body")
)

server <- function(input, output, session) {
  data_r <- reactive({ mtcars })
  nav_state <- reactiveValues(origin = "fixed", tab = "fixed_data")
  active_tool <- reactiveVal(NULL)

  observeEvent(input$menu_fixed, {
    req(input$menu_fixed, input$menu_fixed != "clean")
    nav_state$origin <- "fixed"
    nav_state$tab <- input$menu_fixed
    if (!is.null(active_tool())) nav_select(paste0(active_tool(), "-menu_lateral"), "clean")
  })

  observeEvent(input$btn_load, {
    active_tool("anova")
    module_server("anova", data_r, nav_state)
    nav_state$origin <- "module"
    nav_state$tab <- "tab_config"
    nav_select("menu_fixed", "clean")
  })

  output$ui_separator <- renderUI({ if(!is.null(active_tool())) hr() })
  output$ui_menu_module <- renderUI({
    req(active_tool() == "anova")
    module_ui_menu("anova")
  })

  output$main_shared_body <- renderUI({
    if (nav_state$origin == "fixed") {
      if (nav_state$tab == "fixed_data") {
        card("Dataset management section.")
      } else {
        card(actionButton("btn_load", "LOAD ANOVA MODULE", class="btn-primary"))
      }
    } else {
      module_ui_body("anova", data_r())
    }
  })
}

shinyApp(ui, server)
