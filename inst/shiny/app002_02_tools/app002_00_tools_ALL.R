


source("RShiny_module_folder/module_tool_selector.R")

# ==============================================================================
# APP PRINCIPAL
# ==============================================================================

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience - Tool Manager",
  sidebar = sidebar(
    navset_pill_list(
      id = "menu_tools", well = FALSE,
      nav_panel("1.Tools", value = "tab_ui"),
      nav_panel("1.1. Tools Debug", value = "tab_debug")
    )
  ),
  conditionalPanel(condition = "input.menu_tools == 'tab_ui'", module_tool_selector_ui("tool_module_01")),
  conditionalPanel(condition = "input.menu_tools == 'tab_debug'", card(verbatimTextOutput("debug_verbatim")))
)

server <- function(input, output, session) {
  # Asegúrate de que el archivo tools_config_DEV.yml existe en el directorio de trabajo
  tool_results <- module_tool_selector_server("tool_module_01", "tools_config_DEV.yml")

  output$debug_verbatim <- renderPrint({ tool_results() })
}

shinyApp(ui, server)
