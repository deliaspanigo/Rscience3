

library(shiny)
library(bslib)
library(yaml)

source("module_menu_export.R")
source("module_menu_local.R")


ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience 1.0",
  sidebar = sidebar(
    title = "Control",
    module_sidebar_ui("sidebar_mod")
  ),
  layout_column_wrap(
    width = 1,
    module_selector_ui("selector_mod"),
    module_diagnostics_ui("diag_mod")
  )
)

server <- function(input, output, session) {
  # 1. Shared Config
  config <- reactive({
    tryCatch({ yaml::read_yaml("tools_config_DEV.yml") }, error = function(e) NULL)
  })

  # 2. Call Selector Module
  selector_data <- module_selector_server("selector_mod", config)

  # 3. Call Sidebar Module
  module_sidebar_server("sidebar_mod", selector_data$state, selector_data$choices())

  # 4. Call Diagnostics Module
  module_diagnostics_server("diag_mod", config, selector_data$choices(), selector_data$state)
}

shinyApp(ui, server)
