


library(shiny)
library(bslib)
library(shinycssloaders)

# Cargar archivos (Asegúrate de que las rutas sean correctas)
source("RShiny_module_folder/module_import_01_RData.R")
source("RShiny_module_folder/module_import_02_xlsx.R")
source("RShiny_module_folder/module_orchestrator_01_import_dataset.R")

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience - Centralized Preview",
  sidebar = sidebar(
    navset_pill_list(
      id = "menu_fixed", well = FALSE,
      nav_panel("1. Dataset", value = "tab_import"),
      nav_panel("1.1. Debug Dataset", value = "tab_verbatim")
    )
  ),

  # Panel Orquestador
  conditionalPanel(
    condition = "input.menu_fixed == 'tab_import'",
    module_orchestrator_01_import_dataset_ui("master_import")
  ),

  # Panel Verbatim
  conditionalPanel(
    condition = "input.menu_fixed == 'tab_verbatim'",
    card(
      card_header("Verbatim Object Structure"),
      card_body(
        verbatimTextOutput("debug_verbatim")
      )
    )
  )
)

server <- function(input, output, session) {

  # Llamada al orquestador
  OR_01_import_dataset <- module_orchestrator_01_import_dataset_server("master_import")

  # Debugging: muestra el estado del objeto en tiempo real
  output$debug_verbatim <- renderPrint({
    str(OR_01_import_dataset())
  })
}

shinyApp(ui, server)
