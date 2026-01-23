library(shiny)
library(bslib)


source("RShiny_module_folder/module_import_02_xlsx.R")

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience - Import Orchestrator - 02 - xlsx",
  sidebar = sidebar(
    navset_pill_list(
      id = "menu_fixed", well = FALSE,
      nav_panel("1. Import Data", value = "tab_import")
    )
  ),

  # SOLUTION: Using a conditional panel instead of renderUI.
  # This keeps the module "alive" and connected to its server-side logic.
  conditionalPanel(
    condition = "input.menu_fixed == 'tab_import'",
    module_import_02_xlsx_ui("import_02")
  )


)

server <- function(input, output, session) {

  # 1. Initialize the import module (Runs ONCE and persists).
  # 'data_ready' is a reactive function returning the OR (Output Reactive) list.
  OR_01_import_dataset <- module_import_02_xlsx_server("import_02", show_my_table = T)

  # 2. CONSOLE CHECK
  observe({
    # Safely access the reactive list returned by the module.
    info <- OR_01_import_dataset()
    req(info$name, info$timestamp)

    cat(sprintf("\n[Import Orchestrator - 02 - xlsx] Data received from %s at %s\n",
                info$name,
                format(info$timestamp, "%H:%M:%S")))
  })
}

shinyApp(ui, server)
