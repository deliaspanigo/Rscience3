# Main Application
library(shiny)
library(bslib)

# Source the modules (if they're in separate files)
source("R_modules/module02_theory/module_theory.R")


# UI
ui <- page_sidebar(
  # theme = bslib::bs_theme(version = 5),
  title = "Theory Module",
  sidebar = sidebar(
    "SubModul App"
  ),

  # Use the theory module UI
  module_theory_ui("theory_module")
)

# Server
server <- function(input, output, session) {


  # Call the theory module server
  module_theory_server(id = "theory_module")

}

# Run the application
shinyApp(ui = ui, server = server)
