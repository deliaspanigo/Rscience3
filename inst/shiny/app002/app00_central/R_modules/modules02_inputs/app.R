# Main Application
library(shiny)
library(bslib)

# Source the modules (if they're in separate files)
source("module_inputs.R")



# UI
ui <- page_sidebar(
  title = "Inputs Module",
  sidebar = sidebar(
    "SubModule App",
    # bslib::input_switch("super_selector", "Selector", value = FALSE)
    uiOutput("the_toggle_selector")
  ),

  # Card principal con dos solapas
conditionalPanel(condition = "input.the_toggle == false", module_inputs_ui("inputs_module")),
conditionalPanel(condition = "input.the_toggle == true", uiOutput("view"))



)

# Server
server <- function(input, output, session) {


  ### Toogle 01 - ClassRoom ----------------------------------------------------
  output$the_toggle_selector <- renderUI({

    div(
      tags$head(
        tags$style(HTML("
      /* Toggle style */
      .form-check-input {
        background-color: #4c78dd !important; /* Blue color for default */
        border-color: #4c78dd !important;
        width: 3.5em !important; /* Increase toggle width */
        height: 1.8em !important; /* Increase height proportionally */
      }

      /* Style when activated (TRUE) */
      .form-check-input:checked {
        background-color: #4CAF50 !important; /* Green color for true */
        border-color: #4CAF50 !important;
      }

      /* Ensure smooth transition */
      .form-check-input {
        transition: background-color 0.3s, border-color 0.3s;
      }

      /* Adjust the indicator circle inside the toggle */
      .form-switch .form-check-input:after {
        height: calc(1.8em - 4px) !important;
        width: calc(1.8em - 4px) !important;
      }

      /* Adjust container spacing */
      .form-switch {
        padding-left: 0 !important;
      }
    "))
      ),
      div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-3",
        span("   ", class = "fw-bold"),
        tags$div(
          class = "form-check form-switch",
          tags$input(
            id = "the_toggle",
            type = "checkbox",
            class = "form-check-input",
            role = "switch"
          )
        ),
        uiOutput("the_toggle_info", inline = TRUE)
      )
    )
  })


  output$the_toggle_info <- renderUI({
    # 1.Text to show
    the_selection <- ifelse(
      test = input$the_toggle,
      yes = "Internal",  # Active  - Green
      no = "External"        # Deafult - Blue
    )})

  # Call the theory module server
  list_stone <- module_inputs_server(id = "inputs_module")

  # Render UI con tabpanel para cada elemento de la lista
  output$"view" <- renderUI({
    req(list_stone())

    # Crear lista de pestañas dinámicamente
    nav_panels <- lapply(names(list_stone()), function(list_name) {
      nav_panel(
        title = list_name,
        verbatimTextOutput(outputId = paste0("view_", list_name))
      )
    })

    # Crear el navset con las pestañas
    do.call(navset_tab, nav_panels)
  })

  # Render outputs para cada lista
  observe({
    req(list_stone())

    # Crear un output para cada elemento de list_stone
    for(list_name in names(list_stone())) {
      local({
        local_name <- list_name
        output[[paste0("view_", local_name)]] <- renderPrint({
          list_stone()[[local_name]]
        })
      })
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
