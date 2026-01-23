module_sidebar_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("sidebar_status"))
}

module_sidebar_server <- function(id, state_val, selection_val) {
  moduleServer(id, function(input, output, session) {
    output$sidebar_status <- renderUI({
      if (is.null(state_val$load_time)) return(p("Waiting for confirmation..."))
      tagList(
        p(tags$b("Status: "), "Loaded"),
        p("Time: ", state_val$load_time),
        p("ID: ", selection_val$tool_id)
      )
    })
  })
}




module_diagnostics_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("System Diagnostics"),
    verbatimTextOutput(ns("diagnostics_log"))
  )
}

module_diagnostics_server <- function(id, config, selection_val, state_val) {
  moduleServer(id, function(input, output, session) {
    output$diagnostics_log <- renderPrint({
      if (is.null(config())) {
        cat("❌ ERROR: Could not read 'tools_config_DEV.yml'.")
      } else {
        cat("✅ YAML loaded successfully.\n")
        cat("----------------------------------\n")
        cat("Confirmation State: ", state_val$confirmed, "\n")
        cat("Current Selection:  ", paste(selection_val$category, selection_val$tool_id, selection_val$script, sep = " > "), "\n")

        if (!is.null(selection_val$tool_id)) {
          cat("\n--- YAML Structure for this ID ---\n")
          print(config()$tools[[selection_val$tool_id]])
        }
      }
    })
  })
}
