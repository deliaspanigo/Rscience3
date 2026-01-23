library(shiny)
library(bslib)

# ============================================================
# MODULE: ANOVA
# ============================================================

module_ui_menu <- function(id) {
  ns <- NS(id)
  navset_pill_list(
    id = ns("menu_lateral"),
    well = FALSE,
    nav_panel("3. Configuration222", value = "tab_config"),
    nav_panel("4. Results222", value = "tab_results"),
    nav_panel("", value = "clean")
  )
}

module_ui_body <- function(id, data) {
  ns <- NS(id)

  # Contenedor con ID específico para control total
  div(
    id = ns("container_main"),
    class = "module-main-content",
    navset_tab(
      id = ns("tabset_main"),
      nav_panel(
        title = "Config", value = "tab_config",
        card(card_header("Model Setup222"),
             selectInput(ns("y"), "Response222 (Y):", choices = names(data)),
             selectInput(ns("x"), "Factor222 (X):", choices = names(data)))
      ),
      nav_panel(
        title = "Res", value = "tab_results",
        card(card_header("Results222"), verbatimTextOutput(ns("res_table")))
      )
    )
  )
}

module_server <- function(id, data_r, parent_nav_state) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$menu_lateral, {
      req(input$menu_lateral, input$menu_lateral != "clean")
      nav_select("tabset_main", input$menu_lateral)
    })
    output$res_table <- renderPrint({
      req(input$y, input$x)
      summary(aov(as.formula(paste(input$y, "~", input$x)), data = data_r()))
    })
  })
}


