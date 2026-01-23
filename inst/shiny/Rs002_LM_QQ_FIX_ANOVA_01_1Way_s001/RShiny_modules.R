# ANOVA.R

module_ui_menu <- function(id) {
  ns <- NS(id)
  navset_pill_list(
    id = ns("menu_lateral"),
    well = FALSE,
    nav_panel("3. Config", value = "tab_config"),
    nav_panel("4. Results", value = "tab_results"),
    nav_panel("", value = "clean")
  )
}

module_ui_body <- function(id) {
  ns <- NS(id)
  # Usamos un div con display dinámico para que los inputs no mueran al cambiar de pestaña
  tagList(
    conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_config'", ns("menu_lateral")),
      card(
        card_header("Configuración del Modelo"),
        selectInput(ns("y_var"), "Variable Respuesta (Y):", choices = NULL),
        selectInput(ns("x_var"), "Factor (X):", choices = NULL)
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_results'", ns("menu_lateral")),
      card(
        card_header("Resultados ANOVA"),
        verbatimTextOutput(ns("anova_res"))
      )
    )
  )
}

module_server <- function(id, OR_01_import_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- reactive({
      res_import <- OR_01_import_dataset()
      req(res_import$is_done, res_import$my_dataset)
      res_import$my_dataset
    })

    # Actualizador inteligente: No resetea si el usuario ya eligió algo
    observe({
      cols <- names(df())
      updateSelectInput(session, "y_var", choices = cols, selected = input$y_var)
      updateSelectInput(session, "x_var", choices = cols, selected = input$x_var)
    })

    output$anova_res <- renderPrint({
      req(input$y_var, input$x_var)
      req(input$y_var %in% names(df()), input$x_var %in% names(df()))

      tryCatch({
        f <- as.formula(paste(input$y_var, "~", input$x_var))
        summary(aov(f, data = df()))
      }, error = function(e) "Error en el cálculo.")
    })
  })
}
