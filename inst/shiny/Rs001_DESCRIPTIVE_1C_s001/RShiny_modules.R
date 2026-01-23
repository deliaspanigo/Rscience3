# MÓDULO: Estadística Descriptiva Simple

module_ui_menu <- function(id) {
  ns <- NS(id)
  navset_pill_list(
    id = ns("menu_lateral"),
    well = FALSE,
    nav_panel("3. Config", value = "tab_config"),
    nav_panel("4. Resultados", value = "tab_results"),
    nav_panel("", value = "clean")
  )
}

module_ui_body <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_config'", ns("menu_lateral")),
      card(
        card_header("Configuración"),
        # Dejamos que el server lo renderice por completo
        uiOutput(ns("selector_ui"))
      )
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_results'", ns("menu_lateral")),
      card(
        card_header("Histograma"),
        plotOutput(ns("plot_simple"))
      ),
      card(
        card_header("Resumen"),
        verbatimTextOutput(ns("resumen_simple"))
      )
    )
  )
}

module_server <- function(id, OR_01_import_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Dataset
    df <- reactive({
      res <- OR_01_import_dataset()
      req(res$is_done, res$my_dataset)
      res$my_dataset
    })

    # 2. Renderizamos el selector DESDE CERO (sin updates)
    output$selector_ui <- renderUI({
      data <- df()
      cols <- names(data)

      selectInput(
        ns("target_var"),
        "Seleccione una columna:",
        choices = cols,
        selected = input$target_var # Intenta mantener la selección si existe
      )
    })

    # 3. Gráfico
    output$plot_simple <- renderPlot({
      req(input$target_var)
      data <- df()
      # Verificamos que la columna esté en el dataset actual
      req(input$target_var %in% names(data))

      # Forzamos a numérico para evitar errores si eliges un texto
      x <- as.numeric(data[[input$target_var]])

      hist(x, col = "steelblue", border = "white",
           main = input$target_var, xlab = NULL)
    })

    # 4. Resumen
    output$resumen_simple <- renderPrint({
      req(input$target_var)
      data <- df()
      req(input$target_var %in% names(data))

      summary(data[[input$target_var]])
    })
  })
}
