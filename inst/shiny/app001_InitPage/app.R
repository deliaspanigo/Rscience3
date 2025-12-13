# app.R - Con tabPanel simple
library(shiny)

ui <- navbarPage(
  title = "Rscience3",

  tabPanel("Inicio",
           h3("Bienvenido a Rscience3"),
           p("Selecciona una opción:"),
           actionButton("go_analisis", "Ir a Análisis"),
           actionButton("go_viz", "Ir a Visualización")
  ),

  tabPanel("Análisis",
           h3("Módulo de Análisis"),
           p("Contenido pendiente...")
  ),

  tabPanel("Visualización",
           h3("Módulo de Visualización"),
           p("Contenido pendiente...")
  ),

  footer = div(style = "text-align: center;",
               p("© 2024 Rscience3")
  )
)

server <- function(input, output, session) {
  observeEvent(input$go_analisis, {
    updateNavbarPage(session, "nav", selected = "Análisis")
  })

  observeEvent(input$go_viz, {
    updateNavbarPage(session, "nav", selected = "Visualización")
  })
}

shinyApp(ui = ui, server = server)
