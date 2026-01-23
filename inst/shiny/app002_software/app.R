library(shiny)
library(bslib)

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"), # Tema profesional opcional

  title = h3("General Linear Models - Fix Effects - Balanced Tratments - Anova - Anova 1 Way",
             style = "margin: 0; font-weight: bold;"),

  sidebar = sidebar(
    "Rscience 0.1.5",
    hr(),
    uiOutput("sidebar_content") # Dinámico según la pestaña
  ),

  # Contenedor principal dinámico
  uiOutput("main_content")
)

# --- SERVER ---
server <- function(input, output, session) {

  # 1. Definimos el TabsetPanel principal
  output$main_content <- renderUI({
    navset_tab(
      id = "main_tabs",
      nav_panel("Data Input",
                card(card_header("Importar Datos"), "Aquí va la tabla de datos...")),
      nav_panel("Anova Model",
                card(card_header("Resultados del Modelo"), "Aquí se muestran los resultados...")),
      nav_panel("Diagnostics",
                card(card_header("Gráficos de Residuos"), "Validación de supuestos..."))
    )
  })

  # 2. Definimos el contenido del sidebar basado en la pestaña activa
  output$sidebar_content <- renderUI({
    req(input$main_tabs) # Espera a que cargue el UI principal

    if (input$main_tabs == "Data Input") {
      tagList(
        fileInput("file", "Cargar CSV", accept = ".csv"),
        checkboxInput("header", "Encabezado", TRUE)
      )
    } else if (input$main_tabs == "Anova Model") {
      tagList(
        selectInput("dep_var", "Variable Dependiente (Y)", choices = "Esperando datos..."),
        selectInput("ind_var", "Factor (X)", choices = "Esperando datos..."),
        actionButton("run", "Ejecutar ANOVA", class = "btn-primary w-100")
      )
    } else {
      helpText("Opciones de diagnóstico de modelo.")
    }
  })
}

shinyApp(ui, server)
