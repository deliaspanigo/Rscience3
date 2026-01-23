library(shiny)
library(bslib)

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = h3("Rscience 0.1.5 - Menú Dinámico", style = "margin: 0; font-weight: bold;"),

  sidebar = sidebar(
    width = 250,
    # El menú lateral ahora es dinámico, se genera en el Server
    uiOutput("sidebar_menu_dinamico"),
    hr(),
    downloadButton("download_pdf", "Generar PDF", class = "btn-danger w-100")
  ),

  # El contenido central también es dinámico
  uiOutput("panel_principal")
)

# --- SERVER ---
server <- function(input, output, session) {

  # 1. Definimos la familia de análisis (esto controla el menú)
  # Usamos un modal o un selector inicial para definir el flujo
  family_choice <- reactiveVal("inicio")

  # 2. RENDERIZAR EL MENÚ LATERAL SEGÚN LA ELECCIÓN
  output$sidebar_menu_dinamico <- renderUI({

    # Lista base de pestañas que siempre están
    tabs <- list(nav_panel("1. Base de Datos", value = "paso1"))

    # Agregamos pestañas condicionalmente
    if (family_choice() == "analisis_completo") {
      tabs <- append(tabs, list(
        nav_panel("2. Herramienta", value = "paso2"),
        nav_panel("3. Configuración", value = "paso3"),
        nav_panel("4. R Outputs", value = "paso4"),
        nav_panel("5. R Script", value = "paso5")
      ))
    } else if (family_choice() == "descriptiva_simple") {
      tabs <- append(tabs, list(
        nav_panel("2. Resultados Rápidos", value = "paso_simple")
      ))
    }

    # Creamos el componente con las pestañas filtradas
    do.call(navset_pill_list, c(list(id = "main_nav", well = FALSE), tabs))
  })

  # 3. INTERFAZ PARA CAMBIAR EL FLUJO (Ejemplo con botones en el Paso 1)
  output$panel_principal <- renderUI({
    req(input$main_nav)

    if (input$main_nav == "paso1") {
      card(
        card_header("Configuración Inicial"),
        selectInput("dataset_choice", "Dataset:", choices = c("iris", "mtcars")),
        hr(),
        p("Seleccione el tipo de flujo de trabajo:"),
        layout_column_wrap(
          width = 1/2,
          actionButton("set_completo", "Flujo Estadístico Completo", class = "btn-outline-primary"),
          actionButton("set_simple", "Solo Descriptiva Rápida", class = "btn-outline-secondary")
        )
      )
    } else {
      card(card_header("Panel de Trabajo"), p("Contenido del paso: ", input$main_nav))
    }
  })

  # Eventos para cambiar el estado del menú
  observeEvent(input$set_completo, { family_choice("analisis_completo") })
  observeEvent(input$set_simple, { family_choice("descriptiva_simple") })
}

shinyApp(ui, server)
