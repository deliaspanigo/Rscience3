library(shiny)
library(bslib)

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),

  title = h3("Rscience 0.1.5 - Dynamic Analysis",
             style = "margin: 0; font-weight: bold;"),

  sidebar = sidebar(
    title = "Menú de Navegación",
    # Usamos un nav_list para que el sidebar controle el contenido
    navset_pill_list(
      id = "sidebar_menu",
      nav_panel("Configuración de Datos", value = "datos"),
      nav_panel("Análisis Estadístico", value = "stats"),
      nav_panel("Reportes y Exportación", value = "report")
    ),
    hr(),
    helpText("Seleccione una categoría para ver sus opciones en el panel central.")
  ),

  # El contenido principal es dinámico
  uiOutput("main_content")
)

# --- SERVER ---
server <- function(input, output, session) {

  # Renderizamos el contenido principal basado en la elección del sidebar
  output$main_content <- renderUI({
    req(input$sidebar_menu)

    if (input$sidebar_menu == "datos") {
      # OPCIÓN 1: Layout para gestión de datos
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("1. Carga de Archivos"),
          fileInput("file1", "Seleccionar Dataset", width = "100%"),
          selectInput("sep", "Separador", choices = c("Coma" = ",", "Punto y Coma" = ";"))
        ),
        card(
          card_header("2. Filtros Rápidos"),
          numericInput("rows", "Número de filas a visualizar", 10, min = 1),
          checkboxInput("clean", "Limpiar valores NA", FALSE)
        )
      )

    } else if (input$sidebar_menu == "stats") {
      # OPCIÓN 2: Layout para el modelo ANOVA/GLM
      navset_card_underline(
        title = "Herramientas de Análisis",
        nav_panel("Variables",
                  layout_sidebar(
                    sidebar = sidebar(
                      selectInput("y_var", "Variable Respuesta (Y)", choices = NULL),
                      selectInput("x_var", "Factor Principal (A)", choices = NULL),
                      open = TRUE
                    ),
                    "Aquí aparecería un resumen estadístico de las variables elegidas."
                  )
        ),
        nav_panel("Ajustes del Modelo",
                  radioButtons("type_ss", "Suma de Cuadrados",
                               choices = c("Tipo I", "Tipo II", "Tipo III")),
                  actionButton("calc", "Correr ANOVA", class = "btn-success")
        )
      )

    } else if (input$sidebar_menu == "report") {
      # OPCIÓN 3: Layout de exportación
      card(
        card_header("Finalizar Proyecto"),
        p("Elija el formato de salida para sus resultados:"),
        layout_column_wrap(
          width = 1/3,
          downloadButton("down_pdf", "PDF", class = "w-100"),
          downloadButton("down_csv", "CSV", class = "w-100"),
          downloadButton("down_word", "Word", class = "w-100")
        )
      )
    }
  })
}

shinyApp(ui, server)
