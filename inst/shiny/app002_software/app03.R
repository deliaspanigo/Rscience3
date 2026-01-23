library(shiny)
library(bslib)

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = h3("Rscience 0.1.5 - Pro Lab",
             style = "margin: 0; font-weight: bold;"),

  sidebar = sidebar(
    title = "Navegación",
    navset_pill_list(
      id = "main_nav",
      nav_panel("1. Base de Datos", value = "paso1"),
      nav_panel("2. Herramienta", value = "paso2"),
      nav_panel("3. Configuración", value = "paso3"),
      hr(),
      nav_panel("R Outputs", value = "paso4"),
      nav_panel("R Script", value = "paso5")
    )
  ),

  uiOutput("main_layout")
)

# --- SERVER ---
server <- function(input, output, session) {

  # --- REACTIVIDAD DE DATOS ---
  data_activa <- reactive({
    req(input$dataset_choice)
    get(input$dataset_choice, "package:datasets")
  })

  # --- RENDERIZADO DEL PANEL PRINCIPAL ---
  output$main_layout <- renderUI({
    req(input$main_nav)

    if (input$main_nav == "paso1") {
      card(
        card_header("Selección de Base de Datos"),
        selectInput("dataset_choice", "Elija un dataset:",
                    choices = c("iris", "mtcars", "PlantGrowth", "ToothGrowth")),
        card(card_header("Vista Previa"), tableOutput("preview_data"), full_screen = TRUE)
      )

    } else if (input$main_nav == "paso2") {
      card(
        card_header("Seleccione el tipo de análisis"),
        radioButtons("metodo", "Familias de análisis:",
                     choices = list("Estadística Descriptiva" = "desc", "Modelos Lineales" = "lm")),
        uiOutput("sub_metodo_ui")
      )

    } else if (input$main_nav == "paso3") {
      card(
        card_header(paste("Configuración para:", input$sub_metodo)),
        uiOutput("config_especifica_ui"),
        hr(),
        actionButton("run_analysis", "Procesar y Generar Resultados", class = "btn-success", icon = icon("play"))
      )
    } else if (input$main_nav == "paso4") {
      # --- SECCIÓN R OUTPUTS ---
      layout_column_wrap(
        width = 1,
        card(card_header("Resultados Estadísticos"), verbatimTextOutput("resultado_final")),
        card(card_header("Gráfico Rápido"), plotOutput("plot_final"))
      )
    } else if (input$main_nav == "paso5") {
      # --- SECCIÓN R SCRIPT ---
      card(
        card_header("Código Fuente Replicable"),
        helpText("Copia este código en RStudio para repetir el análisis:"),
        verbatimTextOutput("r_script_code")
      )
    }
  })

  # --- LÓGICA DE SUB-MÉTODOS ---
  output$sub_metodo_ui <- renderUI({
    req(input$metodo)
    choices <- if(input$metodo == "desc") c("Resumen" = "desc_gen") else c("ANOVA 1 vía" = "anova1", "Regresión" = "reg")
    selectInput("sub_metodo", "Herramienta específica:", choices = choices)
  })

  output$config_especifica_ui <- renderUI({
    req(input$sub_metodo, data_activa())
    vars <- names(data_activa())
    if (input$sub_metodo == "anova1") {
      tagList(
        selectInput("y_var", "Y (Numérica):", choices = vars),
        selectInput("x_var", "Factor:", choices = vars)
      )
    } else {
      helpText("Configure los parámetros y pase a la pestaña de Outputs.")
    }
  })

  # --- GENERACIÓN DE SCRIPT Y CÁLCULO ---
  output$r_script_code <- renderText({
    req(input$sub_metodo)
    if (input$sub_metodo == "anova1") {
      paste0(
        "# Script generado por Rscience\n",
        "data <- ", input$dataset_choice, "\n",
        "modelo <- aov(", input$y_var, " ~ ", input$x_var, ", data = data)\n",
        "summary(modelo)\n",
        "boxplot(", input$y_var, " ~ ", input$x_var, ", data = data, col = 'steelblue')"
      )
    } else {
      paste0("summary(", input$dataset_choice, ")")
    }
  })

  output$resultado_final <- renderPrint({
    input$run_analysis
    isolate({
      req(input$sub_metodo)
      if (input$sub_metodo == "anova1") {
        formula <- as.formula(paste(input$y_var, "~", input$x_var))
        summary(aov(formula, data = data_activa()))
      } else {
        summary(data_activa())
      }
    })
  })

  output$plot_final <- renderPlot({
    input$run_analysis
    isolate({
      req(input$sub_metodo == "anova1")
      formula <- as.formula(paste(input$y_var, "~", input$x_var))
      boxplot(formula, data = data_activa(), col = "#007bff", main = "Distribución por Factor")
    })
  })

  output$preview_data <- renderTable({ head(data_activa(), 6) })
}

shinyApp(ui, server)
