library(shiny)
library(bslib)
library(rmarkdown)

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  title = h3("Rscience 0.1.5", style = "margin: 0; font-weight: bold;"),

  sidebar = sidebar(
    width = 250,
    # Menú de botones verticales (Pills)
    navset_pill_list(
      id = "main_nav",
      well = FALSE, # Quita el recuadro gris para evitar superposición
      nav_panel("1. Base de Datos", value = "paso1"),
      nav_panel("2. Herramienta", value = "paso2"),
      nav_panel("3. Configuración", value = "paso3"),
      nav_panel("4. R Outputs", value = "paso4"),
      nav_panel("5. R Script", value = "paso5")
    ),
    hr(),
    # Botón de PDF mejorado
    downloadButton("download_pdf", "Generar PDF", class = "btn-danger w-100")
  ),

  # El contenido cambia según el botón presionado en el sidebar
  uiOutput("panel_principal")
)

# --- SERVER ---
server <- function(input, output, session) {

  # 1. Datos Reactivos
  data_activa <- reactive({
    req(input$dataset_choice)
    get(input$dataset_choice, "package:datasets")
  })

  # 2. Renderizado del Panel Principal
  output$panel_principal <- renderUI({
    req(input$main_nav)

    if (input$main_nav == "paso1") {
      card(
        card_header("Paso 1: Datos"),
        selectInput("dataset_choice", "Dataset:", choices = c("iris", "mtcars", "PlantGrowth")),
        tableOutput("preview_data")
      )
    } else if (input$main_nav == "paso2") {
      card(
        card_header("Paso 2: Herramienta"),
        radioButtons("fam", "Familia:", c("Descriptiva" = "desc", "Modelos" = "lm")),
        uiOutput("select_herramienta_especifica")
      )
    } else if (input$main_nav == "paso3") {
      card(
        card_header("Paso 3: Configuración"),
        uiOutput("config_vars"),
        actionButton("btn_run", "Procesar", class = "btn-success w-100")
      )
    } else if (input$main_nav == "paso4") {
      layout_column_wrap(
        width = 1,
        card(card_header("Output"), verbatimTextOutput("txt_res")),
        card(card_header("Gráfico"), plotOutput("plt_res"))
      )
    } else {
      card(card_header("Script"), verbatimTextOutput("script_res"))
    }
  })

  # 3. Lógica de Herramientas
  output$select_herramienta_especifica <- renderUI({
    req(input$fam)
    opts <- if(input$fam == "desc") c("Resumen" = "sum") else c("ANOVA 1 vía" = "aov1")
    selectInput("herramienta", "Elegir:", choices = opts)
  })

  output$config_vars <- renderUI({
    req(input$herramienta, data_activa())
    vars <- names(data_activa())
    if(input$herramienta == "aov1") {
      tagList(
        selectInput("y", "Y:", vars),
        selectInput("x", "Factor:", vars)
      )
    } else { helpText("Sin configuración necesaria.") }
  })

  # 4. Procesamiento
  resultados <- eventReactive(input$btn_run, {
    req(input$herramienta)
    d <- data_activa()
    if(input$herramienta == "aov1") {
      f <- as.formula(paste(input$y, "~", input$x))
      list(m = summary(aov(f, d)), p = f, type = "aov")
    } else { list(m = summary(d), type = "desc") }
  })

  output$txt_res <- renderPrint({ resultados()$m })
  output$plt_res <- renderPlot({
    req(resultados()$type == "aov")
    boxplot(resultados()$p, data = data_activa(), col = "steelblue")
  })
  output$preview_data <- renderTable({ head(data_activa()) })
  output$script_res <- renderText({ "fit <- aov(y ~ x, data)\nsummary(fit)" })

  # 5. Lógica del PDF (Solución al fallo)
  output$download_pdf <- downloadHandler(
    filename = function() { paste0("reporte-", Sys.Date(), ".pdf") },
    content = function(file) {
      # Creamos un archivo RMarkdown temporal
      temp_report <- tempfile(fileext = ".Rmd")

      # Contenido del reporte
      report_src <- paste0(
        "---\ntitle: 'Reporte Rscience'\noutput: pdf_document\n---\n\n",
        "Resultado del análisis:\n\n",
        "```{r echo=FALSE}\n",
        "summary(aov(", input$y, " ~ ", input$x, ", data = ", input$dataset_choice, "))\n",
        "boxplot(", input$y, " ~ ", input$x, ", data = ", input$dataset_choice, ", col='steelblue')\n",
        "```"
      )

      writeLines(report_src, temp_report)

      # Renderizar
      rmarkdown::render(temp_report, output_file = file,
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)
