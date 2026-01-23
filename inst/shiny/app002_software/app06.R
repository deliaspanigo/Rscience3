library(shiny)
library(bslib)
library(rmarkdown)

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = h3("Rscience 0.1.5", style = "margin: 0; font-weight: bold;"),

  sidebar = sidebar(
    width = 280,
    navset_pill_list(
      id = "main_nav",
      well = FALSE,
      nav_panel("1. Datos", value = "paso_datos"),
      nav_panel("2. Herramienta", value = "paso_herramienta"),
      nav_panel("3. Configuración", value = "paso_config"),
      nav_panel("4. Resultados", value = "paso_res"),
      nav_panel("5. R Script", value = "paso_script")
    ),
    hr(),
    uiOutput("ui_download_btn")
  ),

  # 1. PASO DATOS
  conditionalPanel(
    condition = "input.main_nav == 'paso_datos'",
    card(card_header("Paso 1: Datos"),
         selectInput("dataset", "Dataset:", choices = c("iris", "mtcars", "PlantGrowth")),
         tableOutput("preview_data"))
  ),

  # 2. PASO HERRAMIENTA
  conditionalPanel(
    condition = "input.main_nav == 'paso_herramienta'",
    card(card_header("Paso 2: Selección de Herramienta"),
         uiOutput("ui_herramienta_container"),
         hr(),
         layout_column_wrap(width = 1/3,
                            uiOutput("ui_btn_confirmar"),
                            actionButton("btn_rehab", "Rehabilitar", class = "btn-outline-warning", icon = icon("pen")),
                            actionButton("btn_reset", "Reset", class = "btn-outline-danger", icon = icon("trash-can"))
         )
    )
  ),

  # 3. PASO CONFIGURACIÓN (Con bloqueo y ocultamiento de opciones)
  conditionalPanel(
    condition = "input.main_nav == 'paso_config'",
    uiOutput("bloqueo_config"),
    conditionalPanel(
      condition = "output.confirmado_status == true",
      card(card_header("Paso 3: Configuración de Variables"),
           uiOutput("ui_config_container"), # Selector o Texto resumen
           hr(),
           layout_column_wrap(width = 1/3,
                              uiOutput("ui_btn_run"),
                              actionButton("btn_edit_vars", "Corregir", class = "btn-outline-warning", icon = icon("sliders")),
                              actionButton("btn_clear_vars", "Limpiar", class = "btn-outline-danger", icon = icon("eraser"))
           )
      )
    )
  ),

  # 4 & 5. RESULTADOS Y SCRIPT
  conditionalPanel(condition = "input.main_nav == 'paso_res'", uiOutput("bloqueo_res"),
                   conditionalPanel(condition = "output.confirmado_status == true && output.ejecutado_status == true", uiOutput("ui_res_completo"))),

  conditionalPanel(condition = "input.main_nav == 'paso_script'", uiOutput("bloqueo_script"),
                   conditionalPanel(condition = "output.confirmado_status == true && output.ejecutado_status == true", card(card_header("Script"), verbatimTextOutput("res_script"))))
)

# --- SERVER ---
server <- function(input, output, session) {

  confirmado <- reactiveVal(FALSE) # Paso 2
  ejecutado <- reactiveVal(FALSE)  # Paso 3
  res_data <- reactiveVal(NULL)

  # Pasamos estados al UI
  output$confirmado_status <- reactive({ confirmado() })
  output$ejecutado_status <- reactive({ ejecutado() })
  outputOptions(output, "confirmado_status", suspendWhenHidden = FALSE)
  outputOptions(output, "ejecutado_status", suspendWhenHidden = FALSE)

  # Memoria de variables
  observe({
    df <- get(input$dataset, "package:datasets")
    vars <- names(df)
    updateSelectInput(session, "y_var", choices = vars, selected = input$y_var)
    updateSelectInput(session, "x_var", choices = vars, selected = input$x_var)
    updateSelectInput(session, "desc_var", choices = vars, selected = input$desc_var)
  })

  # --- LÓGICA PASO 2 (HERRAMIENTA) ---
  output$ui_herramienta_container <- renderUI({
    if (!confirmado()) {
      radioButtons("tipo_analisis", "Seleccione el método:",
                   choices = c("Estadística Descriptiva" = "desc", "ANOVA de un factor" = "anova"),
                   selected = input$tipo_analisis)
    } else {
      tagList(h6("Método fijado:"), span(class = "badge bg-success fs-5", if(input$tipo_analisis == "desc") "Descriptiva" else "ANOVA"))
    }
  })

  output$ui_btn_confirmar <- renderUI({
    if (!confirmado()) actionButton("btn_confirmar", "Confirmar", class = "btn-primary w-100")
    else actionButton("btn_confirmar", "Listo", class = "btn-success w-100 disabled")
  })

  observeEvent(input$btn_confirmar, { req(input$tipo_analisis); confirmado(TRUE); nav_select("main_nav", "paso_config") })
  observeEvent(input$btn_rehab, { confirmado(FALSE); ejecutado(FALSE); res_data(NULL) })
  observeEvent(input$btn_reset, { confirmado(FALSE); ejecutado(FALSE); res_data(NULL); updateRadioButtons(session, "tipo_analisis", selected = character(0)) })

  # --- LÓGICA PASO 3 (CONFIGURACIÓN) ---
  output$ui_config_container <- renderUI({
    if (!ejecutado()) {
      # MODO EDICIÓN: Muestra los selectores
      tagList(
        conditionalPanel(condition = "input.tipo_analisis == 'anova'",
                         selectInput("y_var", "Variable Respuesta (Y):", choices = NULL),
                         selectInput("x_var", "Factor de Agrupación (X):", choices = NULL)),
        conditionalPanel(condition = "input.tipo_analisis == 'desc'",
                         selectInput("desc_var", "Variables a Describir:", choices = NULL, multiple = TRUE))
      )
    } else {
      # MODO BLOQUEADO: Muestra resumen de variables
      if(input$tipo_analisis == "anova") {
        tagList(h6("Variables fijadas:"),
                p(strong("Y: "), input$y_var), p(strong("X: "), input$x_var))
      } else {
        tagList(h6("Variables fijadas:"),
                p(strong("Columnas: "), paste(input$desc_var, collapse = ", ")))
      }
    }
  })

  output$ui_btn_run <- renderUI({
    if (!ejecutado()) actionButton("run", "Ejecutar", class = "btn-success w-100", icon = icon("play"))
    else actionButton("run", "Ejecutado", class = "btn-success w-100 disabled", icon = icon("check-double"))
  })

  observeEvent(input$run, {
    df <- get(input$dataset, "package:datasets")
    if(input$tipo_analisis == "anova") {
      req(input$y_var, input$x_var)
      f_str <- paste(input$y_var, "~", input$x_var)
      res_data(list(type="anova", obj=summary(aov(as.formula(f_str), data=df)), f=as.formula(f_str),
                    code=paste0("fit <- aov(", f_str, ", data=", input$dataset, ")\nsummary(fit)")))
    } else {
      req(input$desc_var)
      res_data(list(type="desc", obj=summary(df[, input$desc_var, drop=FALSE]),
                    code=paste0("summary(", input$dataset, "[, c('", paste(input$desc_var, collapse="','"), "')])")))
    }
    ejecutado(TRUE) # Oculta opciones y muestra texto
    nav_select("main_nav", "paso_res")
  })

  observeEvent(input$btn_edit_vars, { ejecutado(FALSE); res_data(NULL) })
  observeEvent(input$btn_clear_vars, {
    updateSelectInput(session, "y_var", selected = character(0))
    updateSelectInput(session, "x_var", selected = character(0))
    updateSelectInput(session, "desc_var", selected = character(0))
    ejecutado(FALSE); res_data(NULL)
  })

  # --- ELEMENTOS COMUNES ---
  msg_bloqueo <- card(class = "bg-light text-center", card_body(h4("Paso Bloqueado"), p("Complete la configuración anterior.")))
  output$bloqueo_config <- renderUI({ if(!confirmado()) msg_bloqueo })
  output$bloqueo_res <- renderUI({ if(!ejecutado()) msg_bloqueo })
  output$bloqueo_script <- renderUI({ if(!ejecutado()) msg_bloqueo })

  output$ui_res_completo <- renderUI({
    req(res_data())
    layout_column_wrap(width = 1, card(card_header("Resultados"), verbatimTextOutput("raw_out")),
                       if(res_data()$type == "anova") card(card_header("Gráfico"), plotOutput("plt_out")))
  })

  output$raw_out <- renderPrint({ res_data()$obj })
  output$plt_out <- renderPlot({ boxplot(res_data()$f, data = get(input$dataset, "package:datasets"), col = "#5dade2") })
  output$res_script <- renderText({ res_data()$code })
  output$preview_data <- renderTable({ head(get(input$dataset, "package:datasets")) })
  output$ui_download_btn <- renderUI({ req(res_data()); downloadButton("download_pdf", "PDF", class = "btn-danger w-100") })

  output$download_pdf <- downloadHandler(
    filename = function() { "Reporte.pdf" },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      writeLines(paste0("---\ntitle: 'Reporte'\noutput: pdf_document\n---\n\n```{r echo=F}\n", res_data()$code, "\n```"), temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file)
    }
  )
}

shinyApp(ui, server)
