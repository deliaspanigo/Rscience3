library(shiny)
library(bslib)
library(yaml)

# ============================================================
# ORCHESTRATOR - YAML SELECTOR
# ============================================================

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience 1.0",

  sidebar = sidebar(
    title = "Configuración",
    # 1. Selector de Categoría
    selectInput(
      "category_sel",
      "Seleccione Categoría:",
      choices = NULL
    ),

    # 2. Selector de ID (Herramienta)
    selectInput(
      "tool_id_sel",
      "Seleccione Herramienta:",
      choices = NULL
    ),

    hr(),
    actionButton("btn_load", "Cargar Módulo", class = "btn-primary w-100")
  ),

  # El contenido principal va directamente aquí en page_sidebar
  uiOutput("main_content")
)

server <- function(input, output, session) {

  # --- 1. CARGA DE CONFIGURACIÓN ---
  config_data <- reactive({
    yaml::read_yaml("tools_config_DEV.yml")
  })

  # Objeto para guardar estados y marcas de tiempo
  nav_state <- reactiveValues(
    load_time = NULL,
    active_id = NULL
  )

  # --- 2. LÓGICA DE SELECTORES (CASCADA) ---

  # Llenar categorías al iniciar
  observe({
    req(config_data())
    tools_list <- config_data()$tools
    categories <- unique(sapply(tools_list, function(x) x$category))
    updateSelectInput(session, "category_sel", choices = categories)
  })

  # Actualizar IDs según categoría
  observeEvent(input$category_sel, {
    req(input$category_sel, config_data())
    tools_list <- config_data()$tools

    # Filtrar herramientas de la categoría elegida
    filtered <- Filter(function(x) x$category == input$category_sel, tools_list)

    # Crear opciones: Nombre visual = ID técnico
    tool_choices <- setNames(
      sapply(filtered, function(x) x$id),
      sapply(filtered, function(x) x$name)
    )

    updateSelectInput(session, "tool_id_sel", choices = tool_choices)
  })

  # --- 3. EVENTO DE CARGA ---

  observeEvent(input$btn_load, {
    req(input$tool_id_sel)

    # Guardar hora del sistema y ID seleccionado
    nav_state$load_time <- Sys.time()
    nav_state$active_id <- input$tool_id_sel

    message(paste("Módulo", nav_state$active_id, "cargado a las:", nav_state$load_time))
  })

  # --- 4. RENDERIZADO PRINCIPAL ---

  output$main_content <- renderUI({
    if (is.null(nav_state$active_id)) {
      # Pantalla de bienvenida si no hay nada cargado
      card(
        card_header("Bienvenido"),
        p("Por favor, seleccione una herramienta en el menú lateral y haga clic en 'Cargar Módulo'.")
      )
    } else {
      # Información de la herramienta cargada
      tool_info <- config_data()$tools[[nav_state$active_id]]

      card(
        card_header(paste("Módulo Activo:", tool_info$name)),
        p(tags$b("ID Técnico: "), tool_info$id),
        p(tags$b("Descripción: "), tool_info$description),
        p(tags$b("Subcategoría: "), tool_info$subcategory),
        p(tags$b("Grupos Estadísticos: "), paste(tool_info$statistic_group, collapse = ", ")),
        hr(),
        p(tags$small(paste("Cargado el:", format(nav_state$load_time, "%d/%m/%Y a las %H:%M:%S"))),
          style = "color: #7f8c8d;")
      )
    }
  })
}

shinyApp(ui, server)
