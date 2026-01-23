library(shiny)
library(bslib)

# ============================================================
# MÓDULO: ANOVA
# ============================================================
mod_anova_menu <- function(id) {
  ns <- NS(id)
  navset_pill_list(
    id = ns("menu_interno"),
    well = FALSE,
    nav_panel("3. Configuración", value = "config"),
    nav_panel("4. Resultados", value = "res")
  )
}

mod_anova_body <- function(id, active_tab, data_df, mod_status) {
  ns <- NS(id)
  req(active_tab)

  # Extraemos valores de mod_status (que es el reactiveValues devuelto por el server)
  is_conf <- if(!is.null(mod_status)) mod_status$confirmado else FALSE

  if (active_tab == "config") {
    card(
      card_header("Configuración ANOVA"),
      if (!is_conf) {
        layout_column_wrap(
          width = 1/2,
          selectInput(ns("y"), "Variable Respuesta (Y):", choices = names(data_df)),
          selectInput(ns("x"), "Factor Grupos (X):", choices = names(data_df))
        )
      } else {
        tagList(
          h5("Modelo fijado"),
          p("Respuesta: ", span(class="text-primary", mod_status$y_name)),
          p("Factor: ", span(class="text-primary", mod_status$x_name))
        )
      },
      hr(),
      layout_column_wrap(
        width = 1/3,
        actionButton(ns("run"), "Ejecutar", class="btn-success", icon = icon("play")),
        actionButton(ns("btn_rehab"), "Corregir", class = "btn-outline-warning"),
        actionButton(ns("btn_reset"), "Limpiar", class = "btn-outline-danger")
      )
    )
  } else if (active_tab == "res") {
    if (!is_conf) return(card("Ejecute el análisis en la pestaña anterior."))
    layout_column_wrap(
      width = 1,
      card(card_header("Resultados"), verbatimTextOutput(ns("txt"))),
      card(card_header("Gráfico"), plotOutput(ns("plt")))
    )
  }
}

mod_anova_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    status <- reactiveValues(confirmado = FALSE, fit = NULL, formula = NULL, y_name = NULL, x_name = NULL)

    observeEvent(input$run, {
      req(input$y, input$x)
      status$y_name <- input$y
      status$x_name <- input$x
      status$formula <- as.formula(paste(input$y, "~", input$x))
      status$fit <- summary(aov(status$formula, data = data_r()))
      status$confirmado <- TRUE
    })

    observeEvent(input$btn_rehab, { status$confirmado <- FALSE })
    observeEvent(input$btn_reset, { status$confirmado <- FALSE; status$fit <- NULL })

    output$txt <- renderPrint({ req(status$fit); status$fit })
    output$plt <- renderPlot({ req(status$formula); boxplot(status$formula, data = data_r(), col = "#5dade2") })

    return(status)
  })
}

# ============================================================
# ORQUESTADOR
# ============================================================
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Rscience 0.6.0",
  sidebar = sidebar(
    width = 300,
    navset_pill_list(
      id = "menu_fijo",
      well = FALSE,
      nav_panel("1. Dataset", value = "datos"),
      nav_panel("2. Herramientas", value = "herramientas")
    ),
    uiOutput("separador"),
    uiOutput("menu_dinamico_ui")
  ),
  uiOutput("cuerpo_compartido")
)

server <- function(input, output, session) {

  # ReactiveValues para controlar la navegación y estados de módulos
  nav_state <- reactiveValues(origen = "fijo", pestaña = "datos")
  mod_results <- reactiveValues()

  herramientas <- list(
    "anova" = list(label = "ANOVA", menu = mod_anova_menu, body = mod_anova_body, server = mod_anova_server)
  )

  h_confirmada <- reactiveVal(FALSE)

  # 1. Listener Menú Fijo
  observeEvent(input$menu_fijo, {
    req(input$menu_fijo)
    nav_state$origen <- "fijo"
    nav_state$pestaña <- input$menu_fijo
    # Desmarcar menú dinámico
    if(h_confirmada()) nav_select(paste0(input$tool_choice, "-menu_interno"), NULL)
  }, ignoreInit = TRUE)

  # 2. Listener Menú Dinámico
  observe({
    req(h_confirmada())
    tool_id <- input$tool_choice
    id_interno <- paste0(tool_id, "-menu_interno")
    req(input[[id_interno]])

    nav_state$origen <- "modulo"
    nav_state$pestaña <- input[[id_interno]]
    # Desmarcar menú fijo
    nav_select("menu_fijo", NULL)
  })

  output$separador <- renderUI({ if(h_confirmada()) hr() })
  output$menu_dinamico_ui <- renderUI({
    req(h_confirmada())
    herramientas[[input$tool_choice]]$menu(input$tool_choice)
  })

  # 3. Main Orchestrator
  output$cuerpo_compartido <- renderUI({
    if (nav_state$origen == "fijo") {
      if (nav_state$pestaña == "datos") {
        card(card_header("Paso 1: Datos"),
             selectInput("dataset_sel", "Dataset:", choices = c("iris", "mtcars", "PlantGrowth")),
             tableOutput("preview"))
      } else {
        card(card_header("Paso 2: Herramientas"),
             radioButtons("tool_choice", "Selección:", choices = setNames(names(herramientas), sapply(herramientas, `[[`, "label"))),
             actionButton("load_tool", "Cargar", class="btn-primary w-100"))
      }
    } else {
      req(h_confirmada())
      t_id <- input$tool_choice
      herramientas[[t_id]]$body(t_id, nav_state$pestaña, get(input$dataset_sel, "package:datasets"), mod_results[[t_id]])
    }
  })

  observeEvent(input$load_tool, {
    h_confirmada(TRUE)
    t_id <- input$tool_choice
    # Inicializar server y guardar el objeto reactiveValues que devuelve
    mod_results[[t_id]] <- herramientas[[t_id]]$server(t_id, reactive({ get(input$dataset_sel, "package:datasets") }))
    nav_select(paste0(t_id, "-menu_interno"), "config")
  })

  output$preview <- renderTable({ head(get(input$dataset_sel, "package:datasets")) })
}

shinyApp(ui, server)
