library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Rscience Orchestrator",
  # Estilo CSS para esconder las pestañas de "limpieza"
  tags$head(tags$style(".nav-item:has(a[data-value='limpiar']) { display: none; }")),
  sidebar = sidebar(
    # MENU 1
    navset_pill_list(
      id = "menu_fijo",
      well = FALSE,
      nav_panel("1. Datos", value = "fijo_datos"),
      nav_panel("2. Herramientas", value = "fijo_herramientas"),
      nav_panel("", value = "limpiar") # Pestaña invisible
    ),
    uiOutput("separador"),
    # MENU 2
    uiOutput("ui_menu_modulo")
  ),
  uiOutput("main_compartido")
)

server <- function(input, output, session) {

  nav_state <- reactiveValues(origen = "fijo", pestaña = "fijo_datos")
  herramienta_cargada <- reactiveVal(FALSE)

  # --- CONTROLADOR DE SELECCIÓN ÚNICA ---

  # Al clicar arriba, limpio abajo
  observeEvent(input$menu_fijo, {
    req(input$menu_fijo != "limpiar")
    nav_state$origen <- "fijo"
    nav_state$pestaña <- input$menu_fijo

    if(herramienta_cargada()) {
      nav_select("menu_modulo", "limpiar") # Manda el menú de abajo a lo invisible
    }
  })

  # Al clicar abajo, limpio arriba
  observeEvent(input$menu_modulo, {
    req(input$menu_modulo != "limpiar")
    nav_state$origen <- "modulo"
    nav_state$pestaña <- input$menu_modulo

    nav_select("menu_fijo", "limpiar") # Manda el menú de arriba a lo invisible
  })

  # --- INTERFAZ ---

  output$separador <- renderUI({ if(herramienta_cargada()) hr() })

  output$ui_menu_modulo <- renderUI({
    req(herramienta_cargada())
    navset_pill_list(
      id = "menu_modulo",
      well = FALSE,
      nav_panel("3. Configuración", value = "mod_config"),
      nav_panel("4. Resultados", value = "mod_res"),
      nav_panel("", value = "limpiar") # Pestaña invisible
    )
  })

  output$main_compartido <- renderUI({
    if (nav_state$origen == "fijo") {
      if (nav_state$pestaña == "fijo_datos") {
        card("Cuerpo: Sección de Datos")
      } else {
        card(actionButton("btn_cargar", "CARGAR ANOVA"))
      }
    } else {
      if (nav_state$pestaña == "mod_config") {
        card("Cuerpo: Configuración Módulo")
      } else {
        card("Cuerpo: Resultados Módulo")
      }
    }
  })

  observeEvent(input$btn_cargar, {
    herramienta_cargada(TRUE)
    nav_state$origen <- "modulo"
    nav_state$pestaña <- "mod_config"
    # Al cargar, el de arriba debe limpiarse
    nav_select("menu_fijo", "limpiar")
  })
}

shinyApp(ui, server)
