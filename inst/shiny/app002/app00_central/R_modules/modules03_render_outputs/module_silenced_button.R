# module_silenced_button.R
library(shiny)
library(shinyjs) # Necesario para la función shinyjs::runjs

# UI Module for the Run Button
module_silenced_button_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dynamic_run_button"))
  )
}

# Server Module for the Run Button
# Ya no necesita 'app_state' como argumento de entrada, 
# ya que crea su propio estado interno.
module_silenced_button_server <- function(id) { 
  moduleServer(id, function(input, output, session) {
    
    # 1. Definición del estado (ENCAPSULADO DENTRO DEL MÓDULO)
    button_state <- reactiveValues(
      is_running = FALSE,
      button_class = "btn-primary",
      button_label = "Run",
      button_icon = icon("play"),
      button_disabled = FALSE,
      module_active = FALSE # Esta es la señal de salida para el pipeline
    )
    
    # Renderizado del botón dinámico basado en el estado interno
    output$dynamic_run_button <- renderUI({
      actionButton(
        inputId = session$ns("run"),
        label = tagList(button_state$button_icon, button_state$button_label),
        class = button_state$button_class,
        width = "100%",
        disabled = button_state$button_disabled
      )
    })
    
    # Observador del clic del botón
    observeEvent(input$"run", {
      if (!button_state$is_running) {
        
        # --- ACTUALIZAR ESTADO INTERNO ---
        button_state$is_running <- TRUE
        button_state$button_class <- "btn-success"
        button_state$button_label <- "Running..."
        button_state$button_icon <- icon("spinner", class = "fa-spin")
        button_state$button_disabled <- TRUE
        button_state$module_active <- TRUE # ACTIVA la señal de salida
        
        # Efecto shinyjs
        button_id_js <- paste0("#", session$ns("run"))
        shinyjs::runjs(paste0("$('", button_id_js, "').addClass('pulse');"))
      }
    })
    
    # ----------------------------------------------------
    # EL MÓDULO RETORNA el objeto 'button_state' (reactiveValues)
    # ----------------------------------------------------
    return(button_state)
    
  })
}