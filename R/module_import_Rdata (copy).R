#' @export
module_import_Rdata_ui2 <- function(id) {
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("iu_base_selector")),
    uiOutput(ns("ui_action_button"))
  )
}

#' @export
module_import_Rdata_server2 <- function(id, sui_data_source) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      # ReactiveVal para almacenar datos confirmados
      confirmed_data <- reactiveVal(NULL)
      
      # ReactiveVal para rastrear el estado del botón
      button_state <- reactiveVal("initial")  # initial, confirmed, modified
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_Rdata"
      })
      
      # UI para el selector de base
      output$iu_base_selector <- renderUI({
        req(check_ok(), sui_data_source())
        
        vector_opt <- c("01 - mtcars"     = "mtcars",
                        "02 - iris"       = "iris",
                        "03 - airquality" = "airquality")
        
        vector_opt <- c("Seleccione una..." = "", vector_opt)
        
        shiny::selectInput(
          inputId = ns("selected_input_file"),
          label = "Bases de R",
          choices = vector_opt
        )
      })
      
      
      
      # UI para el botón de acción - MODIFICADO para mostrarse siempre
      output$ui_action_button <- renderUI({
        req(check_ok())  # Solo requiere que check_ok sea TRUE
        
        # Determinar la clase del botón según su estado
        btn_class <- switch(button_state(),
                            "initial" = "btn-primary",    # Azul inicial
                            "confirmed" = "btn-success",  # Verde después de confirmar
                            "modified" = "btn-primary")   # Vuelve a azul si se modifica
        
        # Determinar si el botón debe estar deshabilitado
        is_disabled <- is.null(input$selected_input_file) || input$selected_input_file == ""
        
        div(
          style = "margin-top: 15px;",
          actionButton(
            inputId = ns("confirm_selection"),
            label = "Confirmar selección",
            icon = icon("check"),
            class = btn_class,
            width = "100%",
            disabled = is_disabled  # Deshabilitado si no hay selección o está vacía
          ),
          # Mostrar mensaje explicativo si está deshabilitado
          if (is_disabled) {
            div(
              style = "margin-top: 10px; color: #e57373; font-style: italic; font-size: 16px; font-weight: bold",
              "Selecciona una base de datos de R"
            )
          },
          # Mostrar mensaje de confirmación solo si el estado es confirmed
          if (button_state() == "confirmed") {
            div(
              style = "margin-top: 10px; color: green;",
              icon("check-circle"), 
              "Selección confirmada"
            )
          }
        )
      })
      
      list_extra <- reactive({
        "No details"
      })
      
      # Datos temporales (no confirmados)
      temp_data <- reactive({
        req(check_ok(), sui_data_source(), list_extra())
        
        # Validamos si existe primero
        selected_file <- NULL
        if (!is.null(input$selected_input_file)) {
          selected_file <- input$selected_input_file
        }
        
        # Creación de la lista
        output_list <- list(
          "data_source" = sui_data_source(),
          "selected_input_file" = selected_file,
          "list_extra" = list_extra()
        )
        
        return(output_list)
      })
      
      # Observar cambios en la selección para detectar cambios después de confirmación
      observeEvent(input$selected_input_file, {
        # Si ya hay datos confirmados, verificamos si la selección actual es diferente
        if (button_state() == "confirmed") {
          current_selection <- input$selected_input_file
          confirmed_selection <- confirmed_data()$selected_input_file
          
          # Si la selección ha cambiado, cambiar el estado a "modified"
          if (!identical(current_selection, confirmed_selection)) {
            button_state("modified")
          }
        }
      }, ignoreInit = TRUE)
      
      # Observar el botón de confirmación
      observeEvent(input$confirm_selection, {
        req(temp_data(), input$selected_input_file, input$selected_input_file != "")
        
        # Guardar los datos en el reactiveVal
        confirmed_data(temp_data())
        
        # Cambiar el estado del botón a "confirmed" (verde)
        button_state("confirmed")
        
        # Mostrar un mensaje de éxito
        showNotification(
          "Selección confirmada correctamente",
          type = "message"
        )
      })
      
      # Observar cambios en check_ok para resetear datos confirmados
      observeEvent(check_ok(), {
        if (!check_ok()) {
          confirmed_data(NULL)
          button_state("initial")
        }
      })
      
      # Devolver solo los datos confirmados
      return(reactive({
        if (!isTruthy(check_ok())) {
          return(NULL)
        }
        # Devolver los datos confirmados, no los temporales
        confirmed_data()
      }))
    }
  )
}