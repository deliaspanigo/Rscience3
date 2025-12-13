#' @export
module_import_csv_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("iu_base_selector"))
    
  )
}

#' @export
module_import_csv_server <- function(id, sui_data_source){
  
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
        sui_data_source() == "source_csv"
      })
      
      # Añadir un reactiveVal para forzar la actualización
      ui_version <- reactiveVal(0)
      
      # Observar cambios en check_ok y actualizar ui_version
      observeEvent(check_ok(), {
        # Incrementar la versión cada vez que check_ok cambia
        ui_version(ui_version() + 1)
        
        # Si check_ok es FALSE, limpiar los inputs y resetear el estado del botón
        if (!check_ok()) {
          # Si estás usando fileInput, no puedes resetearlo directamente
          # Pero puedes limpiar otros inputs
          if (!is.null(input$header)) updateRadioButtons(session, "header", selected = "1")
          if (!is.null(input$sep)) updateRadioButtons(session, "sep", selected = ";")
          if (!is.null(input$dec)) updateRadioButtons(session, "dec", selected = ".")
          if (!is.null(input$quote)) updateRadioButtons(session, "quote", selected = "")
          
          # Resetear datos confirmados y estado del botón
          confirmed_data(NULL)
          button_state("initial")
        }
      })
      
      output$iu_base_selector <- renderUI({
        version <- ui_version()
        
        # Solo renderizar si check_ok es TRUE
        req(check_ok())
        
        div(
          fileInput(ns("selected_input_file"), "Select your CSV file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          uiOutput(ns("ui_action_button")),  # Botón de acción siempre visible
          tags$hr(),
          radioButtons(ns("header"), "Header", choices = c(Yes = 1, No = 0)), br(),
          radioButtons(ns("sep"), "Separator",
                       choices = c(PuntoYComa = ";",
                                   Coma = ",",
                                   Tab = "\t"),
                       selected = ";"), br(),
          radioButtons(ns("dec"), "Decimal",
                       choices = c(Dot = ".",
                                   Comma = ","),
                       selected = "."), br(),
          
          radioButtons(ns("quote"), "Quotes",
                       choices = c(Ninguna = "",
                                   Doble = '"',
                                   Simple = "'"),
                       selected = ""), br()
        )
      })
      
      # UI para el botón de acción - MODIFICADO para mostrarse siempre
      output$ui_action_button <- renderUI({
        req(check_ok())  # Solo requiere que check_ok sea TRUE, no requiere archivo
        
        # Determinar la clase del botón según su estado
        btn_class <- switch(button_state(),
                            "initial" = "btn-primary",    # Azul inicial
                            "confirmed" = "btn-success",  # Verde después de confirmar
                            "modified" = "btn-primary")   # Vuelve a azul si se modifica
        
        # Determinar si el botón debe estar deshabilitado
        is_disabled <- is.null(input$selected_input_file)
        
        div(
          style = "margin-top: 15px;",
          actionButton(
            inputId = ns("confirm_selection"),
            label = "Checking selection",
            icon = icon("check"),
            class = btn_class,
            width = "100%",
            disabled = is_disabled  # Deshabilitado si no hay archivo
          ),
          # Mostrar mensaje explicativo si está deshabilitado
          if (is_disabled) {
            div(
              style = "margin-top: 10px; color: #e57373; font-style: italic; font-size: 16px; font-weight: bold",
              "Selecciona un archivo CSV"
            )
          },
          # Mostrar mensaje de confirmación solo si el estado es confirmed
          if (button_state() == "confirmed") {
            div(
              style = "margin-top: 10px; color: green;",
              icon("check-circle"), 
              "Confirmed - CSV file selected!"
            )
          }
        )
      })
      
      list_extra <- reactive({
        output_list <- list(
          "header" = input$"header", 
          "sep" = input$"sep",
          "dec" = input$"dec",
          "quote" = input$"quote"
        )
        
        return(output_list)
      })
      
      # Datos temporales (no confirmados)
      temp_data <- reactive({
        req(sui_data_source(), input$selected_input_file, list_extra())
        
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
      
      # Observar cambios en cualquiera de los inputs para detectar modificaciones después de confirmar
      observe({
        # Lista de todos los inputs que queremos monitorear
        input$selected_input_file
        input$header
        input$sep
        input$dec
        input$quote
        
        # Si el estado es "confirmed", cambiar a "modified"
        if (button_state() == "confirmed" && !is.null(input$selected_input_file)) {
          # Verificar si cualquier valor ha cambiado comparando con los datos confirmados
          if (!is.null(confirmed_data())) {
            # Comparar al menos el nombre del archivo para simplificar
            current_file <- input$selected_input_file$name
            confirmed_file <- confirmed_data()$selected_input_file$name
            
            if (!identical(current_file, confirmed_file) || 
                !identical(input$header, confirmed_data()$list_extra$header) ||
                !identical(input$sep, confirmed_data()$list_extra$sep) ||
                !identical(input$dec, confirmed_data()$list_extra$dec) ||
                !identical(input$quote, confirmed_data()$list_extra$quote)) {
              button_state("modified")
            }
          }
        }
      })
      
      # Observar el botón de confirmación
      observeEvent(input$confirm_selection, {
        req(temp_data(), input$selected_input_file)
        
        # Guardar los datos en el reactiveVal
        confirmed_data(temp_data())
        
        # Cambiar el estado del botón a "confirmed" (verde)
        button_state("confirmed")
        
        # Mostrar un mensaje de éxito
        showNotification(
          "Confirmed - CSV file selected!",
          type = "message"
        )
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



