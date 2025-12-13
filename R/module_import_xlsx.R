#' @export
module_import_xlsx_ui  <- function(id){
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("iu_base_selector")),
    uiOutput(ns("ui_action_button"))  # Botón de acción siempre visible
  )
}

#' @export
module_import_xlsx_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      # ReactiveVal para almacenar datos confirmados
      confirmed_data <- reactiveVal(NULL)
      
      # ReactiveVal para rastrear el estado del botón
      # Añadimos "error" como nuevo estado posible
      button_state <- reactiveVal("initial")  # initial, confirmed, modified, error
      
      # ReactiveVal para almacenar mensajes de error
      error_message <- reactiveVal(NULL)
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_xlsx"
      })
      
      output$iu_base_selector <- renderUI({
        req(check_ok())
        div(
          fileInput(ns("selected_input_file"), "Choose xlsx files (First sheet only)",
                    accept = c(
                      ".xlsx")
          )
        )
      })
      
      # UI para el botón de acción - MODIFICADO para incluir estado de error
      output$ui_action_button <- renderUI({
        req(check_ok())  # Solo requiere que check_ok sea TRUE, no requiere archivo
        
        # Determinar la clase y estado del botón
        btn_class <- switch(button_state(),
                            "initial" = "btn-primary",    # Azul inicial
                            "confirmed" = "btn-success",  # Verde después de confirmar
                            "modified" = "btn-primary",   # Vuelve a azul si se modifica
                            "error" = "btn-danger")       # Rojo en caso de error
        
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
              "Select an xlsx file."
            )
          },
          # Mostrar mensaje de confirmación solo si el estado es confirmed
          if (button_state() == "confirmed") {
            div(
              style = "margin-top: 10px; color: green;",
              icon("check-circle"), 
              "Confirmed - xlsx file selected!"
            )
          },
          
          # Mostrar mensaje de error si hay uno
          if (!is.null(error_message())) {
            div(
              style = "margin-top: 10px; color: #d32f2f; font-weight: bold;",
              icon("exclamation-triangle"), 
              HTML(error_message())  # Utilizamos HTML() para interpretar las etiquetas <br>
            )
          }
          
        )
      })
      
      list_extra <- reactive({
        req(input$selected_input_file)
        new_list <- list(
          "selected_input_file" = input$selected_input_file,
          "temporal_file_path"  = input$selected_input_file$datapath,
          "original_file_name"  = input$selected_input_file$name,
          "xlsx_file_details"   = analyze_excel(input$selected_input_file$datapath)
        )
      })
      
      # Función para validar el archivo Excel
      validate_excel_file <- function() {
        req(input$selected_input_file, list_extra())
        
        # Obtener detalles del Excel
        excel_details <- list_extra()$xlsx_file_details
        
        # Lista para almacenar mensajes de error
        validation_errors <- c()
        
        # Verificar tamaño del archivo (en MB)
        if (excel_details$file_size_mb > 50) {
          validation_errors <- c(validation_errors, 
                                 paste("The file size is", excel_details$file_size_mb, 
                                       "MB, exceding the limit of 50 MB"))
        }
        
        # Verificar número de hojas
        if (excel_details$sheet_count > 1) {
          validation_errors <- c(validation_errors, 
                                 paste("The file contains", excel_details$sheet_count, 
                                       "sheets. Only xlsx files with 1 sheet are allowed."))
        }
        
        # Verificar número de columnas (primera hoja)
        if (excel_details$vector_cols[1] > 200) {
          validation_errors <- c(validation_errors, 
                                 paste("The sheet from xlsx file contains", excel_details$vector_cols[1], 
                                       "columns, exceding the limit of 200 columns."))
        }
        
        # Verificar número de filas (primera hoja)
        if (excel_details$vector_rows[1] > 5000) {
          validation_errors <- c(validation_errors, 
                                 paste("The sheet from xlsx file contains", excel_details$vector_rows[1], 
                                       "rows, exceding the limit of 5000 rows."))
        }
        
        # Si hay errores de validación, retornar la lista de errores
        if (length(validation_errors) > 0) {
          return(validation_errors)
        }
        
        # Si no hay errores, retornar NULL
        return(NULL)
      }
      
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
      
      # Observar cambios en la selección del archivo
      observeEvent(input$selected_input_file, {
        # Resetear mensaje de error cuando cambia el archivo
        error_message(NULL)
        
        # Si ya hay datos confirmados, verificamos si la selección actual es diferente
        if (button_state() == "confirmed" && !is.null(input$selected_input_file)) {
          current_file <- input$selected_input_file$name
          confirmed_file <- confirmed_data()$selected_input_file$name
          
          # Si el archivo ha cambiado, cambiar el estado a "modified"
          if (!identical(current_file, confirmed_file)) {
            button_state("modified")
          }
        } else if (button_state() == "error") {
          # Si estaba en estado de error, volver a initial
          button_state("initial")
        }
      }, ignoreInit = TRUE)
      
      # Observar el botón de confirmación - MODIFICADO para validar al hacer clic
      observeEvent(input$confirm_selection, {
        req(temp_data(), input$selected_input_file)
        
        # Validar el archivo antes de confirmar
        validation_errors <- validate_excel_file()
        
        if (!is.null(validation_errors)) {
          # Si hay errores, cambiar el estado del botón a "error" (rojo)
          button_state("error")
          
          # Crear y mostrar mensaje de error
          error_msg <- paste("The Excel file could not be imported due to the following limitations:", 
                             paste(validation_errors, collapse = "<br>"), 
                             sep = "<br>")
          
          error_message(error_msg)
          
          # Mostrar pop-up con el error
          showModal(
            modalDialog(
              title = "Error on validation",
              HTML(error_msg),
              easyClose = TRUE,
              footer = modalButton("Cerrar"),
              size = "m",
              style = "color: #721c24; background-color: #f8d7da; border-color: #f5c6cb;"
            )
          )
          
          # Resetear el FileInput para forzar la selección de otro archivo
          session$sendInputMessage("selected_input_file", NULL)
          
          # No confirmar los datos
          return()
        }
        
        # Si no hay errores, guardar los datos en el reactiveVal
        confirmed_data(temp_data())
        
        # Cambiar el estado del botón a "confirmed" (verde)
        button_state("confirmed")
        
        # Resetear mensaje de error
        error_message(NULL)
        
        # Mostrar un mensaje de éxito
        showNotification(
          "Confirmed - xlsx file selected!",
          type = "message"
        )
      })
      
      # Observar cambios en check_ok para resetear datos confirmados
      observeEvent(check_ok(), {
        if (!check_ok()) {
          confirmed_data(NULL)
          button_state("initial")
          error_message(NULL)
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
