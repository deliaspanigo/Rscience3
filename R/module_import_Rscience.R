#' @export
module_import_Rscience_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("iu_base_selector")),
    uiOutput(ns("ui_action_button"))  # Botón de acción siempre visible
  )
}

#' @export
module_import_Rscience_server <- function(id, sui_data_source){
  
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
        sui_data_source() == "source_Rscience"
      })
      
      ################################################
      
      all_names_database <- reactive({
        req(check_ok())
        
        # list.files(input_folder_master)
        #datasets_info <- data(package = "RMedic")
        #dataset_names <- datasets_info$results[, "Item"]
        datasets_info <- data(package = "Rscience.import")
        dataset_names <- datasets_info$results[, "Item"]
        
      })
      
      select_opt_database <- reactive({
        req(check_ok())
        req(all_names_database())
        
        # # # Nombre de las bases de datos
        vector_obj <- all_names_database()
        
        # # # Numero de orden, desed 01 hasta la ultima
        vector_numbers <- 1:length(vector_obj)
        amount_digits <- max(nchar(vector_numbers))
        if(amount_digits < 2) amount_digits <- 2
        
        vector_formatted_numbers <- stringr::str_pad(string = vector_numbers,
                                                     width = amount_digits,
                                                     pad = "0")
        
        # Vector con la visualizacion que tiene el usuario
        vector_visual <- paste0(vector_formatted_numbers, " - ", vector_obj)
        
        # Asignamos la visual a al vector
        names(vector_obj) <- vector_visual
        
        
        # Salida!
        vector_obj
        
      })
      
      output$iu_base_selector <- renderUI({
        req(check_ok())
        
        vector_visual <- c("Selecet one..." = "", select_opt_database())
        
        
        shiny::selectInput(
          inputId = ns("selected_input_file"),
          label = "Rscience - Dataset examples",
          choices = vector_visual
        )
      })
      
      # UI para el botón de acción - MODIFICADO para mostrarse siempre
      output$ui_action_button <- renderUI({
        req(check_ok())  # Solo requiere que check_ok sea TRUE
        
        # Determinar la clase del botón según su estado
        btn_class <- switch(button_state(),
                            "initial"   = "btn-primary",    # Azul inicial
                            "confirmed" = "btn-success",  # Verde después de confirmar
                            "modified"  = "btn-primary")   # Vuelve a azul si se modifica
        
        # Determinar si el botón debe estar deshabilitado
        is_disabled <- is.null(input$selected_input_file) || input$selected_input_file == ""
        
        div(
          style = "margin-top: 15px;",
          actionButton(
            inputId = ns("confirm_selection"),
            label = "Checking selection",
            icon = icon("check"),
            class = btn_class,
            width = "100%",
            disabled = is_disabled  # Deshabilitado si no hay selección o está vacía
          ),
          # Mostrar mensaje explicativo si está deshabilitado
          if (is_disabled) {
            div(
              style = "margin-top: 10px; color: #e57373; font-style: italic; font-size: 16px; font-weight: bold",
              "Select a dataset example from Rscience."
            )
          },
          # Mostrar mensaje de confirmación solo si el estado es confirmed
          if (button_state() == "confirmed") {
            div(
              style = "margin-top: 10px; color: green;",
              icon("check-circle"), 
              "Confirmed - Rscience dataset example selected!"
            )
          }
        )
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
          "Confirmed Rscience data example!",
          type = "message"
        )
      })
      
      list_extra <- reactive({
        "No details"
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

