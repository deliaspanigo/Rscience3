#' @export
module_import_03_show_ui <- function(id) {
  ns <- shiny::NS(id)
  ""
  
}

#' @export
module_import_03_show_server <- function(id, list_sui_settings) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server
      ns <- session$ns
      
      # Internal fn ------------------------------------------------------------
      # Función para mostrar pop-up de error
      show_error_popup <- function(error_msg) {
        showModal(
          modalDialog(
            title = "Error de validación",
            HTML(error_msg),
            easyClose = TRUE,
            footer = modalButton("Cerrar"),
            size = "m",
            style = "color: #721c24; background-color: #f8d7da; border-color: #f5c6cb;"
          )
        )
        
        # También guardar el mensaje para mostrar en la UI principal
        error_message(error_msg)
      }
      ### ----------------------------------------------------------------------
      # _sif_: Selected Input File
      str_xlsx_readxl   <- "readxl::read_excel(path = '_sif_', sheet = 1)"
      str_xlsx_openxlsx <- "openxlsx::read.xlsx(xlsxFile = '_sif_', sheet = 1)"
      str_csv <- "read.csv(file = '_sif_', header = '_header_', 
                          sep = '_sep_', quote = '_quote_', dec = '_dec_')"
      
      list_str_import <- list(
        "source_xlsx" = str_xlsx_openxlsx,
        "source_csv" =  str_csv,
        "source_Rscience" = "Rscience.import::_sif_", 
        "source_Rdata" = "get('_sif_')"
      )
      
      # Validación de configuraciones
      check_ok <- reactive({
        req(list_sui_settings())
        TRUE
      })
      
      # Inicializar valores reactivos
              data_source <- reactiveVal(NULL)
      selected_input_file <- reactiveVal(NULL)
       temporal_file_path <- reactiveVal(NULL)
       original_file_name <- reactiveVal(NULL)
      str_import_selected <- reactiveVal(NULL)
      str_import_external <- reactiveVal(NULL)
      str_import_internal <- reactiveVal(NULL)
               info_extra <- reactiveVal(NULL)
                 my_dataset <- reactiveVal(NULL)
            error_message <- reactiveVal(NULL) # Para almacenar mensajes de error
      #button_clicked <- reactiveVal(FALSE)

      
      # Observar cambios en la fuente de datos
      observeEvent(list_sui_settings(), {
        req(check_ok(), list_sui_settings())
        

        # Actualizar el valor reactivo de data_source
        data_source(list_sui_settings()$"data_source")
        selected_input_file(list_sui_settings()$"selected_input_file")
        
        req(selected_input_file())
        
        # Lógica según la fuente de datos
        if(data_source() == "source_Rdata") {
          tryCatch({
            temporal_file_path("No details")
            original_file_name(selected_input_file())
            str_import_selected(list_str_import[[data_source()]])
            
            str_import_external(sub(pattern = "_sif_", 
                                       replacement = selected_input_file(), 
                                       x = str_import_selected()))
            
            str_import_internal(str_import_external())
            info_extra("No details")
            my_dataset(eval(parse(text = str_import_internal())))
            error_message("No details")
            
          }, error = function(e) {
            error_msg <- paste("Error al cargar el archivo RData:", e$message)
            error_message(error_msg)
            show_error_popup(error_msg)
          })
        }
        
        
        if(data_source() == "source_Rscience") {
          tryCatch({
            datasets_info <- data(package = "Rscience.import")
            dataset_names_Rscience <- datasets_info$results[, "Item"]
            check_selected_data_Rscience <- selected_input_file() %in% dataset_names_Rscience
            
          
             if(check_selected_data_Rscience) {
                   temporal_file_path("No details")
                   original_file_name(selected_input_file())
                   str_import_selected(list_str_import[[data_source()]])
                   
                   str_import_external(sub(pattern = "_sif_", 
                                           replacement = selected_input_file(), 
                                           x = str_import_selected()))
                   
                   str_import_internal(str_import_external())
                   print(str_import_internal())
                   info_extra("No details")
                   my_dataset(eval(parse(text = str_import_internal())))
                   error_message("No details")
              } else {
                error_msg <- paste("El dataset", selected_input_file(), "no existe en data_list_Rscience.")
                show_error_popup(error_msg)
              }
        
          }, error = function(e) {
            error_msg <- paste("Error al cargar el dataset Rscience:", e$message)
            error_message(error_msg)
            show_error_popup(error_msg)
          })
        }


        if(data_source() == "source_xlsx") {
          # Lógica para archivos Excel - SIN las validaciones que ahora están en el otro módulo
          tryCatch({
            temporal_file_path(list_sui_settings()$list_extra$"temporal_file_path")
            original_file_name(list_sui_settings()$list_extra$"original_file_name")
            str_import_selected(list_str_import[[data_source()]])
            
            str_import_external(sub(pattern = "_sif_", 
                                    replacement = original_file_name(), 
                                    x = str_import_selected()))
            
            str_import_internal(sub(pattern = "_sif_", 
                                    replacement = temporal_file_path(), 
                                    x = str_import_selected()))
            
            info_extra(list_sui_settings()$list_extra$"xlsx_file_details")
            
            error_message("No details")
            
            # Cargar los datos del archivo Excel con mejor manejo de errores
            withCallingHandlers({
              my_dataset(eval(parse(text = str_import_internal())))

              # Notificación de éxito
              showNotification(
                paste("Archivo Excel importado correctamente:", original_file_name()),
                type = "message",
                duration = 5
              )
            }, warning = function(w) {
              showNotification(
                paste("Advertencia al importar Excel:", w$message),
                type = "warning",
                duration = 10
              )
              invokeRestart("muffleWarning")
            })

          }, error = function(e) {
            error_msg <- paste("Error al cargar el archivo Excel:", e$message)
            error_message(error_msg)
            show_error_popup(error_msg)
          })
        }
        
        
      })
      
     
      
      # Construir la lista de salida
      output_list <- reactive({
        # No requerir my_dataset() aquí para permitir devolver aunque haya error
        
        list(
          "data_source" = data_source(),
          "selected_input_file" = selected_input_file(),
          "temporal_file_path" = temporal_file_path(),
          "original_file_name" = original_file_name(),
          "str_import_selected" = str_import_selected(),
          "str_import_external" = str_import_external(),
          "str_import_internal" = str_import_internal(),
          "info_extra" = info_extra(),
          "my_dataset" = my_dataset(),
          "error_message" = error_message()
        )
      })
      
      # Devolver la lista de salida
      return(output_list)
    }
  )
}
