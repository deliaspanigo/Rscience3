
#' @export
module_import_02_settings_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
   uiOutput(ns("Selected_UI_DS"))
  )
  
}



#' @export
module_import_02_settings_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      # NOTA: esto se puede mejorar mas tal vez.
      # Pero de esta foram aun se puede entender lo que se esta haciendo
      # de una buena manera.
      
      # UI MODULES
      ui_modules <- list(
        source_xlsx     = module_import_xlsx_ui,
        source_csv      = module_import_csv_ui,
        source_Rscience = module_import_Rscience_ui,
        source_Rdata    = module_import_Rdata_ui
      )
      
      
      # Selected UI for selected Server - Data Source (DS)
      output$Selected_UI_DS <- renderUI({
            req(sui_data_source())
            
            ui_fun <- ui_modules[[sui_data_source()]]
            
            # Validar que la fuente exista
            validate(
              need(!is.null(ui_fun), paste("Error - Selected_UI_DS - Unknown data source:", sui_data_source()))
            )
            
            # Usar la función encontrada, con el id adecuado
            sub_id <- sui_data_source()# gsub("source_", "", )  # elimina el "source_"
            ui_fun(id = ns(sub_id))
      })
      
      # 1.2 - Settings para cada fuente de datos.
      the_01_xlsx_settings     <- module_import_xlsx_server(id =   "source_xlsx", sui_data_source)
      the_02_csv_settings      <- module_import_csv_server(id =    "source_csv", sui_data_source)
      the_03_Rscience_settings <- module_import_Rscience_server(id = "source_Rscience", sui_data_source)
      the_04_Rdata_settings    <- module_import_Rdata_server(id =  "source_Rdata", sui_data_source)
      
      # https://gallery.shinyapps.io/assistant/?_gl=1*slchuy*_ga*MTQ4NTM0MTYxMC4xNzQ0ODMzMDEy*_ga_2C0WZ1JHG0*czE3NDQ4OTg3NDEkbzIkZzEkdDE3NDQ4OTkwNTkkajAkbDAkaDA.#
      control_only_one_alive <- reactive({
        
        vector_status <- c(!is.null(the_01_xlsx_settings()),
                           !is.null(the_02_csv_settings()),
                           !is.null(the_03_Rscience_settings()),
                           !is.null(the_04_Rdata_settings()))
        
        check_only_one <- sum(vector_status) <= 1
        
        return(check_only_one)
        
      })  
      
      
      # Lista con el settings para realizar la importacion
      list_sui_settings <- reactive({
        req(sui_data_source(), control_only_one_alive())
        
        
        
        el_elegido <- switch(sui_data_source(),
                             "source_xlsx"   = the_01_xlsx_settings(),
                             "source_csv"    = the_02_csv_settings(),
                             "source_Rscience" = the_03_Rscience_settings(),
                             "source_Rdata"  = the_04_Rdata_settings(),
                             # Valor por defecto o manejo de error
                             stop("Fuente de datos no reconocida:", sui_data_source())
        )
        
        
        return(el_elegido)
        # NULL
      })
      
      
      return(list_sui_settings)
      
    }
  )
}



#######################################################################################
