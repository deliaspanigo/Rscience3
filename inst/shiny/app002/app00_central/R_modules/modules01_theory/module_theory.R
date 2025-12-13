#' UI for Theory Module
#' @param id Module ID
module_theory_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # JavaScript para manejar botones
    tags$script(HTML("
      // Cambiar clase de botones
      Shiny.addCustomMessageHandler('change_button_color', function(message) {
        var button = document.getElementById(message.button_id);
        if (button) {
          button.classList.remove(message.from_class);
          button.classList.add(message.to_class);
        }
      });
      
      // Abrir nueva ventana
      Shiny.addCustomMessageHandler('open_new_window', function(message) {
        window.open(message.url, '_blank');
      });
    ")),
    
    # UI principal
    uiOutput(ns("main_theory_general"))
  )
}


module_theory_server <- function(id) {
  
  # CARGA DE LIBRERÍAS
  library(digest) 
  library(later) 
  library(shinyjs)
  
  # Pasar información a la función interna
  moduleServer(id, function(input, output, session) {
    
    # 1. ns - Always first
    ns <- session$ns
    
    # 2. Librerías
    library("here")
    
    # 3. Basics
    str_central_path <- here::here()
    
    # 4. Loading my fns
    str_fn_app_general <- here::here("fn_app_general.R")
    str_fn_app_html <- here::here("fn_app_html.R")
    source(str_fn_app_general)
    source(str_fn_app_html)
    
    # 5. Material to show on ClassRoom
    # 5.1. File names and topics
    theory_list <- list(
      list(id = "theory_general"      , label = "Theory"      , html_file_name = "theory_01_anova_intro.html"),
      list(id = "theory_tukey"        , label = "Tukey"       , html_file_name = "theory_02_tukey.html"),
      list(id = "theory_decision_making", label = "Decision Making", html_file_name = "theory_03_decision_making.html"),
      list(id = "theory_asa"          , label = "ASA"         , html_file_name = "theory_04_ASA.html")
    )
    
    # 5.2. Folder path from resources
    str_source_folder_full_path <- here::here("..", "R_and_quarto_files_01_ready", "theory_quarto_files")
    
    # 5.3. Dynamically create render functions for each theory item
    lapply(theory_list, function(theory_item) {
      output_id <- theory_item$id  # Sin ns() aquí, se aplica después
      
      output[[output_id]] <- renderText({
        file_name <- theory_item$"html_file_name"
        fn_app_show_my_html(str_source_folder_full_path, file_name)
      })
    })
    
    # 6. All Dynamic UI generation for theory tabs inside one renderUI
    output$"main_theory_general" <- renderUI({
      
      ## 6.1 Style for navigation panels
      str_style_NAV_PANEL <- "flex-grow: 1; overflow-y: auto; height: 72vh; width: 100%; overflow: hidden;"
      
      ## 6.2 Create nav panels dynamically
      nav_panels <- lapply(theory_list, function(item) {
        
        # Create nav panel
        bslib::nav_panel(
          title = item$label,
          fluidRow(
            column(2, h4(item$label)),
            column(9),
            column(1,
                   tags$span(
                     style = "white-space: nowrap;",
                     # Botón binoculars
                     actionButton(inputId = ns(paste0("open_", item$id)),
                                  label = NULL,
                                  icon = icon("binoculars", class = "fa-2x"),
                                  class = "btn-warning btn-sm",
                                  style = "display: inline-block; margin-right: 5px;"),
                     # Botón download
                     downloadButton(outputId = ns(paste0("download_", item$id)),
                                    label = NULL,
                                    icon = icon("download", class = "fa-2x"),
                                    class = "btn-warning btn-sm")
                   )
            )
          ),
          tags$div(
            style = str_style_NAV_PANEL,
            htmlOutput(ns(item$id))  # Output con namespace
          )
        )
      })
      
      ## 6.3 Create the navset with dynamic panels
      do.call(bslib::navset_card_tab, c(
        list(
          full_screen = TRUE,
          title = tags$div(
            style = "min-height: 10px; padding: 0px;",
            tags$h4("ClassRoom")
          ),
          id = ns("theory_navset") 
        ),
        nav_panels
      ))
    })
    
    # 7. Observer for binoculars button clicks - SIN RESET (se queda verde)
    observe({
      # Para cada item en theory_list
      for (item in theory_list) {
        
        # Usar local() para capturar el item actual
        local({
          current_item <- item
          button_name <- paste0("open_", current_item$id)
          
          # Observer individual para cada botón
          observeEvent(input[[button_name]], {
            
            # Get HTML file path
            html_file_name <- current_item$"html_file_name"
            full_html_path <- file.path(str_source_folder_full_path, html_file_name)
            
            # 1. Change button to green (success) - SE QUEDA VERDE
            session$sendCustomMessage(
              type = "change_button_color",
              message = list(
                button_id = ns(button_name),  # ID con namespace
                from_class = "btn-warning",
                to_class = "btn-success"
              )
            )
            
            # 2. Abrir archivo si existe
            if (file.exists(full_html_path)) {
              html_dir <- dirname(full_html_path)
              html_filename <- basename(full_html_path)
              
              # Usar un hash de la carpeta como ID del recurso
              resource_id <- digest::digest(html_dir, algo = "md5")
              
              # Registrar la carpeta para que sea accesible
              shiny::addResourcePath(resource_id, html_dir) 
              
              # Construir la URL
              html_url <- paste0(resource_id, "/", html_filename)
              
              # 3. Abrir la URL en una nueva ventana
              session$sendCustomMessage(
                type = "open_new_window",
                message = list(url = html_url)
              )
            } else {
              # Mostrar error
              showNotification(
                paste("File not found:", html_file_name),
                type = "error",
                duration = 5
              )
            }
            
            # ❌ NO hay later::later() - SE QUEDA VERDE
            
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # 8. Download handlers and observers - SIN RESET (se queda verde)
    # 8. Download handlers and observers - SIN RESET (se queda verde)
    observe({
      # Para cada item en theory_list
      for (item in theory_list) {
        
        local({
          current_item <- item
          download_button_name <- paste0("download_", current_item$id)
          html_file_name <- current_item$"html_file_name"
          full_html_path <- file.path(str_source_folder_full_path, html_file_name)
          
          # 1. Download handler para cada archivo
          output[[download_button_name]] <- downloadHandler(
            filename = function() {
              html_file_name
            },
            content = function(file) {
              if (file.exists(full_html_path)) {
                # CAMBIO DE COLOR al descargar
                session$sendCustomMessage(
                  type = "change_button_color",
                  message = list(
                    button_id = ns(download_button_name),
                    from_class = "btn-warning",
                    to_class = "btn-success"
                  )
                )
                
                file.copy(full_html_path, file)
              } else {
                stop("File not found: ", html_file_name)
              }
            }
          )
          
          # 2. Observer para detectar clic en el botón de descarga
          # Necesitamos usar JavaScript porque downloadButton no genera eventos Shiny normales
          shinyjs::runjs(paste0("
            // Detectar clic en el botón de descarga
            $(document).on('click', '#", ns(download_button_name), "', function() {
              // Cambiar color inmediatamente al hacer clic
              Shiny.setInputValue('", ns(paste0("download_click_", current_item$id)), "', 
                Date.now(), {priority: 'event'});
            });
          "))
          
          # 3. Observer para el evento de clic detectado por JavaScript
          observeEvent(input[[paste0("download_click_", current_item$id)]], {
            # Cambiar color del botón a verde permanentemente
            session$sendCustomMessage(
              type = "change_button_color",
              message = list(
                button_id = ns(download_button_name),
                from_class = "btn-warning",
                to_class = "btn-success"
              )
            )
            
            # Mostrar notificación
            showNotification(
              paste("Downloading:", html_file_name),
              type = "message",
              duration = 2
            )
          }, ignoreInit = TRUE)
          
        })
      }
    })
    
    # 9. Observer opcional: Reset manual si necesitas (por ejemplo, con un botón "reset")
    observeEvent(input$reset_buttons, {
      # Resetear todos los botones a naranja
      for (item in theory_list) {
        # Botón binoculars
        session$sendCustomMessage(
          type = "change_button_color",
          message = list(
            button_id = ns(paste0("open_", item$id)),
            from_class = "btn-success",
            to_class = "btn-warning"
          )
        )
        
        # Botón download
        session$sendCustomMessage(
          type = "change_button_color",
          message = list(
            button_id = ns(paste0("download_", item$id)),
            from_class = "btn-success",
            to_class = "btn-warning"
          )
        )
      }
    })
    
  })
}

